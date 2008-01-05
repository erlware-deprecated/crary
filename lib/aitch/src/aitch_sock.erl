-module(aitch_sock).

%% public
-export([start_link/4]).
-export([read/2, read/3, write/2]).
-export([read_line/1, read_line/2]).
-export([read_req_line/1, read_req_line/2]).
-export([write_resp_line/2, write_resp_line/3]).
-export([done_reading/1, done_writing/1]).
-export([new_resp/2]).
-export([sockname/1, peername/1]).
-export([peer_name/1, peer_port/1, this_name/1, this_port/1]).
-export([close/1, close_reader/1]).

-include("aitch.hrl").

-define(EOL, <<"\r\n">>).

%% the state structures for the reader process and writer process.
%% 'ctrl' and 'resp' are reference which clients pass in and help make
%% sure that things get read/written in the right order during
%% pipe-lining
-record(r_state, {sock,   %% the socket's port()
                  ctrl,   %% the reference() given to the controller
                  resp,   %% the reference() given to the handler that
                          %% currently has permission to read
                  resp_q, %% queue of future resp refs()
                  w       %% pid() of writer
                 }).
-record(w_state, {sock, ctrl, resp, resp_q, r}).

%% this is the socket descriptor given to the controller and handlers.
%% its private, so in general clients shouldn't try to depend on what's
%% inside of it.
%%
%% it includes several lambda forms that use closure to hide the raw
%% socket from the client, but avoid having to have a seperate process
%% to handle them (for instance, peername() can't be handled by the
%% reader as sometimes peername() is needed while the reader is blocked
-record(sock, {r,        %% pid() of reader
               w,        %% pid() of writer
               resp,     %% reference() identifies when this sock can read/write
               sockname, %% fun() which returns socket's inet:sockname()
               peername, %% fun() which returns socket's inet:peername()
               close}).  %% fun() which returns socket's inet:close()

%% @type sock() = record()
%% @type r_state() = record()
%% @type w_state() = record()
%% @type proplist() = [Key::atom() | {Key::atom(), Value::term}]
%% @type mfa() = {Module::atom(), Function::atom(), Args::list()}
%% @type timeout() = Milliseconds | infinity
%%       Milliseconds = integer()
%% @type vsn() = {Major, Minor}
%%       Major = integer()
%%       Minor = integer()

%%-------------------------------------------------------------------
%% @doc Usually only called by aitch_port to spawn/accept/process.
%% @spec start_link(Port::pid(), ListenSock::port(),
%%                  Handler::mfa(), Opts::proplists()) -> pid()
%% @end
%%-------------------------------------------------------------------
start_link(PortPid, ListenSock, Handler, Opts) ->
    aitch_util:spawn_link(
      fun() -> accept(PortPid, ListenSock, Handler, Opts) end).

%%-------------------------------------------------------------------
%% @private
%% @doc accept() on the socket and process resulting connection.
%% @spec accept(Port::pid(), ListenSock::port(),
%%              Handler::mfa(), Opts::proplist()) -> none()
%% @end
%%-------------------------------------------------------------------
accept(PortPid, ListenSock, Handler, Opts) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            %% send message to the listener process to create a new acceptor
            aitch_port:accepted(PortPid, self()),
            start_ctrl_with_wrapped_sock(Sock, Handler, Opts);
        {error, R} ->
            exit({aitch_error, {accept_failed, R}})
    end.

%%-------------------------------------------------------------------
%% @private
%% @doc create processes for, link-together, and start the writer,
%% reader, and controller
%% @spec start_ctrl_with_wrapped_sock(Sock::port(), Handler::mfa(),
%%                                    Opts::proplist()) -> none()
%% @end
%%-------------------------------------------------------------------
start_ctrl_with_wrapped_sock(Sock, Handler, Opts) ->
    Ctrl = make_ref(),
    WPid = start_link_w(Sock, Ctrl, self()),
    aitch_ctrl:start_link(new_sock(self(), WPid, Ctrl, Sock), Handler, Opts),
    start_r(Sock, Ctrl, WPid).

new_sock(RPid, WPid, Ctrl, Sock) ->
    #sock{r = RPid,
          w = WPid,
          resp = Ctrl,
          sockname = fun() -> inet:sockname(Sock) end,
          peername = fun() -> inet:peername(Sock) end,
          close = fun() -> gen_tcp:close(Sock) end}.

%%-------------------------------------------------------------------
%% @private
%% @doc start the reader process by taking over this process
%% @spec start_r(Sock::port(), Ctrl::reference(), Writer::pid()) -> none()
%% @end
%%-------------------------------------------------------------------
start_r(Sock, Ctrl, W) ->
    process_flag(trap_exit, true),
    r_sock_loop(#r_state{sock = Sock,
                         ctrl = Ctrl,
                         w = W,
                         resp = Ctrl,
                         resp_q = queue:new()}).

%%-------------------------------------------------------------------
%% @private
%% @doc spawn/start the writer process
%% @spec start_link_w(Sock::port(), Ctrl::reference(), Reader::pid()) -> none()
%% @end
%%-------------------------------------------------------------------
start_link_w(Sock, Ctrl, R) ->
    aitch_util:spawn_link(
      fun () ->
              process_flag(trap_exit, true),
              w_sock_loop(#w_state{sock = Sock,
                                   ctrl = Ctrl,
                                   resp_q = queue:new(),
                                   r = R})
      end).

%%-------------------------------------------------------------------
%% @doc Close the socket, shutdown the reader and writer.
%% 
%% Only call this if you really want to close the connection, otherwise
%% this will terminate any futher pipe-lined requests.
%%
%% @spec close(sock() | aitch:aitch_req()) -> ok
%% @throws {aitch_sock, {error_closing_sock, R}}
%% @end
%%-------------------------------------------------------------------
close(#aitch_req{sock = S}) ->
    close(S);
close(#sock{r = R, w = W, resp = Ctrl, close = Close}) ->
    erlang:send(R, {close, Ctrl}),
    erlang:send(W, {close, Ctrl}),
    case Close() of
        ok -> ok;
        {error, R} -> exit({aitch_sock, {error_closing_sock, R}})
    end.


%%-------------------------------------------------------------------
%% @doc Close the read half of the TCP socket.
%% 
%% On hitting EOF when reading, we don't know if it was a close() or a
%% half duplex shutdown(), so close the reader, and add 'close' to
%% the writers resp_q to finish shutting down when the last pipe-lined
%% response has been written.
%%
%% @spec close_reader(Sock::sock() | aitch:aitch_req()) -> ok
%% @throws {aitch_sock, {error_closing_sock, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
close_reader(#aitch_req{sock = S}) ->
    close_reader(S);
close_reader(#sock{r = R, w = W, resp = Ctrl}) ->
    erlang:send(R, {close, Ctrl}),
    erlang:send(W, {add_resp, Ctrl, close}),
    ok.

%%-------------------------------------------------------------------
%% @doc Return the next Len bytes read from Sock.
%% @spec read(Sock::sock() | aitch:aitch_req(), Len::integer()) -> binary()
%% @throws {aitch_sock, eof} | {aitch_sock, {read_error, Reason::term()}}
%% @see read/3
%% @end
%%-------------------------------------------------------------------
read(S, Len) ->
    read(S, Len, infinity).

%%-------------------------------------------------------------------
%% @doc Return a binary of the next Len bytes read from Sock.
%%
%% Blocks until Len bytes are read. Use a Len of 0 to indicate
%% blocking (if needed) until the next set of bytes are available
%% and returning those regardless of size.
%%
%% @spec read(Sock::sock() | aitch:aitch_req(), Len::integer(), timeout()) ->
%%           binary()
%% @throws {aitch_sock, eof} | {aitch_sock, timeout} |
%%         {aitch_sock, {read_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
read(#aitch_req{sock = S}, Len, Timeout) ->
    read(S, Len, Timeout);
read(S, Len, Opts) when is_list(Opts) ->
    read(S, Len, proplists:get_value(read_timeout, Opts));
read(#sock{r = R, resp = Resp}, Len, Timeout) ->
    erlang:send(R, {self(), read, Resp, Len, Timeout}),
    receive
	{ok, <<>>} ->
	    throw({aitch_sock, eof});
	{ok, Data} ->
	    Data;
	{error, closed} ->
	    throw({aitch_sock, eof});
	%% known errors: timeout, emsgsize
	{error, timeout} ->
	    throw({aitch_sock, timeout});
	{error, Error} ->
	    io:format("read error: ~p~n", [Error]),
	    throw({aitch_sock, {read_error, Error}})
    end.

%%-------------------------------------------------------------------
%% @doc Return a string of the next line read from the socket.
%% @see read_line/2
%% @spec read_line(Sock::sock() | aitch:aitch_req()) -> string()
%% @throws {aitch_sock, eof} | {aitch_sock, {read_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
read_line(S) ->
    read_line(S, infinity, []).

%%-------------------------------------------------------------------
%% @doc Return a string of the next line read from the socket.
%% @spec read_line(Sock::sock() | aitch:aitch_req(), timeout()) -> string()
%% @throws {aitch_sock, eof} | {aitch_sock, timeout} |
%%         {aitch_sock, {read_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
read_line(S, Timeout) ->
    read_line(S, Timeout, []).

read_line(S, Timeout, Acc) ->
    Data = binary_to_list(read(S, 0, Timeout)),
    case string:chr(Data, $\n) of
        0 -> % not found
	    read_line(S, Timeout, [Data | Acc]);
        1 ->
            [$\n | Rest] = Data,
	    unread(S, Rest),
	    Acc;
        Idx ->
            LineLen = case lists:nth(Idx - 1, Data) of
                          $\r -> Idx - 2;
                          _   -> Idx - 1
                      end,
            Line = string:substr(Data, 1, LineLen),
            Rest = string:substr(Data, Idx + 1),
	    unread(S, Rest),
	    lists:flatten(lists:reverse([Line | Acc]))
    end.

%%-------------------------------------------------------------------
%% @doc Read the request line from the socket.
%% @see read_req_line/2
%% @spec read_req_line(Sock::sock() | aitch:aitch_req()) ->
%%       {Method::string(), URI::string(), vsn()}
%% @throws {aitch_sock, eof} | {aitch_sock, timeout} |
%%         {aitch_sock, {read_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
read_req_line(#aitch_req{sock = S, opts = Opts}) ->
    read_req_line(S, Opts);
read_req_line(S) ->
    read_req_line(S, infinity).

%%-------------------------------------------------------------------
%% @doc Read the request line from the socket.
%% @spec read_req_line(Sock::sock() | aitch:aitch_req(),
%%                     Opts::proplist() | timeout()) ->
%%           {Method::string(), Uri::string(), vsn()}
%% @throws {aitch_sock, eof} | {aitch_sock, timeout} |
%%         {aitch_sock, {read_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
read_req_line(S, Opts) when is_list(Opts) ->
    read_req_line(S, proplists:get_value(read_timeout, Opts));
read_req_line(S, Timeout) ->
    case string:tokens(read_line(S, Timeout), " ") of
        [Method, Uri, "HTTP/" ++ VsnStr] ->
            {Method, Uri, aitch:list_to_vsn(VsnStr)};
        [Method, Uri] ->
            {Method, Uri, {1, 0}};
	_ -> throw({aitch_sock, parse_error})
    end.

unread(_S, []) ->
    ok;
unread(#aitch_req{sock = S}, Data) ->
    unread(S, Data);
unread(#sock{r = R, resp = Resp}, Data) ->
    erlang:send(R, {unread, Resp, Data}).

%%-------------------------------------------------------------------
%% @doc Write to the socket.
%% @spec write(Sock::sock() | aitch:aitch_req(), Data::iolist()) -> ok
%% @throws {aitch_sock, {write_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
write(#aitch_req{sock = S}, Data) ->
    write(S, Data);
write(#sock{w = W, resp = Resp}, Data) ->
    erlang:send(W, {self(), write, Resp, Data}),
    receive
        ok         -> ok;
        {error, R} -> throw({aitch_sock, {write_error, R}});
	Msg        -> throw({aitch_sock, {unknown_w_msg, Msg}})
    end.

%%-------------------------------------------------------------------
%% @doc Write the response line to the socket.
%% @spec write_resp_line(Sock::sock() | aitch:aitch_req(), aitch:status()) -> ok
%% @throws {aitch_sock, {write_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
write_resp_line(#aitch_req{sock = S, vsn = Vsn}, Status) ->
    write_resp_line(S, Vsn, Status).

%%-------------------------------------------------------------------
%% @doc Write the response line to the socket.
%% @spec write_resp_line(Sock::sock() | aitch:aitch_req(),
%%                       vsn(), aitch:status()) -> ok
%% @throws {aitch_sock, {write_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
write_resp_line(S, Vsn, Status) when is_atom(Status); is_integer(Status)  ->
    write_resp_line(S, Vsn, aitch:code_to_list(Status));
write_resp_line(S, Vsn, {StatusCode, StatusPhrase}) ->
    write_resp_line(S, Vsn, [StatusCode, $ , StatusPhrase]);
write_resp_line(S, Vsn, Status) ->
    write(S, [<<"HTTP/">>, aitch:vsn_to_iolist(Vsn), $ , Status, ?EOL]).

%%-------------------------------------------------------------------
%% @doc Return the name of the socket's peer.
%% @spec peer_name(Sock::sock() | aitch:aitch_req()) -> string()
%% @throws {aitch_sock, {peername_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
peer_name(S) ->
    {Addr, _Port} = peername(S),
    Addr.

%%-------------------------------------------------------------------
%% @doc Return the port of the socket's peer.
%% @spec peer_port(Sock::sock() | aitch:aitch_req()) -> integer()
%% @throws {aitch_sock, {peername_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
peer_port(S) ->
    {_Addr, Port} = peername(S),
    Port.

%%-------------------------------------------------------------------
%% @doc Return the address and port of the socket's peer.
%% @spec peername(Sock::sock() | aitch:aitch_req()) -> {string(), integer()}
%% @throws {aitch_sock, {peername_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
peername(#aitch_req{sock = S}) ->
    peername(S);
peername(#sock{peername = Peername}) ->
    case Peername() of
	{ok, {Addr, Port}} -> {Addr, Port};
	{error, Error}     -> throw({aitch_sock, {peername_error, Error}})
    end.

%%-------------------------------------------------------------------
%% @doc Return the name of the socket's local side.
%% @spec this_name(Sock::sock() | aitch:aitch_req()) -> string()
%% @throws {aitch_sock, {peername_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
this_name(S) ->
    {Addr, _Port} = sockname(S),
    Addr.

%%-------------------------------------------------------------------
%% @doc Return the port of the socket's local side.
%% @spec this_port(Sock::sock() | aitch:aitch_req()) -> integer()
%% @throws {aitch_sock, {peername_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
this_port(S) ->
    {_Addr, Port} = sockname(S),
    Port.

%%-------------------------------------------------------------------
%% @doc Return the address and port of the socket's local side.
%% @spec sockname(Sock::sock() | aitch:aitch_req()) -> {string(), integer()}
%% @throws {aitch_sock, {peername_error, Reason::term()}}
%% @end
%%-------------------------------------------------------------------
sockname(#aitch_req{sock = S}) ->
    sockname(S);
sockname(#sock{sockname = Sockname}) ->
    case Sockname() of
	{ok, {Addr, Port}} -> {Addr, Port};
	{error, Error}     -> throw({aitch_sock, {sockname_error, Error}})
    end.

done_reading(#aitch_req{sock = S}) ->
    done_reading(S);
done_reading(#sock{r = R, resp = Resp}) ->
    erlang:send(R, {done_reading, Resp}).

done_writing(#aitch_req{sock = S}) ->
    done_writing(S);
done_writing(#sock{w = W, resp = Resp}) ->
    erlang:send(W, {done_writing, Resp}).

new_resp(#aitch_req{sock = S}, Mode) ->
    new_resp(S, Mode);
new_resp(#sock{w = W, resp = Ctrl} = S, wo) ->
    Resp = make_ref(),
    erlang:send(W, {add_resp, Ctrl, Resp}),
    S#sock{resp = Resp};
new_resp(#sock{r = R, w = W, resp = Ctrl} = S, rw) ->
    Resp = make_ref(),
    erlang:send(R, {add_resp, Ctrl, Resp}),
    erlang:send(W, {add_resp, Ctrl, Resp}),
    done_reading(S),
    S#sock{resp = Resp}.

r_sock_loop(#r_state{sock = S, ctrl = Ctrl, resp = Resp} = State) ->
    r_sock_loop(
      receive
          {Pid, read, Resp, Len, Timeout} ->
              erlang:send(Pid, gen_tcp:recv(S, Len, Timeout)),
              State;

          {unread, Resp, Data} ->
              ok = gen_tcp:unrecv(S, Data),
              State;

          {add_resp, Ctrl, NewResp} ->
              r_add_resp_(State, NewResp);

          {done_reading, Resp} ->
              done_reading_(State, Resp);

          {done_writing, Resp} ->
              error_logger:warning_msg(
                "aitch_sock: done writing before reading.~n" ++
                "Did your handler forget to read the request body?~n"),
              State;
          {done_writing, _} ->
              State;

          {close, Ctrl} ->
              exit(normal);

          {'EXIT', _From, normal} ->
              State;
          {'EXIT', From, Reason} ->
              io:format("reader got 'EXIT': ~p~n", [Reason]),
              kill_pids_q_(From, State),
              exit(pipe_crash);
          {Msg, _From, Reason} when is_atom(Msg) ->
              io:format("reader got ~p: ~p~n", [Msg, Reason]),
              State
      end).

w_sock_loop(#w_state{resp = close, sock = S}) ->
    gen_tcp:close(S),
    exit(normal);
w_sock_loop(#w_state{sock = S, ctrl = Ctrl, resp = Resp} = State) ->
    w_sock_loop(receive
		    {Pid, write, PidResp, Data}
		    when PidResp == Resp; PidResp == Ctrl->
			erlang:send(Pid, gen_tcp:send(S, Data)),
			State;

		    {done_writing, Resp} ->
			done_writing_(State, Resp);

		    {add_resp, Ctrl, NewResp} ->
			w_add_resp_(State, NewResp);
		    
		    {close, Ctrl} ->
			exit(normal);

		    {'EXIT', _From, normal} ->
			State;

		    {'EXIT', From, Reason} ->
			io:format("exit from ~p: ~p~n", [From, Reason]),
			kill_pids_q_(From, State),
			exit(pipe_crash)
		end).

% alternate between the controller and a responder reading
done_reading_(#r_state{ctrl = Resp, resp_q = RespQ} = State, Resp) ->
    case queue:out(RespQ) of
	{{value, NextResp}, RespQ2} ->
	    State#r_state{resp = NextResp, resp_q = RespQ2};
	{empty, RespQ2} ->
	    State#r_state{resp = undefined, resp_q = RespQ2}
    end;
done_reading_(#r_state{ctrl = Ctrl} = State, _Resp) ->
    State#r_state{resp = Ctrl}.

done_writing_(#w_state{resp_q = RespQ, r = R} = State, Resp) ->
    erlang:send(R, {done_writing, Resp}),
    case queue:out(RespQ) of
	{{value, NextResp}, RespQ2} ->
	    State#w_state{resp = NextResp, resp_q = RespQ2};
	{empty, RespQ2} ->
	    State#w_state{resp = undefined, resp_q = RespQ2}
    end.

r_add_resp_(#r_state{resp_q = RespQ} = State, Resp) ->
    State#r_state{resp_q = queue:in(Resp, RespQ)}.

w_add_resp_(#w_state{resp = undefined} = State, Resp) ->
    State#w_state{resp = Resp};
w_add_resp_(#w_state{resp_q = RespQ} = State, Resp) ->
    State#w_state{resp_q = queue:in(Resp, RespQ)}.

kill_pids_q_(_CrashedPid, #r_state{} = State) ->
    % todo: figure out what state this pid was in, and kill others accordingly
    lists:foreach(fun (Pid) -> exit(Pid, pipe_crash) end,
		  queue:to_list(State#r_state.resp_q));
kill_pids_q_(_CrashedPid, #w_state{} = State) ->
    % todo: figure out what state this pid was in, and kill others accordingly
    lists:foreach(fun (Pid) -> exit(Pid, pipe_crash) end,
		  queue:to_list(State#w_state.resp_q)).

