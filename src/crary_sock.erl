%%% Copyright (c) 2007, 2008 Scott Parish
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.

%%%-------------------------------------------------------------------

%%% @author Scott Parish <srp@srparish.net>
%%% @copyright 2007, 2008 Scott Parish <srp@srparish.net>
%%% @doc Sock abstraction for HTTP.
%%% Provides an abstraction over a TCP socket to make it easy to write
%%% streaming/pipe-lining/etc HTTP servers
%%% @end

-module(crary_sock).

%% public
-export([start_link/4]).
-export([read/2, read/3, write/2]).
-export([read_line/1, read_line/2]).
-export([read_req_line/1, read_req_line/2]).
-export([write_resp_line/2, write_resp_line/3]).
-export([done_reading/1, done_writing/1]).
-export([sockname/1, peername/1]).
-export([peer_name/1, peer_port/1, this_name/1, this_port/1]).

%% methods for crary_ctrl
-export([close_reader/1]).
-export([new_resp/2]).

-include("crary.hrl").

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
-record(sock, {r,          %% pid() of reader
               w,          %% pid() of writer
               resp,       %% reference() identifies when this sock can
                           %% be read from / written to
               sockname,   %% fun() which returns socket's inet:sockname()
               peername}). %% fun() which returns socket's inet:peername()

%%% @type sock() = record()
%%% @type r_state() = record()
%%% @type w_state() = record()
%%% @type timeout() = Milliseconds | infinity
%%%       Milliseconds = integer()

%% @doc Usually only called by crary_port to spawn/accept/process.
%% @spec (Port::pid(), ListenSock::port(),
%%        crary:handler(), crary:proplists()) -> pid()
start_link(PortPid, ListenSock, Handler, Opts) ->
    crary_util:spawn_link(
      fun() -> accept(PortPid, ListenSock, Handler, Opts) end).

%% @private
%% @doc accept() on the socket and process resulting connection.
%% @spec (Port::pid(), ListenSock::port(),
%%        crary:handler(), crary:proplist()) -> none()
accept(PortPid, ListenSock, Handler, Opts) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            %% send message to the listener process to create a new acceptor
            crary_port:accepted(PortPid),
            start_ctrl_with_wrapped_sock(Sock, Handler, Opts);
        {error, R} ->
            exit({crary_error, {accept_failed, R}})
    end.

%% @private
%% @doc create processes for, link-together, and start the writer,
%% reader, and controller
%% @spec (Sock::port(), crary:handler(), crary:proplist()) -> none()
start_ctrl_with_wrapped_sock(Sock, Handler, Opts) ->
    Ctrl = make_ref(),
    RPid = start_link_r(Sock, Ctrl, self()),
    crary_ctrl:start_link(new_sock(RPid, self(), Ctrl, Sock), Handler, Opts),
    %% this process (self()) is linked to the port; we'll let this process
    %% become the writer as the reader and control processes can exit early,
    %% but the writer always has to be the last process to exit.
    start_w(Sock, Ctrl, RPid).

new_sock(RPid, WPid, Ctrl, Sock) ->
    #sock{r = RPid,
          w = WPid,
          resp = Ctrl,
          sockname = fun() -> inet:sockname(Sock) end,
          peername = fun() -> inet:peername(Sock) end}.

%% @private
%% @doc start the reader process by taking over this process
%% @spec (Sock::port(), Ctrl::reference(), Writer::pid()) -> none()
start_link_r(Sock, Ctrl, W) ->
    crary_util:spawn_link(
      fun () ->
              process_flag(trap_exit, true),
              r_sock_loop(#r_state{sock = Sock,
                                   ctrl = Ctrl,
                                   w = W,
                                   resp = Ctrl,
                                   resp_q = queue:new()})
      end).

%% @private
%% @doc spawn/start the writer process
%% @spec (Sock::port(), Ctrl::reference(), Reader::pid()) -> none()
start_w(Sock, Ctrl, R) ->
    process_flag(trap_exit, true),
    w_sock_loop(#w_state{sock = Sock,
                         ctrl = Ctrl,
                         resp_q = queue:new(),
                         r = R}).

%% @private
%% @doc Close the read half of the TCP socket.
%%
%% On hitting `EOF' when reading, we don't know if it was a full
%% close() or a half duplex shutdown(), so close the reader, and add
%% 'close' to the writers resp_q to finish shutting down when the last
%% pipe-lined response has been written.
%%
%% Only the crary_ctrl can use this function.
%%
%% @spec (Sock::sock() | crary:crary_req()) -> ok
%% @throws {crary_sock, {error_closing_sock, Reason::term()}}
close_reader(#crary_req{sock = S}) ->
    close_reader(S);
close_reader(#sock{r = R, w = W, resp = Ctrl}) ->
    erlang:send(R, {close, Ctrl}),
    erlang:send(W, {add_resp, Ctrl, close}),
    ok.

%% @doc Return the next `Len' bytes read from `Sock'.
%% @spec (Sock::sock() | crary:crary_req(), Len::integer()) -> binary()
%% @throws {crary_sock, eof} | {crary_sock, {read_error, Reason::term()}}
%% @see read/3
read(S, Len) ->
    read(S, Len, infinity).

%% @doc Return a binary of the next `Len' bytes read from `Sock'.
%%
%% Blocks until `Len' bytes are read. Use a `Len' of `0' to indicate
%% blocking (if needed) until the next set of bytes are available and
%% returning those regardless of size.
%%
%% @spec (Sock::sock() | crary:crary_req(), Len::integer(), timeout()) ->
%%       binary()
%% @throws {crary_sock, eof} | {crary_sock, timeout} |
%%         {crary_sock, {read_error, Reason::term()}}
read(#crary_req{sock = S}, Len, Timeout) ->
    read(S, Len, Timeout);
read(S, Len, Opts) when is_list(Opts) ->
    read(S, Len, proplists:get_value(read_timeout, Opts));
read(#sock{r = R, resp = Resp}, Len, Timeout) ->
    erlang:send(R, {self(), read, Resp, Len, Timeout}),
    receive
        {Resp, {ok, <<>>}} ->
            throw({crary_sock, eof});
        {Resp, {ok, Data}} ->
            Data;
        {Resp, {error, closed}} ->
            throw({crary_sock, eof});
        %% known errors: timeout, emsgsize
        {Resp, {error, timeout}} ->
            throw({crary_sock, timeout});
        {Resp, {error, Error}} ->
            io:format("read error: ~p~n", [Error]),
            throw({crary_sock, {read_error, Error}})
    end.

%% @doc Return a string of the next line read from the socket.
%% @spec (Sock::sock() | crary:crary_req()) -> string()
%% @throws {crary_sock, eof} | {crary_sock, {read_error, Reason::term()}}
%% @see read_line/2
read_line(S) ->
    read_line(S, infinity, []).

%% @doc Return a string of the next line read from the socket.
%% @spec (Sock::sock() | crary:crary_req(), timeout()) -> string()
%% @throws {crary_sock, eof} | {crary_sock, timeout} |
%%         {crary_sock, {read_error, Reason::term()}}
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

%% @doc Read the request line from the socket.
%% @see read_req_line/2
%% @spec (Sock::sock() | crary:crary_req()) ->
%%       {Method::string(), URI::string(), crary:vsn()}
%% @throws {crary_sock, eof} | {crary_sock, timeout} |
%%         {crary_sock, {read_error, Reason::term()}}
read_req_line(#crary_req{sock = S, opts = Opts}) ->
    read_req_line(S, Opts);
read_req_line(S) ->
    read_req_line(S, infinity).

%% @doc Read the request line from the socket.
%% @spec (Sock::sock() | crary:crary_req(), crary:proplist() | timeout()) ->
%%       {Method::string(), Uri::string(), crary:vsn()}
%% @throws {crary_sock, eof} | {crary_sock, timeout} |
%%         {crary_sock, {read_error, Reason::term()}}
read_req_line(S, Opts) when is_list(Opts) ->
    read_req_line(S, proplists:get_value(read_timeout, Opts));
read_req_line(S, Timeout) ->
    case string:tokens(read_line(S, Timeout), " ") of
        [Method, Uri, "HTTP/" ++ VsnStr] ->
            {Method, Uri, crary:list_to_vsn(VsnStr)};
        [Method, Uri] ->
            {Method, Uri, {1, 0}};
        _ -> throw({crary_sock, parse_error})
    end.

unread(_S, []) ->
    ok;
unread(#crary_req{sock = S}, Data) ->
    unread(S, Data);
unread(#sock{r = R, resp = Resp}, Data) ->
    erlang:send(R, {unread, Resp, Data}).

%% @doc Write to the socket.
%% @spec (Sock::sock() | crary:crary_req(), Data::iolist()) -> ok
%% @throws {crary_sock, {write_error, Reason::term()}}
write(#crary_req{sock = S}, Data) ->
    write(S, Data);
write(#sock{w = W, resp = Resp}, Data) ->
    erlang:send(W, {self(), write, Resp, Data}),
    receive
        {Resp, ok}         -> ok;
        {Resp, {error, R}} -> throw({crary_sock, {write_error, R}});
        {Resp, Msg}        -> throw({crary_sock, {unknown_w_msg, Msg}})
    end.

%% @doc Write the response line to the socket.
%% @spec (Sock::sock() | crary:crary_req(), crary:status()) -> ok
%% @throws {crary_sock, {write_error, Reason::term()}}
write_resp_line(#crary_req{sock = S, vsn = Vsn}, Status) ->
    write_resp_line(S, Vsn, Status).

%% @doc Write the response line to the socket.
%% @spec (Sock::sock() | crary:crary_req(), crary:vsn(), crary:status()) -> ok
%% @throws {crary_sock, {write_error, Reason::term()}}
write_resp_line(S, Vsn, Status) when is_atom(Status); is_integer(Status)  ->
    write_resp_line(S, Vsn, crary:code_to_binary(Status));
write_resp_line(S, Vsn, {StatusCode, StatusPhrase}) ->
    write_resp_line(S, Vsn, [StatusCode, $ , StatusPhrase]);
write_resp_line(S, Vsn, Status) ->
    write(S, [<<"HTTP/">>, crary:vsn_to_iolist(Vsn), $ , Status, ?EOL]).

%% @doc Return the name of the socket's peer.
%% @spec (Sock::sock() | crary:crary_req()) -> string()
%% @throws {crary_sock, {peername_error, Reason::term()}}
peer_name(S) ->
    {Addr, _Port} = peername(S),
    Addr.

%% @doc Return the port of the socket's peer.
%% @spec (Sock::sock() | crary:crary_req()) -> integer()
%% @throws {crary_sock, {peername_error, Reason::term()}}
peer_port(S) ->
    {_Addr, Port} = peername(S),
    Port.

%% @doc Return the address and port of the socket's peer.
%% @spec (Sock::sock() | crary:crary_req()) -> {string(), integer()}
%% @throws {crary_sock, {peername_error, Reason::term()}}
peername(#crary_req{sock = S}) ->
    peername(S);
peername(#sock{peername = Peername}) ->
    case Peername() of
        {ok, {Addr, Port}} -> {Addr, Port};
        {error, Error}     -> throw({crary_sock, {peername_error, Error}})
    end.

%% @doc Return the name of the socket's local side.
%% @spec (Sock::sock() | crary:crary_req()) -> string()
%% @throws {crary_sock, {peername_error, Reason::term()}}
this_name(S) ->
    {Addr, _Port} = sockname(S),
    Addr.

%% @doc Return the port of the socket's local side.
%% @spec (Sock::sock() | crary:crary_req()) -> integer()
%% @throws {crary_sock, {peername_error, Reason::term()}}
this_port(S) ->
    {_Addr, Port} = sockname(S),
    Port.

%% @doc Return the address and port of the socket's local side.
%% @spec (Sock::sock() | crary:crary_req()) -> {string(), integer()}
%% @throws {crary_sock, {peername_error, Reason::term()}}
sockname(#crary_req{sock = S}) ->
    sockname(S);
sockname(#sock{sockname = Sockname}) ->
    case Sockname() of
        {ok, {Addr, Port}} -> {Addr, Port};
        {error, Error}     -> throw({crary_sock, {sockname_error, Error}})
    end.

%% @doc Indicate that the handler or controller is done reading.
%%
%% This should be called whenever the handler or controller are done
%% {@link read/3}ing for a particular request. Doing this right away allows
%% the next pipe-lined request to start being handled.
%%
%% @spec (Sock::sock() | crary:crary_req()) -> ok
done_reading(#crary_req{sock = S}) ->
    done_reading(S);
done_reading(#sock{r = R, resp = Resp}) ->
    erlang:send(R, {done_reading, Resp}),
    ok.

%% @doc Indicate that the handler or controller is done writing.
%%
%% This should be called whenever the handler or controller are done
%% {@link write/2}ing for a particular response. Doing this right away
%% allows the next pipe-lined response to start being written.
%%
%% @spec (Sock::sock() | crary:crary_req()) -> ok
done_writing(#crary_req{sock = S}) ->
    done_writing(S);
done_writing(#sock{w = W, resp = Resp}) ->
    erlang:send(W, {done_writing, Resp}),
    ok.

%% @private
%% @doc Used by a controller: return a new {@link sock()} for a new handler.
%%
%% This should be called to create a unique {@link sock()} for each
%% handler to use. Basically this creates the references() used to
%% identify when the handler should be allowed to {@link read/3} as
%% well as {@link write/2}
%%
%% The mode `wo' should be used for requests such as `GET' where the
%% handler will not need to read any body. `rw' should be used for
%% requests such as ``PUT'' where the handler will be reading a body.
%%
%% @spec (Sock::sock() | crary:crary_req(), Mode) -> ok
%%       Mode = wo | rw
%% @see crary_body:has_body/1
new_resp(#crary_req{sock = S}, Mode) ->
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
              erlang:send(Pid, {Resp, gen_tcp:recv(S, Len, Timeout)}),
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
                "crary_sock: done writing before reading.~n" ++
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
                        erlang:send(Pid, {Resp, gen_tcp:send(S, Data)}),
                        State;

                    {done_writing, Resp} ->
                        done_writing_(State, Resp);

                    {add_resp, Ctrl, NewResp} ->
                        w_add_resp_(State, NewResp);

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

