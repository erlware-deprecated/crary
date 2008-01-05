-module(aitch_sock).

%% public
-export([start_link/4]).
-export([list_to_vsn/1, vsn_to_iolist/1]).
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

-record(r_state, {sock, ctrl, resp, resp_q, w}).
-record(w_state, {sock, ctrl, resp, resp_q, r}).
-record(sock, {r, w, resp, sockname, peername, close}).

start_link(ListenPid, ListenSock, Handler, Opts) ->
    aitch_util:spawn_link(
      fun() -> accept(ListenPid, ListenSock, Handler, Opts) end).

accept(ListenPid, ListenSock, Handler, Opts) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            %% send message to the listener process to create a new acceptor
            aitch_port:accepted(ListenPid, self()),
            start_ctrl_with_wrapped_sock(Sock, Handler, Opts);
        {error, R} ->
            exit({aitch_error, {accept_failed, R}})
    end.

start_ctrl_with_wrapped_sock(Sock, Handler, Opts) ->
    process_flag(trap_exit, true),
    Ctrl = make_ref(),
    WPid = start_link_w(Sock, Ctrl, self()),
    aitch_ctrl:start_link(make_sock(self(), WPid, Ctrl, Sock), Handler, Opts),
    init_r(Sock, Ctrl, WPid).

make_sock(RPid, WPid, Ctrl, Sock) ->
    #sock{r = RPid,
          w = WPid,
          resp = Ctrl,
          sockname = fun() -> sockname(Sock) end,
          peername = fun() -> peername(Sock) end,
          close = fun() -> gen_tcp:close(Sock) end}.

init_r(Sock, Ctrl, WPid) ->
    r_sock_loop(#r_state{sock = Sock,
                         ctrl = Ctrl,
                         w = WPid,
                         resp = Ctrl,
                         resp_q = queue:new()}).

start_link_w(Sock, Ctrl, R) ->
    aitch_util:spawn_link(
      fun () ->
              process_flag(trap_exit, true),
              w_sock_loop(#w_state{sock = Sock,
                                   ctrl = Ctrl,
                                   resp_q = queue:new(),
                                   r = R})
      end).

list_to_vsn(VsnStr) ->
    {Maj, [$. | MinS]} = string:to_integer(VsnStr),
    {Maj, list_to_integer(MinS)}.

vsn_to_iolist(#aitch_req{vsn = Vsn}) ->
    vsn_to_iolist(Vsn);
vsn_to_iolist({Maj, Min}) ->
    [integer_to_list(Maj), $., integer_to_list(Min)].

close(#aitch_req{sock = S}) ->
    close(S);
close(#sock{r = R, w = W, resp = Ctrl, close = Close}) ->
    erlang:send(R, {close, Ctrl}),
    erlang:send(W, {close, Ctrl}),
    Close().

% on eof, we don't know if it was a close() or a half duplex
% shutdown(), so close the reader, and queue the writer to
% finish shutting down
close_reader(#aitch_req{sock = S}) ->
    close_reader(S);
close_reader(#sock{r = R, w = W, resp = Ctrl}) ->
    erlang:send(R, {close, Ctrl}),
    erlang:send(W, {add_resp, Ctrl, close}).

read(S, Len) ->
    read(S, Len, infinity).

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
	    throw({aitch_sock, {read_error, timeout}});
	{error, Error} ->
	    io:format("read error: ~p~n", [Error]),
	    throw({aitch_sock, {read_error, Error}})
    end.

read_line(S) ->
    read_line(S, infinity, []).

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

read_req_line(#aitch_req{sock = S, opts = Opts}) ->
    read_req_line(S, Opts);
read_req_line(S) ->
    read_req_line(S, infinity).

read_req_line(S, Opts) when is_list(Opts) ->
    read_req_line(S, proplists:get_value(read_timeout, Opts));
read_req_line(S, Timeout) ->
    case string:tokens(read_line(S, Timeout), " ") of
        [Method, URI, "HTTP/" ++ VsnStr] -> {Method, URI, list_to_vsn(VsnStr)};
        [Method, URI]                    -> {Method, URI, {1, 0}};
	_ -> throw({aitch_sock, parse_error})
    end.

unread(_S, []) ->
    ok;
unread(#aitch_req{sock = S}, Data) ->
    unread(S, Data);
unread(#sock{r = R, resp = Resp}, Data) ->
    erlang:send(R, {unread, Resp, Data}).

write(#aitch_req{sock = S}, Data) ->
    write(S, Data);
write(#sock{w = W, resp = Resp}, Data) ->
    erlang:send(W, {self(), write, Resp, Data}),
    receive
        ok         -> ok;
        {error, R} -> throw({aitch_sock, {write_error, R}});
	Msg        -> throw({aitch_sock, {unknown_w_msg, Msg}})
    end.

write_resp_line(#aitch_req{sock = S, vsn = Vsn}, Status) ->
    write_resp_line(S, Vsn, Status).

write_resp_line(S, Vsn, Status) when is_atom(Status); is_integer(Status)  ->
    write_resp_line(S, Vsn, aitch:code_to_list(Status));
write_resp_line(S, Vsn, {StatusCode, StatusPhrase}) ->
    write_resp_line(S, Vsn, [StatusCode, $ , StatusPhrase]);
write_resp_line(S, Vsn, Status) ->
    write(S, [<<"HTTP/">>, vsn_to_iolist(Vsn), $ , Status, ?EOL]).

peer_name(S) ->
    {Addr, _Port} = peername(S),
    Addr.

peer_port(S) ->
    {_Addr, Port} = peername(S),
    Port.

this_name(S) ->
    {Addr, _Port} = sockname(S),
    Addr.

this_port(S) ->
    {_Addr, Port} = sockname(S),
    Port.

sockname(#aitch_req{sock = S}) ->
    sockname(S);
sockname(#sock{sockname = Sockname}) ->
    Sockname();
sockname(S) when is_port(S) ->
    case inet:sockname(S) of
	{ok, {Addr, Port}} -> {Addr, Port};
	{error, Error}     -> throw({aitch_sock, {sockname_error, Error}})
    end.

peername(#aitch_req{sock = S}) ->
    peername(S);
peername(#sock{peername = Peername}) ->
    Peername();
peername(S) when is_port(S) ->
    case inet:peername(S) of
	{ok, {Addr, Port}} -> {Addr, Port};
	{error, Error}     -> throw({aitch_sock, {peername_error, Error}})
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

