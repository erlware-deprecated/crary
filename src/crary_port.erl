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

%%% @doc This module is responsible for opening a TCP port to listen
%%% on and starting new crary_sock workers to accept(2) and process
%%% requests. To start crary listening on a port, see {@link crary:start/3}.

-module(crary_port).

-behaviour(gen_server).

%% public
-export([start_link/2, start_link/3, accepted/1]).
-export([opts/1, opts/2, handler/1, handler/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).

-record(state, {listen_socket,
                tcp_port,
                acceptor,
                handler,
                opts}).

%% @doc Start a crary_port server to listen on a port. Its not
%% preferable to call this directly; see {@link crary:start/2}
%% @spec (TcpPort::integer() | {inet:ip_address(), TcpPort::integer()},
%%        crary:handler()) ->
%%       {ok, pid()} | ignore |
%%       {error, {already_started, pid()}} | {error, term()}
start_link(IpTcpPort, Handler) ->
    start_link(IpTcpPort, Handler, []).

%% @doc Start a crary_port server to listen on a port. Its not
%% preferable to call this directly; see {@link crary:start/3}
%% @spec (TcpPort::integer() | {inet:ip_address(), TcpPort::integer()},
%%        crary:handler(), proplist()) ->
%%       {ok, pid()} | ignore |
%%       {error, {already_started, pid()}} | {error, term()}
%% @see start_link/3
start_link(IpTcpPort, Handler, Opts) ->
    gen_server:start_link({local, crary_name(IpTcpPort)}, ?MODULE,
                          {IpTcpPort, Handler, Opts}, []).

%% @doc {@link crary_sock} calls this to notify this module that it
%% has successfully accept(2)ed a new connection. This module can then
%% start a new {@link crary_sock} to start listening for the next
%% connection.
%% @spec (pid()) -> ok()
accepted(ServerPid) ->
    gen_server:cast(ServerPid, {accepted, self()}).

%% @private
opts(TcpIpSpec) ->
    gen_server:call(crary_name(TcpIpSpec), opts).

%% @private
opts(TcpIpSpec, NewOpts) ->
    gen_server:call(crary_name(TcpIpSpec), {opts, NewOpts}).

%% @private
handler(TcpIpSpec) ->
    gen_server:call(crary_name(TcpIpSpec), handler).

%% @private
handler(TcpIpSpec, NewHandler) ->
    gen_server:call(crary_name(TcpIpSpec), {handler, NewHandler}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

%% @private
%% @doc Called by gen_server framework at process startup. Create
%% listening socket
init({IpTcpPort, Handler, Opts}) ->
    process_flag(trap_exit, true),
    {ListenOpts, TcpPort} = case IpTcpPort of
                                {IpAddr, TcpP} -> {[{ip, IpAddr}], TcpP};
                                TcpP when is_integer(TcpP) -> {[], TcpP}
                            end,
    %% if updating this: be sure to update sock_opts/1 below
    SockOpts = [binary,
                {packet, raw},
                {active, false},
                {exit_on_close, false} | ListenOpts] ++ sock_opts(Opts),
    case gen_tcp:listen(TcpPort, SockOpts) of
        {ok, ListenSocket} ->
            try {ok, #state{listen_socket = ListenSocket,
                            tcp_port = TcpPort,
                            handler = Handler,
                            opts = Opts}}
            %% send ourself a message so we'll start an initial acceptor
            %% once we've finished starting
            after gen_server:cast(self(), {accepted, null})
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call(opts, _From, State) ->
    {reply, State#state.opts, State};
handle_call({opts, NewOpts}, _From, State) ->
    {reply, ok, State#state{opts = NewOpts}};
handle_call(handler, _From, State) ->
    {reply, State#state.handler, State};
handle_call({handler, NewHandler}, _From, State) ->
    {reply, ok, State#state{handler = NewHandler}}.

%% @private
handle_cast({accepted, _Pid}, State) ->
    {noreply, start_acceptor(State)}.

%% @private
%% @doc The current acceptor has died, wait a little and try again
handle_info({'EXIT', Pid, Reason}, #state{acceptor=Pid} = State) ->
    io:format("Acceptor ~p died: ~p~n", [Pid, Reason]),
    timer:sleep(500),
    {noreply, start_acceptor(State)};
%% @private
%% @doc this happens when the socket closes
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
%% @private
%% @doc this happens when the socket timeouts
handle_info({'EXIT', Pid, timeout}, State) ->
    io:format("socket timeout exit: ~p~n", [Pid]),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("Socket ~p exited: ~p~n", [Pid, Reason]),
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% private
%%%====================================================================

start_acceptor(#state{listen_socket = LS,
                      handler = H,
                      opts = Opts} = State) ->
    Pid = crary_sock:start_link(self(), LS, H, Opts),
    State#state{acceptor = Pid}.


crary_name({{A, B, C, D}, TcpPort}) ->
    list_to_atom(
      lists:flatten(io_lib:format("crary_~w_~w_~w_~w__~w",
                                  [A, B, C, D, TcpPort])));
crary_name({{A, B, C, D, E, F, G, H}, TcpPort}) ->
    list_to_atom(
      lists:flatten(
        io_lib:format("crary_" ++
                      "~.16B_~.16B_~.16B_~.16B_~.16B_~.16B_~.16B_~.16B__" ++
                      "~w",
                      [A, B, C, D, E, F, G, H,
                       TcpPort])));
crary_name(TcpPort) when is_integer(TcpPort) ->
    list_to_atom("crary_" ++ integer_to_list(TcpPort)).

sock_opts(Opts) ->
    lists:filter(fun (list)               -> false;
                     ({active, _})        -> false;
                     ({exit_on_close, _}) -> false;
                     ({header, _})        -> false;
                     ({packet, _})        -> false;
                     ({packet_size, _})   -> false
                 end,
                 proplists:get_value(socket_opts, Opts, [])).
