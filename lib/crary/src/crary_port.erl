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

%%% public
-export([start_link/2, start_link/3, accepted/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).

-record(state, {listen_socket,
                tcp_port,
                acceptor,
                handler,
                options}).

%% @doc Start a crary_port server to listen on a port. Its not
%% preferable to call this directly; see {@link crary:start/2}
%% @spec start_link(integer(), crary:mfa()) ->
%%           {ok, pid()} | ignore |
%%           {error, {already_started, pid()}} | {error, term()}
start_link(TcpPort, Handler) ->
    start_link(TcpPort, Handler, []).

%% @doc Start a crary_port server to listen on a port. Its not
%% preferable to call this directly; see {@link crary:start/3}
%% @spec start_link(integer(), crary:mfa(), crary:proplist()) ->
%%           {ok, pid()} | ignore |
%%           {error, {already_started, pid()}} | {error, term()}
%% @see start_link/3
start_link(TcpPort, Handler, Options) when is_integer(TcpPort) ->
    Name = list_to_atom("crary_" ++ integer_to_list(TcpPort)),
    gen_server:start_link({local, Name}, ?MODULE,
                          {TcpPort, Handler, Options}, []).

%% @doc {@link crary_sock} calls this to notify this module that it
%% has successfully accept(2)ed a new connection. This module can then
%% start a new {@link crary_sock} to start listening for the next
%% connection.
%% @spec(pid(), pid()) -> ok()
accepted(ServerPid) ->
    gen_server:cast(ServerPid, {accepted, self()}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

%% @private
%% @doc Called by gen_server framework at process startup. Create
%% listening socket
init({TcpPort, Handler, Options}) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(TcpPort, [binary, {packet, raw}, {active, false},
                                  {exit_on_close, false}]) of
        {ok, ListenSocket} ->
            try {ok, #state{listen_socket = ListenSocket,
                            tcp_port = TcpPort,
                            handler = Handler,
                            options = Options}}
            %% send ourself a message so we'll start an initial acceptor
            %% once we've finished starting
            after gen_server:cast(self(), {accepted, null})
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

%% @private
handle_call(Request, _From, _State) ->
    exit({error, {unexpected_request, Request}}).

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
                      options = Opts} = State) ->
    Pid = crary_sock:start_link(self(), LS, H, Opts),
    State#state{acceptor = Pid}.
