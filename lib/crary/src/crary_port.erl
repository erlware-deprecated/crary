-module(crary_port).

-behaviour(gen_server).

-export([start_link/2, start_link/3, accepted/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([code_change/3]).

-record(state, {listen_socket,
                tcp_port,
                acceptor,
                handler,
                options}).

start_link(TcpPort, Handler) ->
    start_link(TcpPort, Handler, []).

start_link(TcpPort, Handler, Options) when is_integer(TcpPort) ->
    Name = list_to_atom("crary_" ++ integer_to_list(TcpPort)),
    gen_server:start_link({local, Name}, ?MODULE,
                          {TcpPort, Handler, Options}, []).

%% the last accept was used, so create a new accept process
accepted(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {accepted, Pid}).

%% Called by gen_server framework at process startup. Create listening socket
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

handle_call(Request, _From, _State) ->
    exit({error, {unexpected_request, Request}}).

handle_cast({accepted, _Pid}, State) ->
    {noreply, start_acceptor(State)}.

%% The current acceptor has died, wait a little and try again
handle_info({'EXIT', Pid, Reason}, #state{acceptor=Pid} = State) ->
    io:format("Acceptor ~p died: ~p~n", [Pid, Reason]),
    timer:sleep(500),
    {noreply, start_acceptor(State)};
%% this happens when the socket closes
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
%% this happens when the socket timeouts
handle_info({'EXIT', Pid, timeout}, State) ->
    io:format("socket timeout exit: ~p~n", [Pid]),
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("Socket ~p exited: ~p~n", [Pid, Reason]),
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.listen_socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private

start_acceptor(#state{listen_socket = LS,
                      handler = H,
                      options = Opts} = State) ->
    Pid = crary_sock:start_link(self(), LS, H, Opts),
    State#state{acceptor = Pid}.
