%%%----------------------------------------------------------------
%%% @author Scott R Parish <srp@srparish.net> 
%%% @doc
%%% @end
%%% @copyright 2007 Scott R Parish 
%%%----------------------------------------------------------------
-module(crary_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).


%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%=================================================================
%% API functions
%%=================================================================

%% @doc Starts the supervisor
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%=================================================================
%% Supervisor callbacks
%%=================================================================
%%-----------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc
%%  Whenever a supervisor is started using 
%%  supervisor:start_link/[2,3], this function is called by the 
%%  new process to find out about restart strategy, maximum 
%%  restart frequency and childspecifications.
%% @end
%%-----------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 0, 1}, []}}.

%%=================================================================
%% Internal functions
%%=================================================================


