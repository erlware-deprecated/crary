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
%%% @author Scott R Parish <srp@srparish.net>
%%% @doc
%%% @end
%%% @copyright 2007,2008 Scott R Parish
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
%% @spec () -> {ok,Pid} | ignore | {error,Error}
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%=================================================================
%% Supervisor callbacks
%%=================================================================
%%-----------------------------------------------------------------
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
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


