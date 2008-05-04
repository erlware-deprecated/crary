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

%%%----------------------------------------------------------------
%%% @author Scott R Parish <srp@srparish.net>
%%% @doc
%%%
%%% @end
%%% @copyright 2007 Scott R Parish
%%%----------------------------------------------------------------,
-module(crary_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%=================================================================
%% Application callbacks
%%=================================================================

%%-----------------------------------------------------------------
%% @spec (Type, StartArgs) -> {ok, Pid}        |
%%                            {ok, Pid, State} |
%%                            {error, Reason}
%% @doc
%%  This function is called whenever an application
%%  is started using application:start/1,2, and should
%%  start the processes of the application. If the application
%%  is structured according to the OTP design principles as a
%%  supervision tree, this means starting the top supervisor
%%  of the tree.
%% @end
%%-----------------------------------------------------------------
start(_Type, _StartArgs) ->
   case  crary_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
         Error ->
            Error
    end.

%%-----------------------------------------------------------------
%% @spec (State) -> void()
%% @doc
%%  This function is called whenever an application
%%  has stopped. It is intended to be the opposite of
%%  Module:start/2 and should do any necessary cleaning
%%  up. The return value is ignored.
%% @end
%%-----------------------------------------------------------------
stop(_State) ->
   ok.


%%=================================================================
%% Internal functions\n",
%%=================================================================

