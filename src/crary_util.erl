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
%%% @doc note that stuff here may/should end up somewhere else, such as
%%% a completely seperate library
-module(crary_util).

-export([spawn_link/1]).

%% @doc proc_lib:spawn_link() doesn't detect uncaught throws, do so and
%% return them in a form it will detect and handle
%% @spec (function()) -> pid()
spawn_link(F) ->
    proc_lib:spawn_link(
      fun () ->
              try F()
              catch
                  _:normal -> normal;
                  _:shutdown -> shutdown;
                  C:R -> {'EXIT', {nocatch, {C, R, erlang:get_stacktrace()}}}
              end
      end).
