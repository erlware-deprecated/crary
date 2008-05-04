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

-export([rfc1123_date/0, rfc1123_date/1, spawn_link/1]).

%% @doc Return a date string for "now" in the format specified by RFC1123.
%% @spec () -> iolist()
rfc1123_date() ->
    rfc1123_date(calendar:universal_time()).

%% @doc Return a date string in the format specified by RFC1123.
%% The time should be passed to this module in the same format the
%% {@link calendar} module uses.
%% @spec (calendar()) -> iolist()
rfc1123_date({{Y, M, D} = Date, {H, N, S}}) ->
    io_lib:format("~s, ~.2.0w ~s ~b ~.2.0w:~.2.0w:~.2.0w GMT",
                  [dow_name(Date), D, moy_name(M), Y, H, N, S]).

dow_name(Date) ->
    case calendar:day_of_the_week(Date) of
        1 -> <<"Mon">>;
        2 -> <<"Tue">>;
        3 -> <<"Wed">>;
        4 -> <<"Thu">>;
        5 -> <<"Fri">>;
        6 -> <<"Sat">>;
        7 -> <<"Sun">>
    end.

moy_name(1) -> <<"Jan">>;
moy_name(2) -> <<"Feb">>;
moy_name(3) -> <<"Mar">>;
moy_name(4) -> <<"Apr">>;
moy_name(5) -> <<"May">>;
moy_name(6) -> <<"Jun">>;
moy_name(7) -> <<"Jul">>;
moy_name(8) -> <<"Aug">>;
moy_name(9) -> <<"Sep">>;
moy_name(10) -> <<"Oct">>;
moy_name(11) -> <<"Nov">>;
moy_name(12) -> <<"Dec">>.

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
