% note that stuff here may/should end up somewhere else, such as
% a completely seperate library
-module(crary_util).

-export([rfc1123_date/0, spawn_link/1]).

rfc1123_date() ->
    {{Y, M, D} = Date, {H, N, S}} = calendar:universal_time(),
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

%% proc_lib:spawn_link() doesn't detect uncaught throws, do so and
%% return them in a form it will detect and handle
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
