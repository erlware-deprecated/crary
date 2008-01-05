-module(example).
-export([hello_world/1]).
-export([counter/1]).

% code:add_path("_build/development/apps/aitch-0.1.0/ebin").
% code:add_path("_build/development/apps/aitch_dir_listing-0.1.0/ebin").
% application:start(sasl).
% application:start(aitch).
% aitch:start(8001, {example, hello_world, []}).
% aitch:start(8002, {example, counter, []}).
% aitch:start(8003, {aitch_dir_listing, handler, ["/tmp/test"]}).

-include("aitch.hrl").

hello_world(#aitch_req{uri = "/404"} = Req) ->
    aitch:not_found(Req);
hello_world(#aitch_req{uri = "/crash"} = _Req) ->
    exit(oops);
hello_world(#aitch_req{uri = "/chunk"} = Req) ->
    aitch:r(Req, 200, [{<<"transfer-encoding">>, <<"chunked">>}]),
    W = aitch_body:new_writer(Req),
    aitch_body:write(W, <<"<html><body>">>),
    aitch_body:write(W, <<"hello ">>),
    aitch_body:write(W, <<"world!">>),
    aitch_body:write(W, <<"</html></body>">>),
    aitch_body:done_writing(W),
    aitch_sock:done_writing(Req);
hello_world(Req) ->
    aitch:r(Req, ok, [], <<"<html><body>Hello World!</body></html>\r\n">>).


counter(#aitch_req{method = Method} = Req) ->
    Count = case Method of
                "GET" -> 0;
                "POST" ->
                    Body = binary_to_list(aitch_body:read_all(Req)),
                    [BodyCount, BodyAdd] = string:tokens(Body, "&"),
                    ["count", Num] = string:tokens(BodyCount, "="),
                    ["add", Add] = string:tokens(BodyAdd, "="),
                    list_to_integer(Num) + case Add of
                                               "%3C%3C" -> -1;
                                               "%3E%3E" -> 1
                                           end
            end,
    CountStr = integer_to_list(Count),
    aitch:r(Req, ok, [],
            [<<"<html>
                 <head><title>
                  Counter: ">>, CountStr, <<"
                 </title></head>
                 <body>
                  <form method=\"Post\" action=\"/\">
                   <input type=\"hidden\" name=\"count\"
                          value=\"">>, CountStr, <<"\">
                   Counter:
                   <input type=\"submit\" name=\"add\" value=\"<<\">
                   ">>, CountStr, <<"
                   <input type=\"submit\" name=\"add\" value=\">>\">
                  </form>
                 </body>
                </html>">>]).
