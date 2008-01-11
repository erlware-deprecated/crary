-module(example).
-export([hello_world/1]).
-export([counter/1]).

% code:add_path("_build/development/apps/crary-0.1.0/ebin").
% code:add_path("_build/development/apps/crary_dir_listing-0.1.0/ebin").
% application:start(sasl).
% application:start(crary).
% crary:start(8001, {example, hello_world, []}).
% crary:start(8002, {example, counter, []}).
% crary:start(8003, {crary_dir_listing, handler, ["/Users/srp/tmp"]}).

-include("crary.hrl").

hello_world(#crary_req{uri = "/404"} = Req) ->
    crary:not_found(Req);
hello_world(#crary_req{uri = "/crash"} = _Req) ->
    exit(oops);
hello_world(#crary_req{uri = "/chunk"} = Req) ->
    crary:r(Req, 200, [{<<"transfer-encoding">>, <<"chunked">>}]),
    W = crary_body:new_writer(Req),
    crary_body:write(W, <<"<html><body>">>),
    crary_body:write(W, <<"hello ">>),
    crary_body:write(W, <<"world!">>),
    crary_body:write(W, <<"</html></body>">>),
    crary_body:done_writing(W),
    crary_sock:done_writing(Req);
hello_world(Req) ->
    crary:r(Req, ok, [], <<"<html><body>Hello World!</body></html>\r\n">>).


counter(#crary_req{method = Method} = Req) ->
    Count = case Method of
                "GET" -> 0;
                "POST" ->
                    Body = binary_to_list(crary_body:read_all(Req)),
                    [BodyCount, BodyAdd] = string:tokens(Body, "&"),
                    ["count", Num] = string:tokens(BodyCount, "="),
                    ["add", Add] = string:tokens(BodyAdd, "="),
                    list_to_integer(Num) + case Add of
                                               "%3C%3C" -> -1;
                                               "%3E%3E" -> 1
                                           end
            end,
    CountStr = integer_to_list(Count),
    crary:r(Req, ok, [],
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
