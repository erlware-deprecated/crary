-module(example).
-export([hello_world/1]).
-export([counter/1]).

% code:add_path("_build/development/apps/crary-0.1.0/ebin").
% code:add_path("_build/development/apps/crary_dir_listing-0.1.0/ebin").
% code:add_path("_build/development/apps/uri-0.1.0/ebin").
% application:start(uri).
% application:start(sasl).
% application:start(crary).
% crary:start(8001, {example, hello_world, []}).
% crary:start(8002, {example, counter, []}).
% crary:start(8003, {crary_dir_listing, handler, ["/Users/srp/tmp"]}).

-include("crary.hrl").
-include("uri.hrl").

hello_world(#crary_req{uri = #uri{path = "/404"}} = Req) ->
    crary:not_found(Req);
hello_world(#crary_req{uri = #uri{path = "/crash"}} = _Req) ->
    exit(oops);
hello_world(#crary_req{uri = #uri{path = "/chunk"}} = Req) ->
    crary:r(Req, 200, [{<<"transfer-encoding">>, <<"chunked">>}]),
    W = crary_body:new_writer(Req),
    crary_body:write(W, <<"<html><body>">>),
    crary_body:write(W, <<"hello ">>),
    crary_body:write(W, <<"world!">>),
    crary_body:write(W, <<"</html></body>">>),
    crary_body:done_writing(W),
    crary_sock:done_writing(Req);
hello_world(Req) ->
    io:format("uri: ~p", [Req#crary_req.uri]),
    io:format("path: ~p", [(Req#crary_req.uri)#uri.path]),
    crary:r(Req, ok, [], <<"<html><body>Hello World!</body></html>\r\n">>).


counter(#crary_req{method = Method} = Req) ->
    Count = case Method of
                "GET" -> 0;
                "POST" ->
                    Q = uri:query_to_dict(crary_body:read_all(Req)),
                    list_to_integer(dict:fetch("count", Q)) +
                        list_to_integer(dict:fetch("add", Q))
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
                   <input type=\"submit\" name=\"add\" value=\"-100\">
                   <input type=\"submit\" name=\"add\" value=\"-1\">
                   ">>, CountStr, <<"
                   <input type=\"submit\" name=\"add\" value=\"+1\">
                   <input type=\"submit\" name=\"add\" value=\"+100\">
                  </form>
                 </body>
                </html>">>]).
