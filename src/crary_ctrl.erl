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
%%% @copyright 2007, 2008 Scott Parish <srp@srparish.net>-module(crary_ctrl).
-module(crary_ctrl).

-export([start_link/3]).

-export([call_handler/2]).

-include("crary.hrl").
-include("uri.hrl").

-define(OPT_DEFAULTS,
        [{keep_alive_timeout, 30000}, % in milliseconds
         {keep_alive_max_requests, infinity},
         {read_timeout, 30000}, % in milliseconds
         {write_timeout, 30000}, % in milliseconds
         {max_body_size, 10 * 1024 * 1024}, % in bytes
         {max_header_size, 1 * 1024 * 1024} % in bytes
        ]).

start_link(S, Handler, Opts) ->
    crary_util:spawn_link(fun () ->
                                  ctrl(S, Handler, Opts ++ ?OPT_DEFAULTS)
                          end).

ctrl(S, Handler, Opts) ->
    try
        ctrl_loop(S, Handler, Opts,
                  proplists:get_value(keep_alive_max_requests, Opts))
    catch
        {crary_sock, timeout} ->
            crary_sock:close_reader(S),
            exit(normal)
    end.

ctrl_loop(S, Handler, Opts, NReqs) ->
    Req0 = read_req_line(#crary_req{sock = S, opts = Opts}),
    Req1 = Req0#crary_req{headers = crary_headers:from_sock(Req0)},
    validate_vsn11_has_host(Req1),
    Req2 = setup_uri(Req1),
    Req = make_resp_s(Req2),
    case keep_alive_p(Req, NReqs) of
        true ->
            crary_util:spawn_link(fun () -> call_handler(Req, Handler) end),
            ctrl_loop(S, Handler, Opts, dec_keep_alive_requests(NReqs));
        false ->
            call_handler(Req, Handler),
            crary_sock:close_reader(S),
            exit(normal)
        end.

make_resp_s(Req) ->
    Req#crary_req{sock = crary_sock:new_resp(
                           Req, case crary_body:has_body(Req) of
                                    true  -> rw;
                                    false -> wo
                                end)}.

read_req_line(Req) ->
    try crary_sock:read_req_line(Req) of
        {Method, Uri, Vsn} ->
            Req#crary_req{method = Method, uri = Uri, vsn = Vsn}
    catch
        {crary_sock, parse_error} ->
            crary:bad_request(Req),
            crary_sock:close_reader(Req),
            exit(normal);
        {crary_sock, eof} ->
            crary_sock:close_reader(Req),
            exit(normal)
    end.

%% todo: configurable hostname fallback for http/1.0
setup_uri(#crary_req{uri = "http" ++ _ = Uri} = Req) ->
    Req#crary_req{uri = uri:from_string(Uri)};
setup_uri(#crary_req{uri = Uri} = Req) ->
    UriRec = uri:from_http_1_1("http", crary_headers:get("host", Req), Uri),
    Req#crary_req{uri = UriRec}.

call_handler(#crary_req{uri = #uri{path = Path}} = Req, Handler) ->
    try
        case Handler of
            {M, F, Args}          -> apply(M, F, [Req, Path | Args]);
            F when is_function(F) -> F(Req, Path);
            Unknown ->
                throw({crary_ctrl_error, {unknown_handler_type, Unknown}})
        end
    catch
        throw:{resp, Code, Headers, BodyOrF} ->
            crary:r(Req, Code, Headers, BodyOrF);
        throw:{resp_error, Code, Msg} ->
            crary:error(Req, Code, Msg);
        throw:{resp_error, Code, Headers, Msg} ->
            crary:error(Req, Code, Headers, Msg);
        throw:R when R == not_implemented;
                     R == bad_request;
                     R == not_found;
                     R == forbidden ->
            crary:R(Req);
        C:R ->
            crary:internal_server_error(Req, C, R, erlang:get_stacktrace())
    end.

dec_keep_alive_requests(infinity) -> infinity;
dec_keep_alive_requests(N) -> N - 1.

keep_alive_p(_Req, 1) -> false;
keep_alive_p(#crary_req{vsn = {0, 9}}, _) -> false;
keep_alive_p(#crary_req{vsn = {1, 0}} = Req, _) ->
    case connection_keep_alive_header(Req) of
        "none" -> false;
        "keep-alive" -> true;
        C ->
            error_logger:warning_msg(
              "unknown_connection_header '~p' for HTTP/1.0~n", [C]),
            false
    end;
keep_alive_p(#crary_req{vsn = {1, 1}} = Req, _) ->
    case connection_keep_alive_header(Req) of
        "none" -> true;
        "close" -> false;
        "keep-alive" -> true;
        C ->
            error_logger:warning_msg(
              "unknown_connection_header '~p' for HTTP/1.1~n", [C]),
            false
    end;
keep_alive_p(#crary_req{vsn = Vsn}, _) ->
    error_logger:warning_msg("unknown_http_version '~p'~n",
                             [crary_sock:vsn_to_list(Vsn)]),
    false.

connection_keep_alive_header(Req) ->
    Val = string:to_lower(crary_headers:get("connection", Req, "none")),
    connection_keep_alive_token(string:tokens(Val, ", ")).

connection_keep_alive_token([T | _Ts]) when T == "keep-alive"; T == "close" ->
    T;
connection_keep_alive_token([_ | Ts]) ->
    connection_keep_alive_token(Ts);
connection_keep_alive_token([]) ->
    "none".

validate_vsn11_has_host(#crary_req{vsn = {1, 1}} = Req) ->
    case crary_headers:has("host", Req) of
        true ->
            ok;
        false ->
            crary:r_error(Req, 400, <<"<p>HTTP 1.1 requests must include the
                                          <i>host</i> header</p>.">>),
            crary_sock:close(Req),
            exit(normal)
    end;
validate_vsn11_has_host(_) ->
    ok.
