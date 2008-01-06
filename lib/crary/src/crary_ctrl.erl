-module(crary_ctrl).

-export([start_link/3]).

-export([call_handler/2]).

-include("crary.hrl").

-define(OPT_DEFAULTS,
        [{keep_alive_timeout, 30000}, % in milliseconds
         {keep_alive_max_requests, infinity},
% todo: uncomment:
%         {read_timeout, 30000}, % in milliseconds
         {read_timeout, 5000}, % in milliseconds
         {write_timeout, 30000}, % in milliseconds
         {max_body_size, 10 * 1024 * 1024}, % in bytes
         {max_header_size, 10 * 1024 * 1024} % in bytes
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
    Req1 = Req0#crary_req{headers = crary_headers:new(S, Opts)},
    validate_vsn11_has_host(Req1),
    Req = make_resp_s(Req1),
    case keep_alive_p(Req, NReqs) of
        true ->
            crary_util:spawn_link(fun () -> call_handler(Req, Handler) end),
            ctrl_loop(S, Handler, Opts, dec_keep_alive_requests(NReqs));
        false ->
            call_handler(Req, Handler),
            crary_sock:close(S)
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
            crary_sock:close(Req),
            exit(normal);
        {crary_sock, eof} ->
            crary_sock:close_reader(Req),
            exit(normal)
    end.


call_handler(Req, {M, F, Args}) ->
    try	apply(M, F, [Req | Args])
    catch
	C:R ->
	    crary:internal_server_error(Req, C, R, erlang:get_stacktrace())
    end.


dec_keep_alive_requests(infinity) -> infinity;
dec_keep_alive_requests(N) -> N - 1.

keep_alive_p(_Req, 1) -> false;
keep_alive_p(#crary_req{vsn = {0, 9}}, _) -> false;
keep_alive_p(#crary_req{vsn = {1, 0}, headers = Headers}, _) ->
    case string:to_lower(crary_headers:get("connection", Headers, "none")) of
	"none" -> false;
	"keep-alive" -> true;
	C ->
	    error_logger:warning_msg(
	      "unknown_connection_header '~p' for HTTP/1.0~n", [C]),
	    false
    end;
keep_alive_p(#crary_req{vsn = {1, 1}, headers = Headers}, _) ->
    case string:to_lower(crary_headers:get("connection", Headers, "none")) of
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

validate_vsn11_has_host(#crary_req{vsn = {1, 1},
                                   headers = Headers} = Req) ->
    case crary_headers:has("host", Headers) of
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
