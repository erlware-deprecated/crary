-module(aitch).

-include("aitch.hrl").

% application control
-export([start/2, start/3, stop/1, servers/0]).

% misc
-export([code_to_list/1, to_list/1, to_float/1]).
-export([ident/0, ident/1, long_ident/0]).
-export([full_uri/1]).
-export([r/3, r/4, r_error/3, with_chunked_resp/4]).
-export([not_implemented/1, internal_server_error/4, not_found/1, forbidden/1]).
-export([bad_request/1]).
-export([internal_server_error_html/4]).

-define(EOL, <<"\r\n">>).

% TODO-- support long forms of the names

start(TcpPort, Handler) ->    
    start(TcpPort, Handler, []).

% todo: what should this return?
start(TcpPort, Handler, Options) ->
    Args = [TcpPort, Handler, Options], 
    case supervisor:start_child(aitch_sup,
                                {TcpPort, {aitch_port, start_link, Args},
                                 permanent, 5, worker, [aitch_sup]}) of
        {ok, Child} -> Child;
        {error, Reason} -> throw({aitch_error, Reason})
    end.

stop(TcpPort) ->
    ok = supervisor:terminate_child(aitch_sup, TcpPort),
    ok = supervisor:delete_child(aitch_sup, TcpPort).

servers() ->
    lists:map(fun ({Id, _Child, _Type, _Modules}) -> Id end,
              supervisor:which_children(aitch_sup)).

to_list(L) when is_list(L); is_binary(L) ->
    L;
to_list(I) when is_integer(I) ->
    integer_to_list(I).

to_float(F) ->
    float_to_list(F).

ident() ->
    {ok, Vsn} = application:get_key(aitch, vsn),
    [<<"aitch/">>, Vsn].

ident(#aitch_req{opts = Opts}) ->
    ident(Opts);
ident(Opts) ->
    case proplists:get_value(long_ident, Opts, false) of
        true  -> long_ident();
        false -> ident()
    end.

long_ident() ->
    lists:map(fun ({Name, _Desc, Vsn}) ->
                      [atom_to_list(Name), $/, Vsn, $ ]
              end, lists:keysort(1, application:loaded_applications())).

full_uri(#aitch_req{uri = Uri, headers = Headers}) ->
    % todo: configurable hostname fallback for http/1.0
    lists:flatten([<<"http://">>, aitch_headers:get("host", Headers), Uri]).

r(Req, Code, Headers, Body) ->
    Headers2 = aitch_headers:extend(
                 [{<<"content-length">>, integer_to_list(iolist_size(Body))}],
                 Headers),
    r(Req, Code, Headers2),
    aitch_sock:write(Req, Body),
    aitch_sock:done_writing(Req).

r(Req, Code, Headers) ->
    aitch_sock:write_resp_line(Req, Code),
    Headers2 = aitch_headers:extend([{<<"server">>, ident(Req)},
                                     {<<"date">>, aitch_util:rfc1123_date()}],
                                    Headers),
    aitch_headers:write(Req, Headers2).

with_chunked_resp(Req, Code, Headers, F) ->
    try
        Headers2 = case Req of
                      #aitch_req{vsn = Vsn} when Vsn == {1, 0}; Vsn == {0, 9} ->
                           Headers;
                      _ ->
                           aitch_headers:add("transfer-encoding", "chunked",
                                             Headers)
                   end,
        r(Req, Code, Headers2),
        aitch_body:with_writer(Req, F)
    after
        aitch_sock:done_writing(Req)
    end.
            
r_error(Req, Code, Body) ->
    CodeStr = code_to_list(Code),
    r(Req, Code, [{<<"content-type">>, <<"text/html">>}],
                 [<<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 4.0//EN\">
<HTML><HEAD>
<TITLE>">>, CodeStr, <<"</TITLE>
</HEAD><BODY>
<H1>">>, CodeStr, <<"</H1>">>,
Body,
<<"<HR />
<ADDRESS>">>, ident(Req),
              <<" Server at srparish.net Port ">>,
              integer_to_list(aitch_sock:this_port(Req)),
<<"</ADDRESS>
</BODY></HTML>">>]).
     

not_implemented(#aitch_req{uri = URI, method = Method, vsn = Vsn} = Req) ->
    r_error(Req, 501,
            [<<"<P>">>, Method, <<" to ">>, URI, <<" not supported.</P>\n">>,
             <<"<P>Invalid method in request ">>, Method,
             <<" HTTP/">>, aitch_sock:vsn_to_iolist(Vsn), <<"</P>\n">>]).

bad_request(Req) ->
    r_error(Req, 400,
            <<"<p>Your browser sent a request that this server
                   could not understand.</p>">>).

internal_server_error_html(#aitch_req{uri = URI, method = Method} = Req,
			  Class, Reason, Stack) ->
    error_logger:error_report([internal_server_error,
			       {req, Req},
			       {class, Class},
			       {reason, Reason},
			       {stack, Stack}]),
    [<<"Internal error on ">>, Method,
     <<" to ">>, URI, <<": ">>,
     io_lib:format("~p: ~p", [Class, Reason]),
     <<"<P>Stack:<PRE><CODE>">>, io_lib:format("~p", [Stack]),
     <<"</CODE></PRE></P>">>].

internal_server_error(Req, Class, Reason, Stack) ->
    r_error(Req, 500, internal_server_error_html(Req, Class, Reason, Stack)).

not_found(#aitch_req{uri = URI} = Req) ->
    r_error(Req, 404,
	    [<<"<P>The requested URL ">>, URI,
	     <<" was not found on this server.</P>">>]).

forbidden(#aitch_req{uri = URI} = Req) ->
    r_error(Req, 404,
	    [<<"<P>You don't have permission to access ">>, URI,
	     <<" on this server.</P>">>]).

code_to_list(100)                 -> <<"100 Continue">>;
code_to_list(continue)            -> <<"100 Continue">>;
code_to_list(101)                 -> <<"101 Switching Protocols">>;
code_to_list(switching_protocols) -> <<"101 Switching Protocols">>;
code_to_list(200)                 -> <<"200 OK">>;
code_to_list(ok)                  -> <<"200 OK">>;
code_to_list(201)                 -> <<"201 Created">>;
code_to_list(created)             -> <<"201 Created">>;
code_to_list(202)                 -> <<"202 Accepted">>;
code_to_list(accepted)            -> <<"202 Accepted">>;
code_to_list(203)                 -> <<"203 Non-Authoritative Information">>;
code_to_list(non_authoritative_information) ->
                                     <<"203 Non-Authoritative Information">>;
code_to_list(204)                 -> <<"204 No Content">>;
code_to_list(no_content)          -> <<"204 No Content">>;
code_to_list(205)                 -> <<"205 Reset Content">>;
code_to_list(reset_content)       -> <<"205 Reset Content">>;
code_to_list(206)                 -> <<"206 Partial Content">>;
code_to_list(partial_content)     -> <<"206 Partial Content">>;
code_to_list(300)                 -> <<"300 Multiple Choices">>;
code_to_list(multiple_choices)    -> <<"300 Multiple Choices">>;
code_to_list(301)                 -> <<"301 Moved Permanently">>;
code_to_list(moved_permanently)   -> <<"301 Moved Permanently">>;
code_to_list(302)                 -> <<"302 Found">>;
code_to_list(found)               -> <<"302 Found">>;
code_to_list(303)                 -> <<"303 See Other">>;
code_to_list(see_other)           -> <<"303 See Other">>;
code_to_list(304)                 -> <<"304 Not Modified">>;
code_to_list(not_modified)        -> <<"304 Not Modified">>;
code_to_list(305)                 -> <<"305 Use Proxy">>;
code_to_list(use_proxy)           -> <<"305 Use Proxy">>;
code_to_list(307)                 -> <<"307 Temporary Redirect">>;
code_to_list(temporary_redirect)  -> <<"307 Temporary Redirect">>;
code_to_list(400)                 -> <<"400 Bad Request">>;
code_to_list(bad_request)         -> <<"400 Bad Request">>;
code_to_list(401)                 -> <<"401 Unauthorized">>;
code_to_list(unauthorized)        -> <<"401 Unauthorized">>;
code_to_list(402)                 -> <<"402 Payment Required">>;
code_to_list(payment_required)    -> <<"402 Payment Required">>;
code_to_list(403)                 -> <<"403 Forbidden">>;
code_to_list(forbidden)           -> <<"403 Forbidden">>;
code_to_list(404)                 -> <<"404 Not Found">>;
code_to_list(not_found)           -> <<"404 Not Found">>;
code_to_list(405)                 -> <<"405 Method Not Allowed">>;
code_to_list(method_not_allowed)  -> <<"405 Method Not Allowed">>;
code_to_list(406)                 -> <<"406 Not Acceptable">>;
code_to_list(not_acceptable)      -> <<"406 Not Acceptable">>;
code_to_list(407)                 -> <<"407 Proxy Authentication Required">>;
code_to_list(proxy_authentication_required) ->
                                     <<"407 Proxy Authentication Required">>;
code_to_list(408)                 -> <<"408 Request Time-out">>;
code_to_list(request_time_out)    -> <<"408 Request Time-out">>;
code_to_list(409)                 -> <<"409 Conflict">>;
code_to_list(conflict)            -> <<"409 Conflict">>;
code_to_list(410)                 -> <<"410 Gone">>;
code_to_list(gone)                -> <<"410 Gone">>;
code_to_list(411)                 -> <<"411 Length Required">>;
code_to_list(length_required)     -> <<"411 Length Required">>;
code_to_list(412)                 -> <<"412 Precondition Failed">>;
code_to_list(precondition_failed) -> <<"412 Precondition Failed">>;
code_to_list(413)                 -> <<"413 Request Entity Too Large">>;
code_to_list(request_entity_too_large) ->
                                     <<"413 Request Entity Too Large">>;
code_to_list(414)                 -> <<"414 Request-URI Too Large">>;
code_to_list(request_uri_too_large) ->
                                     <<"414 Request-URI Too Large">>;
code_to_list(415)                 -> <<"415 Unsupported Media Type">>;
code_to_list(unsupported_media_type) ->
                                     <<"415 Unsupported Media Type">>;
code_to_list(416)                 -> <<"416 Requested range not satisfiable">>;
code_to_list(requested_range_not_satisfiable) ->
                                     <<"416 Requested range not satisfiable">>;
code_to_list(417)                 -> <<"417 Expectation Failed">>;
code_to_list(expectation_failed)  -> <<"417  Expectation Failed">>;
code_to_list(500)                 -> <<"500 Internal Server Error">>;
code_to_list(internal_server_error) ->
                                     <<"500 Internal Server Error">>;
code_to_list(501)                 -> <<"501 Not Implemented">>;
code_to_list(not_implemented)     -> <<"501 Not Implemented">>;
code_to_list(502)                 -> <<"502 Bad Gateway">>;
code_to_list(bad_gateway)         -> <<"502 Bad Gateway">>;
code_to_list(503)                 -> <<"503 Service Unavailable">>;
code_to_list(service_unavailable) -> <<"503 Service Unavailable">>;
code_to_list(504)                 -> <<"504 Gateway Time-out">>;
code_to_list(gateway_time_out)    -> <<"504 Gateway Time-out">>;
code_to_list(505)                 -> <<"505 HTTP Version not supported">>;
code_to_list(http_version_not_supported) ->
                                     <<"505 HTTP Version not supported">>;
code_to_list(Code) ->
    Code.
