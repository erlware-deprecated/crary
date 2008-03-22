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
%%% @doc This module provides the core crary apis.

-module(crary).

-include("uri.hrl").
-include("crary.hrl").

% application control
-export([start/2, start/3, stop/1, servers/0]).

% misc
% TODO-- support long forms of the names
-export([list_to_vsn/1, vsn_to_iolist/1]).
-export([code_to_binary/1]).
-export([ident/0, ident/1, long_ident/0]).
-export([pp/1]).
-export([r/3, resp/3, r/4, resp/4, r_error/3, error/3]).
-export([not_implemented/1, internal_server_error/4, not_found/1, forbidden/1]).
-export([bad_request/1]).
-export([internal_server_error_html/4]).

-define(EOL, <<"\r\n">>).

%%% @type crary_req() = record().
%%%       This is a record that represents the specifics of the
%%%       request currently being handled. It has the following
%%%       fields:
%%%       <dl>
%%%        <dt>vsn::{@link vsn()}</dt>
%%%        <dd><p>The client HTTP Version.</p>
%%%            <p>Example: `{1, 1}'</p></dd>
%%%
%%%        <dt>method::string()</dt>
%%%        <dd><p>The request method.</p>
%%%            <p>Example: `"GET"'</p></dd>
%%%
%%%        <dt>uri::string()</dt>
%%%        <dd><p>The request uri. Crary does not decode these, nor does it
%%%            intend to ever do so; it may be only the path/query parts
%%%            or it may be the absolute URI (see rfc2616, section 5.1.2).</p>
%%%            <p>Examples: `"/index.html?sort=date"',
%%%                         `"http://myhost.com:80/path/"'</p></dd>
%%%
%%%        <dt>headers::{@link crary_headers:headers()}</dt>
%%%        <dd><p>The request headers. Use {@link crary_headers} to access the
%%%            contents of this data structure</p></dd>
%%%
%%%        <dt>sock::{@link crary_sock:sock()}</dt>
%%%        <dd><p>A descriptor for reading the body or writing the response via
%%%            {@link crary_sock}. Most of the {@link crary_sock} functions
%%%            accept this {@link crary_req()} record, so it should be rarely
%%%            necessary to access this directly.</p></dd>
%%%
%%%        <dt>opts::{@link proplist()}</dt>
%%%        <dd><p>The options that this server (the one for this port)
%%%            was started with. Use the erlang `proplists' module to
%%%            access values. Feel free to add whatever options your
%%%            handler needs to this data structure. Please name your
%%%            options so that they won't conflict with other handler's
%%%            options.</p></dd>
%%%       </dl>
%%%       Please `-include("crary.hrl").' in your source files that
%%%       need to interact with this record.
%%% @end

%%%
%%% @type code() = integer() | atom() | binary().
%%%       For example: `404' or `not_found' or `<<"404 Not Found">>'
%%% @type proplist() = [Key::atom() | {Key::atom(), Value::term}]
%%% @type mfa() = {Module::atom(), Function::atom(), Args::list()} | function()
%%% @type vsn() = {Major, Minor}
%%%       Major = integer()
%%%       Minor = integer()


%%%====================================================================
%%% Server Control APIs
%%%====================================================================

%% @doc Start a crary server listening on `TcpPort'.
%% @spec start(integer(), mfa() | {function(), Args}) -> pid()
%% @see start/3
start(TcpPort, Handler) ->
    start(TcpPort, Handler, []).

%% @doc Start a crary server listening on `TcpPort'. `Handler' will be
%% called as `apply(M, F, [Req | Args])' for each request.
%% @spec start(integer(), mfa() | {function(), Args}, proplist()) -> pid()
start(TcpPort, Handler, Options) ->
    Args = [TcpPort, Handler, Options],
    case supervisor:start_child(crary_sup,
                                {TcpPort, {crary_port, start_link, Args},
                                 permanent, 5, worker, [crary_sup]}) of
        {ok, Child} -> Child;
        {error, Reason} -> throw({crary_error, Reason})
    end.

%% @doc Stop the crary server that's running on `TcpPort'.
%% @spec stop(integer()) -> ok
stop(TcpPort) ->
    ok = supervisor:terminate_child(crary_sup, TcpPort),
    ok = supervisor:delete_child(crary_sup, TcpPort).

%% @doc Return a list of crary servers (as ports) currently running.
%% @spec servers() -> [TcpPort::integer()]
servers() ->
    lists:map(fun ({Id, _Child, _Type, _Modules}) -> Id end,
              supervisor:which_children(crary_sup)).

%%%====================================================================
%%% APIs for dealing with HTTP type data
%%%====================================================================

%% @doc Parse the HTTP version string into a version tuple of `{Maj, Min}'.
%% @spec list_to_vsn(string()) -> vsn()
list_to_vsn(VsnStr) ->
    {Maj, [$. | MinS]} = string:to_integer(VsnStr),
    {Maj, list_to_integer(MinS)}.

%% @doc Convert a {@link vsn()} tuple to a string.
%% @spec vsn_to_iolist(vsn()) -> string()
vsn_to_iolist(#crary_req{vsn = Vsn}) ->
    vsn_to_iolist(Vsn);
vsn_to_iolist({Maj, Min}) ->
    [integer_to_list(Maj), $., integer_to_list(Min)].

%% @doc Return an iolist of the short ident string such as: `crary/1.0.5'.
%% @spec ident() -> iolist()
ident() ->
    {ok, Vsn} = application:get_key(crary, vsn),
    [<<"crary/">>, Vsn].

%% @doc Return an iolist of the ident, using `Opts' to determin if the long
%% or short ident string should be used.
%% @spec ident(proplist()) -> iolist()
%% @see ident/0
%% @see long_ident/0
ident(#crary_req{opts = Opts}) ->
    ident(Opts);
ident(Opts) ->
    case proplists:get_value(long_ident, Opts, false) of
        true  -> long_ident();
        false -> ident()
    end.

%% @doc Return an iolist of the longer ident string, which will list
%% all the loaded applications, for instance:
%%   ``crary/1.0.5 kernel/2.11.5 stdlib/1.14.5 sasl/2.1.5.1''
%% @spec long_ident() -> iolist()
long_ident() ->
    lists:map(fun ({Name, _Desc, Vsn}) ->
                      [atom_to_list(Name), $/, Vsn, $ ]
              end, lists:keysort(1, application:loaded_applications())).

%% @doc `Pretty print' the request: return a tuple list representing
%% the crary_req structure in a form that will print nicely via
%% {@link io:format/2} `~p' or {@link error_logger:error_report/1}.
%% @spec pp(crary_req()) -> list()
pp(#crary_req{} = Req) ->
    [{method, Req#crary_req.method},
     {uri, Req#crary_req.uri},
     {vsn, Req#crary_req.vsn},
     {headers, crary_headers:to_list(Req)},
     {sock, Req#crary_req.sock},
     {opts, Req#crary_req.opts}].


%%%====================================================================
%%% APIs for forming responses
%%%====================================================================

%% @doc Write a response line and response headers to socket and
%% either write the body, or start a streamed body call `F(Writer)' to
%% generate the body. In both cases {@link crary_sock:done_writing/1}
%% is called before returning.
%%
%% Static Example:
%% ```hello_handler(Req) ->
%%        crary:r(Req, ok, [{"content-type", "text/html"}],
%%                <<"<html><body>Hello World!</body></html>">>).
%% '''
%%
%% Streamed Example:
%% ```hello_handler(Req) ->
%%        crary:r(Req, ok, [{"content-type", "text/html"}],
%%                fun (W) ->
%%                           crary_body:write(W, "<html><body>"),
%%                           crary_body:write(W, "Hello World!"),
%%                           crary_body:write(W, "</body></html>"),
%%                end).
%% '''
%%
%% You can get the same effect by throwing the tuple:
%% ```throw({resp, Code, Headers, BodyOrF})'''
%%
%% @spec r(crary_req(), code(), crary_headers:headers(), BodyOrF) -> ok
%%       BodyOrF = Body | F
%%       Body = iolist()
%%       F = function()
%% @see crary_body:with_writer/2
r(Req, Code, Headers, F) when is_function(F) ->
    try
        Headers2 = case Req of
                      #crary_req{vsn = Vsn} when Vsn == {1, 0}; Vsn == {0, 9} ->
                           Headers;
                      _ ->
                           crary_headers:add("transfer-encoding", "chunked",
                                             Headers)
                   end,
        r(Req, Code, Headers2),
        crary_body:with_writer(Req, F)
    after
        crary_sock:done_writing(Req)
    end;
r(Req, Code, Headers, Body) when is_list(Body); is_binary(Body) ->
    Headers2 = crary_headers:extend(
                 [{<<"content-length">>, integer_to_list(iolist_size(Body))}],
                 Headers),
    r(Req, Code, Headers2),
    crary_sock:write(Req, Body),
    crary_sock:done_writing(Req).

%% @doc alias for {@link r/4}
%% @see r/4
resp(Req, Code, Headers, BodyOrF) ->
    r(Req, Code, Headers, BodyOrF).

%% @doc Write a response line and response headers to socket, does not
%% write the body, nor does it call {@link crary_sock:done_writing/1}. It
%% adds `Server' and `Date' headers to the response. Be sure to set the
%% headers appropriately for the form of the body (ei `Content-Length'
%% or `Transfer-Encoding').
%%
%% Example:
%% ```hello_handler(Req) ->
%%        Body = "<html><body>Hello World!</body></html>",
%%        BodyLenStr = integer_to_list(iolist_size(Body)),
%%        crary:r(Req, ok, [{"content-type", "text/html"},
%%                          {<<"content-length">>, BodyLenStr}],
%%        crary_sock:write(Req, Body),
%%        crary_sock:done_writing(Req).
%% '''
%%
%% If you need to write a body or do body streaming, {@link r/4} may
%% be a more convient function then this one.
%%
%% @spec r(crary_req(), code(), crary_headers:headers()) -> ok
r(Req, Code, Headers) ->
    crary_sock:write_resp_line(Req, Code),
    Headers2 = crary_headers:extend([{<<"server">>, ident(Req)},
                                     {<<"date">>, crary_util:rfc1123_date()}],
                                    Headers),
    crary_headers:write(Req, Headers2).

%% @doc Alias for {@link r/3}
%% @see r/3
resp(Req, Code, Headers) ->
    r(Req, Code, Headers).

%% @doc Write a response for errors, this includes the standard error
%% header and footer html. The `title' and `h1' are generated from the
%% `Code'. `Msg' should be verbage describing the problem.
%%
%% Example:
%% ```ro_handler(crary_req{method = 'GET'} = Req) ->
%%               crary:r(Req, ok, ["content-type", "text/plain"],
%%                       "Hello World!");
%%    ro_handler(crary_req{method = Method} = Req) when Method =/= 'GET' ->
%%        crary:r_error(Req, not_implemented,
%%                      ["The method `", Method,
%%                       "' is not supported by this server.",
%%                       "Please only use 'GET' with this server"]).
%% '''
%%
%% You can get the same effect by throwing the tuple:
%% ```throw({resp_error, Code, Msg})'''
%%
%% @spec r_error(crary_req(), code(), iolist()) -> ok
r_error(Req, Code, Msg) ->
    CodeStr = code_to_binary(Code),
    r(Req, Code, [{<<"content-type">>, <<"text/html">>}],
      [<<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 4.0//EN\">
<HTML><HEAD>
<TITLE>">>, CodeStr, <<"</TITLE>
</HEAD><BODY>
<H1>">>, CodeStr, <<"</H1>">>,
Msg,
<<"<HR />
<ADDRESS>">>, ident(Req),
              <<" Server at srparish.net Port ">>,
              integer_to_list(crary_sock:this_port(Req)),
<<"</ADDRESS>
</BODY></HTML>">>]).

%% @see r_error/3
error(Req, Code, Msg) ->
    r_error(Req, Code, Msg).

%% @doc This is a short cut for sending 501, `Not Implemented', error
%% responses with the body already filled in.
%%
%% Example:
%% ```ro_handler(crary_req{method = 'GET'} = Req) ->
%%        crary:r(Req, ok, ["content-type", "text/plain"], "Hello World!");
%%    ro_handler(Req) -> % method =/= 'GET'
%%        crary:not_implemented(Req).
%% '''
%%
%% You can get the same effect by throwing the tuple:
%% ```throw(not_implemented)'''
%%
%% @spec not_implemented(crary_req()) -> ok
not_implemented(#crary_req{uri = Uri, method = Method, vsn = Vsn} = Req) ->
    r_error(Req, 501,
            [<<"<P>">>, Method, <<" to ">>,
             Uri#uri.raw, <<" not implemented.</P>\n">>,
             <<"<P>Invalid method in request ">>, Method,
             <<" HTTP/">>, vsn_to_iolist(Vsn), <<"</P>\n">>]).

%% @doc This is a short cut for sending 400, `Bad Request', error responses
%% with the body already filled in.
%%
%% You can get the same effect by throwing the tuple:
%% ```throw(bad_request)'''
%%
%% @spec bad_request(crary_req()) -> ok
bad_request(Req) ->
    r_error(Req, 400,
            <<"<p>Your browser sent a request that this server
                   could not understand.</p>">>).

%% @doc This generates the HTML that is used for 500, `Internal Server
%% Error'. {@link crary_body:with_writer/2} uses this to display error
%% messages; it can't call internal_server_error/4 as the response
%% headers will have already been sent.
%% @spec internal_server_error_html(crary_req(), atom(), term(), list()) ->
%%           iolist()
internal_server_error_html(#crary_req{uri = Uri, method = Method} = Req,
                          Class, Reason, Stack) ->
    error_logger:error_report([internal_server_error,
                               {req, pp(Req)},
                               {class, Class},
                               {reason, Reason},
                               {stack, Stack}]),
    [<<"Internal error on ">>, Method,
     <<" to ">>, Uri#uri.raw, <<": ">>,
     io_lib:format("~p: ~p", [Class, Reason]),
     <<"<P>Stack:<PRE><CODE>">>, io_lib:format("~p", [Stack]),
     <<"</CODE></PRE></P>">>].

%% @doc This is a short cut for sending 500, `Internal Server Error',
%% error responses with the body already filled in.
%%
%% {@link crary_ctrl} before calling the handler, places a try/catch
%% on the stack that will automatically call this function if there is
%% an uncaught exception, so you may not need to use this directly.
%%
%% @spec internal_server_error(crary_req(), atom(), term(), list()) -> ok
internal_server_error(Req, Class, Reason, Stack) ->
    r_error(Req, 500, internal_server_error_html(Req, Class, Reason, Stack)).

%% @doc This is a short cut for sending 404, `Not Found' error
%% responses with the body already filled in.
%%
%% Example:
%% ```handler(crary_req{uri = '/'} = Req) ->
%%        crary:r(Req, ok, ["content-type", "text/plain"], "Hello World!");
%%    handler(Req) -> % unknown uri
%%        crary:not_found(Uri).
%% '''
%%
%% You can get the same effect by throwing the tuple:
%% ```throw(not_found)'''
%%
%% @spec not_found(crary_req()) -> ok
not_found(#crary_req{uri = Uri} = Req) ->
    r_error(Req, 404,
            [<<"<P>The requested URL ">>, Uri#uri.raw,
             <<" was not found on this server.</P>">>]).

%% @doc This is a short cut for sending 403, `Forbidden' error
%% responses with the body already filled in.
%%
%% You can get the same effect by throwing the tuple:
%% ```throw(forbidden)'''
%%
%% @spec forbidden(crary_req()) -> ok
forbidden(#crary_req{uri = Uri} = Req) ->
    r_error(Req, 403,
            [<<"<P>You don't have permission to access ">>, Uri#uri.raw,
             <<" on this server.</P>">>]).

%% @doc Given a number or atom of a standard HTTP response code, return
%% a binary (string) of the number and name.
%%
%% Example:
%% ```code_to_binary(404) => <<"404 Not Found">>
%%    code_to_binary(not_found) => <<"404 Not Found">>
%% '''
%%
%% This function is used by functions such as {@link r/4}, {@link
%% with_chunked_resp/4}, and {@link r_error/3} so that the `Code'
%% argument can be minimally specified.
%%
%% @spec code_to_binary(integer() | atom() | binary()) -> binary()
code_to_binary(100)                 -> <<"100 Continue">>;
code_to_binary(continue)            -> <<"100 Continue">>;
code_to_binary(101)                 -> <<"101 Switching Protocols">>;
code_to_binary(switching_protocols) -> <<"101 Switching Protocols">>;
code_to_binary(200)                 -> <<"200 OK">>;
code_to_binary(ok)                  -> <<"200 OK">>;
code_to_binary(201)                 -> <<"201 Created">>;
code_to_binary(created)             -> <<"201 Created">>;
code_to_binary(202)                 -> <<"202 Accepted">>;
code_to_binary(accepted)            -> <<"202 Accepted">>;
code_to_binary(203)                 -> <<"203 Non-Authoritative Information">>;
code_to_binary(non_authoritative_information) ->
                                       <<"203 Non-Authoritative Information">>;
code_to_binary(204)                 -> <<"204 No Content">>;
code_to_binary(no_content)          -> <<"204 No Content">>;
code_to_binary(205)                 -> <<"205 Reset Content">>;
code_to_binary(reset_content)       -> <<"205 Reset Content">>;
code_to_binary(206)                 -> <<"206 Partial Content">>;
code_to_binary(partial_content)     -> <<"206 Partial Content">>;
code_to_binary(300)                 -> <<"300 Multiple Choices">>;
code_to_binary(multiple_choices)    -> <<"300 Multiple Choices">>;
code_to_binary(301)                 -> <<"301 Moved Permanently">>;
code_to_binary(moved_permanently)   -> <<"301 Moved Permanently">>;
code_to_binary(302)                 -> <<"302 Found">>;
code_to_binary(found)               -> <<"302 Found">>;
code_to_binary(303)                 -> <<"303 See Other">>;
code_to_binary(see_other)           -> <<"303 See Other">>;
code_to_binary(304)                 -> <<"304 Not Modified">>;
code_to_binary(not_modified)        -> <<"304 Not Modified">>;
code_to_binary(305)                 -> <<"305 Use Proxy">>;
code_to_binary(use_proxy)           -> <<"305 Use Proxy">>;
code_to_binary(307)                 -> <<"307 Temporary Redirect">>;
code_to_binary(temporary_redirect)  -> <<"307 Temporary Redirect">>;
code_to_binary(400)                 -> <<"400 Bad Request">>;
code_to_binary(bad_request)         -> <<"400 Bad Request">>;
code_to_binary(401)                 -> <<"401 Unauthorized">>;
code_to_binary(unauthorized)        -> <<"401 Unauthorized">>;
code_to_binary(402)                 -> <<"402 Payment Required">>;
code_to_binary(payment_required)    -> <<"402 Payment Required">>;
code_to_binary(403)                 -> <<"403 Forbidden">>;
code_to_binary(forbidden)           -> <<"403 Forbidden">>;
code_to_binary(404)                 -> <<"404 Not Found">>;
code_to_binary(not_found)           -> <<"404 Not Found">>;
code_to_binary(405)                 -> <<"405 Method Not Allowed">>;
code_to_binary(method_not_allowed)  -> <<"405 Method Not Allowed">>;
code_to_binary(406)                 -> <<"406 Not Acceptable">>;
code_to_binary(not_acceptable)      -> <<"406 Not Acceptable">>;
code_to_binary(407)                 -> <<"407 Proxy Authentication Required">>;
code_to_binary(proxy_authentication_required) ->
                                       <<"407 Proxy Authentication Required">>;
code_to_binary(408)                 -> <<"408 Request Time-out">>;
code_to_binary(request_time_out)    -> <<"408 Request Time-out">>;
code_to_binary(409)                 -> <<"409 Conflict">>;
code_to_binary(conflict)            -> <<"409 Conflict">>;
code_to_binary(410)                 -> <<"410 Gone">>;
code_to_binary(gone)                -> <<"410 Gone">>;
code_to_binary(411)                 -> <<"411 Length Required">>;
code_to_binary(length_required)     -> <<"411 Length Required">>;
code_to_binary(412)                 -> <<"412 Precondition Failed">>;
code_to_binary(precondition_failed) -> <<"412 Precondition Failed">>;
code_to_binary(413)                 -> <<"413 Request Entity Too Large">>;
code_to_binary(request_entity_too_large) ->
                                       <<"413 Request Entity Too Large">>;
code_to_binary(414)                 -> <<"414 Request-URI Too Large">>;
code_to_binary(request_uri_too_large) ->
                                       <<"414 Request-URI Too Large">>;
code_to_binary(415)                 -> <<"415 Unsupported Media Type">>;
code_to_binary(unsupported_media_type) ->
                                       <<"415 Unsupported Media Type">>;
code_to_binary(416)                 -> <<"416 Requested range not satisfiable">>;
code_to_binary(requested_range_not_satisfiable) ->
                                       <<"416 Requested range not",
                                         " satisfiable">>;
code_to_binary(417)                 -> <<"417 Expectation Failed">>;
code_to_binary(expectation_failed)  -> <<"417  Expectation Failed">>;
code_to_binary(500)                 -> <<"500 Internal Server Error">>;
code_to_binary(internal_server_error) ->
                                       <<"500 Internal Server Error">>;
code_to_binary(501)                 -> <<"501 Not Implemented">>;
code_to_binary(not_implemented)     -> <<"501 Not Implemented">>;
code_to_binary(502)                 -> <<"502 Bad Gateway">>;
code_to_binary(bad_gateway)         -> <<"502 Bad Gateway">>;
code_to_binary(503)                 -> <<"503 Service Unavailable">>;
code_to_binary(service_unavailable) -> <<"503 Service Unavailable">>;
code_to_binary(504)                 -> <<"504 Gateway Time-out">>;
code_to_binary(gateway_time_out)    -> <<"504 Gateway Time-out">>;
code_to_binary(505)                 -> <<"505 HTTP Version not supported">>;
code_to_binary(http_version_not_supported) ->
                                       <<"505 HTTP Version not supported">>;
code_to_binary(Code) ->
    Code.
