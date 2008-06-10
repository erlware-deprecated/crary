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
-export([start/2, start/3, start/4, stop/1, stop/2, servers/0]).
-export([opts/1, opts/2, opts/3, handler/1, handler/2, handler/3]).

% misc
% TODO-- support long forms of the names
-export([list_to_vsn/1, vsn_to_iolist/1]).
-export([code_to_binary/1]).
-export([ident/0, ident/1, long_ident/0]).
-export([pp/1]).
-export([r/3, resp/3, r/4, resp/4]).
-export([error_resp/3, error/3, error_resp/4, error/4]).
-export([not_implemented/1, internal_server_error/4, not_found/1, forbidden/1]).
-export([bad_request/1]).
-export([internal_server_error_html/4]).

-export([vsn/1, method/1, uri/1, headers/1, sock/1]).

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
%%%        <dt>uri::{@link uri:uri()}</dt>
%%%        <dd><p>The parsed uri structure.</p></dd>
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
%%%        <dt>opts::{@link opts()}</dt>
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

%%% @type handler() = mfa() | function().
%%%
%%%      Crary reads in a request and headers and parses them, but
%%%      most of the rest is left up to the handler. A handler is a
%%%      callback which is passed the {@link crary_req()} record;
%%%      ultimately the job of the handler is to:
%%%
%%%      <ul>
%%%       <li> If applicable, read the request body and call {@link
%%%            crary_sock:done_reading/1}. Since crary supports
%%%            connection pipe-lining, the sooner {@link
%%%            crary_sock:done_reading/1} is called, the sooner
%%%            the next request can be started.</li>
%%%       <li> Write an HTTP response </li>
%%%       <li> Call {@link crary_sock:done_writing/1}</li>
%%%      </ul>
%%%
%%%      The handler is usually a process created to handle a single
%%%      request in a connection/pipeline of requests. However, this
%%%      is not always the case, so you should always let the handler
%%%      return (ie don't re-use the process to do long-lived
%%%      non-connection related things).
%%%
%%%      A handler should only use {@link crary_sock:read/3} and
%%%      similar or derived functions for consuming data from the
%%%      connection.
%%%
%%%      While the handler is free to do pretty much anything, crary
%%%      provides quite a few functions to make commonly done things
%%%      easy. For instance, {@link crary} supplies methods such as
%%%      {@link r/4}, {@link error/4}, and {@link not_found/1}. As a
%%%      convenience many of these can also be triggered by throwing an
%%%      appropriate expression. For more information on what to throw
%%%      see the documentation for the various functions in this
%%%      module.
%%%
%%%      If your handler throws an exception that wasn't of one of the
%%%      anticipated types, an ``internal service error'' response
%%%      will automatically be generated.
%%%
%%%      The handler returned value is completely ignored.
%%%
%%% @end

%%% @type code() = integer() | atom() | binary().
%%%       For example: `404' or `not_found' or `<<"404 Not Found">>'
%%% @type opts() = [Key::atom() | {Key::atom(), Value::term}].
%%%       The options are passed in when the server is started and can be
%%%       used to configure the TCP socket, crary internals, as well as
%%%       handlers.
%%%
%%%       The option keys that crary currently recognizes are:
%%%       <dl>
%%%        <dt>socket_opts</dt>
%%%        <dd>Options passed to gen_tcp when opening the socket. See
%%%            {@link inet:setopts/2} for available options. The following
%%%            options can not be specified as it would break crary internal
%%%            assumptions: list, active, exit_on_close, header, packet,
%%%            packet_size</dd>
%%%
%%%        <dt>keep_alive_timeout</dt>
%%%        <dd>The number of milliseconds (or 'infinity') to wait
%%%            between requests on a single TCP connection.<br />
%%%            Default: 30s</dd>
%%%
%%%        <dt>keep_alive_max_requests</dt>
%%%        <dd>The number of requests allowed on a single TCP connection before
%%%            forcing it to be closed.<br />
%%%            Default: infinity</dd>
%%%
%%%        <dt>read_timeout</dt>
%%%        <dd>The longest amount of time in milliseconds (or 'infinity')
%%%            to block for a read before closing the connection and
%%%            throwing a timeout error.<br />
%%%            Default: 30s</dd>
%%%
%%%        <dt>write_timeout</dt>
%%%        <dd>The longest amount of time in milliseconds (or 'infinity')
%%%            to block for a write before closing the connection and
%%%            throwing a timeout error.<br />
%%%            Default: 30s</dd>
%%%
%%%        <dt>max_body_size</dt>
%%%        <dd>The largest body size in bytes (or 'infinity') to allow for
%%%            a body (eg 'POST', etc). This should only cause any effect on
%%%            crary functions that read in the whole body; streaming apis,
%%%            as well as any handler specific body readers will not be
%%%            limited.<br />
%%%            Default: 10Mib</dd>
%%%
%%%        <dt>max_header_size</dt>
%%%        <dd>The largest header size in bytes (or 'infinity') to allow.<br />
%%%            Default: 1Mib</dd>
%%%       </dl>
%%% @end

%%% @type mfa() = {Module::atom(), Function::atom(), Args::list()} | function()
%%% @type vsn() = {Major, Minor}
%%%       Major = integer()
%%%       Minor = integer()

%%% @type tcp_ip_spec() = TcpPort | {IpAddr,  TcpPort}
%%      IpAddr = inet:ip_address()
%%      TcpPort = integer()


%%%====================================================================
%%% Server Control APIs
%%%====================================================================

%% @doc Start a crary server listening on `TcpIpSpec'. `Handler' will be
%% called as `apply(M, F, [Req | Args])' for each request.
%% @spec (tcp_ip_spec(), handler()) -> pid()
start(TcpIpSpec, {M, F, A} = Handler)
  when is_atom(M), is_atom(F), is_list(A) ->
    start_child(TcpIpSpec, [TcpIpSpec, Handler, []]);
start(TcpIpSpec, Handler)
  when is_function(Handler) ->
    start_child(TcpIpSpec, [TcpIpSpec, Handler, []]).

%% @doc Start a crary server listening on `TcpPort' of
%% `IpAddr'. `Handler' will be called as `apply(M, F, [Req | Args])'
%% for each request.
%% @spec (inet:ip_address(), integer(), handler()) -> pid()
start(IpAddr, TcpPort, {M, F, A} = Handler)
  when integer(TcpPort), is_atom(M), is_atom(F), is_list(A) ->
    start_child({IpAddr, TcpPort}, [{IpAddr, TcpPort}, Handler, []]);
start(IpAddr, TcpPort, Handler)
  when is_integer(TcpPort), is_function(Handler) ->
    start_child({IpAddr, TcpPort}, [{IpAddr, TcpPort}, Handler, []]);
%% @spec (tcp_ip_spec(), handler(), opts()) -> pid()
start(TcpIpSpec, {M, F, A} = Handler, Opts)
  when is_list(Opts), is_atom(M), is_atom(F), is_list(A) ->
    start_child(TcpIpSpec, [TcpIpSpec, Handler, Opts]);
start(TcpIpSpec, Handler, Opts)
  when is_list(Opts), is_function(Handler) ->
    start_child(TcpIpSpec, [TcpIpSpec, Handler, Opts]).

%% @doc Start a crary server listening on `TcpPort' of
%% `IpAddr'. `Handler' will be called as `apply(M, F, [Req | Args])'
%% for each request.
%% @spec (inet:ip_address(), integer(), handler(), opts()) -> pid()
start(IpAddr, TcpPort, {M, F, A} = Handler, Opts)
  when is_integer(TcpPort), is_list(Opts), is_atom(M), is_atom(F), is_list(A) ->
    start_child({IpAddr, TcpPort}, [{IpAddr, TcpPort}, Handler, Opts]);
start(IpAddr, TcpPort, Handler, Opts)
  when is_integer(TcpPort), is_list(Opts), is_function(Handler) ->
    start_child({IpAddr, TcpPort}, [{IpAddr, TcpPort}, Handler, Opts]).

%% @private
start_child(Ident, Args) ->
    case supervisor:start_child(crary_sup,
                                {Ident,
                                 {crary_port, start_link, Args},
                                 permanent, 5, worker, [crary_sup]}) of
        {ok, Child} -> Child;
        {error, Reason} -> throw({crary_error, Reason})
    end.

%% @doc Stop the crary server that's running on `TcpPort'.
%% @spec (tcp_ip_spec()) -> ok
stop(TcpIpSpec) ->
    ok = supervisor:terminate_child(crary_sup, TcpIpSpec),
    ok = supervisor:delete_child(crary_sup, TcpIpSpec).

%% @doc Stop the crary server that's running on `TcpPort' of `IpAddr'.
%% @spec (inet:ip_address(), integer()) -> ok
stop(IpAddr, TcpPort) ->
    stop({IpAddr, TcpPort}).

%% @doc Return a list of crary servers (as ports) currently running.
%% @spec () -> [tcp_ip_spec()]
servers() ->
    lists:map(fun ({Id, _Child, _Type, _Modules}) -> Id end,
              supervisor:which_children(crary_sup)).


%% @doc Return the options part of {@link crary_req()} or return the
%% {@link opts()} currently set for the given server.
%% @spec(E) -> opts()
%%     E = crary_req() | tcp_ip_spec()
opts(#crary_req{opts = Opts}) ->
    Opts;
opts(TcpIpSpec) ->
    crary_port:opts(TcpIpSpec).

%% @doc Return or set the {@link opts()} currently set for the given server.
%% @spec(inet:ip_address(), integer()) -> opts().
opts(IpAddr, TcpPort) when is_integer(TcpPort) ->
    crary_port:opts({IpAddr, TcpPort});
opts(TcpIpSpec, Opts) ->
    crary_port:opts(TcpIpSpec, Opts).

%% @doc Set the {@link opts} for given server to specified value.
%% @spec(inet:ip_address(), integer(), opts()) -> ok
opts(IpAddr, TcpPort, NewOpts) ->
    crary_port:opts({IpAddr, TcpPort}, NewOpts).


%% Return the {@link handler()} currently set for the given server.
%% @spec(inet:ip_address(), integer()) -> handler().
handler(TcpIpSpec) ->
    crary_port:handler(TcpIpSpec).

%% Return or set the {@link handler()} currently set for the given server.
%% @spec(inet:ip_address(), integer()) -> handler().
handler(IpAddr, TcpPort) when is_integer(TcpPort) ->
    crary_port:handler({IpAddr, TcpPort});
handler(TcpIpSpec, NewHandler) ->
    crary_port:handler(TcpIpSpec, NewHandler).

%% Set the {@link handler()} for the given server to specified value.
%% @spec(inet:ip_address(), integer(), handler()) -> ok
handler(IpAddr, TcpPort, NewHandler) ->
    crary_port:handler({IpAddr, TcpPort}, NewHandler).

%%%====================================================================
%%% APIs for dealing with HTTP type data
%%%====================================================================

%% @doc Parse the HTTP version string into a version tuple of `{Maj, Min}'.
%% @spec (string()) -> vsn()
list_to_vsn(VsnStr) ->
    {Maj, [$. | MinS]} = string:to_integer(VsnStr),
    {Maj, list_to_integer(MinS)}.

%% @doc Convert a {@link vsn()} tuple to a string.
%% @spec (vsn()) -> string()
vsn_to_iolist(#crary_req{vsn = Vsn}) ->
    vsn_to_iolist(Vsn);
vsn_to_iolist({Maj, Min}) ->
    [integer_to_list(Maj), $., integer_to_list(Min)].

%% @doc Return an iolist of the short ident string such as: `crary/1.0.5'.
%% @spec () -> iolist()
ident() ->
    {ok, Vsn} = application:get_key(crary, vsn),
    [<<"crary/">>, Vsn].

%% @doc Return an iolist of the ident, using `Opts' to determin if the long
%% or short ident string should be used.
%% @spec (proplist()) -> iolist()
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
%% @spec () -> iolist()
long_ident() ->
    lists:map(fun ({Name, _Desc, Vsn}) ->
                      [atom_to_list(Name), $/, Vsn, $ ]
              end, lists:keysort(1, application:loaded_applications())).

%% @doc `Pretty print' the request: return a tuple list representing
%% the crary_req structure in a form that will print nicely via
%% {@link io:format/2} `~p' or {@link error_logger:error_report/1}.
%% @spec (crary_req()) -> list()
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
%% @spec (crary_req(), code(), crary_headers:headers(), BodyOrF) -> ok
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
%% @spec (crary_req(), code(), crary_headers:headers()) -> ok
r(Req, Code, Headers) ->
    crary_sock:write_resp_line(Req, Code),
    Headers2 = crary_headers:extend(
                 [{<<"server">>, ident(Req)},
                  {<<"date">>, gtime:gtostr(gtime:gnow(), rfc1123)}],
                 Headers),
    crary_headers:write(Req, Headers2).

%% @doc Alias for {@link r/3}
%% @see r/3
resp(Req, Code, Headers) ->
    r(Req, Code, Headers).

%% @see error_resp/4
error_resp(Req, Code, Msg) ->
    error_resp(Req, Code, [], Msg).

%% @see error_resp/4
error(Req, Code, Msg) ->
    error_resp(Req, Code, [], Msg).

%% @doc Write a response for errors, this includes the standard error
%% header and footer html. The `title' and `h1' are generated from the
%% `Code'. `Msg' should be verbage describing the problem.
%%
%% Example:
%% ```ro_handler(crary_req{method = 'GET'} = Req) ->
%%               crary:r(Req, ok, ["content-type", "text/plain"],
%%                       "Hello World!");
%%    ro_handler(crary_req{method = Method} = Req) when Method =/= 'GET' ->
%%        crary:error(Req, not_implemented,
%%                    ["The method `", Method,
%%                     "' is not supported by this server.",
%%                     "Please only use 'GET' with this server"]).
%% '''
%%
%% You can get the same effect by throwing one of these tuples:
%% <ul>
%%  <li>```throw({resp_error, Code, Headers, Msg})'''</li>
%%  <li>```throw({resp_error, Code, Msg})'''</li>
%% </ul>
%%
%% @spec (crary_req(), code(), headers(), iolist()) -> ok
error_resp(Req, Code, Headers, Msg) ->
    CodeStr = code_to_binary(Code),
    r(Req, Code, [{<<"content-type">>, <<"text/html">>} | Headers],
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

%% @see error_resp/4
error(Req, Code, Headers, Msg) ->
    error_resp(Req, Code, Headers, Msg).

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
%% @spec (crary_req()) -> ok
not_implemented(#crary_req{uri = Uri, method = Method, vsn = Vsn} = Req) ->
    error(Req, 501,
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
%% @spec (crary_req()) -> ok
bad_request(Req) ->
    error(Req, 400,
          <<"<p>Your browser sent a request that this server
                could not understand.</p>">>).

%% @doc This generates the HTML that is used for 500, `Internal Server
%% Error'. {@link crary_body:with_writer/2} uses this to display error
%% messages; it can't call internal_server_error/4 as the response
%% headers will have already been sent.
%% @spec (crary_req(), atom(), term(), list()) -> iolist()
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
%% @spec (crary_req(), atom(), term(), list()) -> ok
internal_server_error(Req, Class, Reason, Stack) ->
    error(Req, 500, internal_server_error_html(Req, Class, Reason, Stack)).

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
%% @spec (crary_req()) -> ok
not_found(#crary_req{uri = Uri} = Req) ->
    error(Req, 404,
          [<<"<P>The requested URL ">>, Uri#uri.raw,
           <<" was not found on this server.</P>">>]).

%% @doc This is a short cut for sending 403, `Forbidden' error
%% responses with the body already filled in.
%%
%% You can get the same effect by throwing the tuple:
%% ```throw(forbidden)'''
%%
%% @spec (crary_req()) -> ok
forbidden(#crary_req{uri = Uri} = Req) ->
    error(Req, 403,
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
%% with_chunked_resp/4}, and {@link error/3} so that the `Code'
%% argument can be minimally specified.
%%
%% The supported status codes are taken from:
%%   ["http://en.wikipedia.org/wiki/List_of_HTTP_status_codes"]
%%
%% @spec (integer() | atom() | binary()) -> binary()
code_to_binary(100)                 -> <<"100 Continue">>;
code_to_binary(continue)            -> <<"100 Continue">>;
code_to_binary(101)                 -> <<"101 Switching Protocols">>;
code_to_binary(switching_protocols) -> <<"101 Switching Protocols">>;
code_to_binary(102)                 -> <<"102 Processing">>;
code_to_binary(processing)          -> <<"102 Processing">>;
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
code_to_binary(207)                 -> <<"207 Multi-Status">>;
code_to_binary(multi_status)        -> <<"207 Multi-Status">>;
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
code_to_binary(416)                 ->
    <<"416 Requested range not satisfiable">>;
code_to_binary(requested_range_not_satisfiable) ->
    <<"416 Requested range not satisfiable">>;
code_to_binary(417)                 -> <<"417 Expectation Failed">>;
code_to_binary(expectation_failed)  -> <<"417 Expectation Failed">>;
code_to_binary(421)                 ->
    <<"421 There are too many connections from your internet address">>;
code_to_binary(too_many_connections) ->
    <<"421 There are too many connections from your internet address">>;
code_to_binary(422)                 -> <<"422 Unprocessable Entity">>;
code_to_binary(unprocessable_entity)-> <<"422 Unprocessable Entity">>;
code_to_binary(423)                 -> <<"423 Locked">>;
code_to_binary(locked)              -> <<"423 Locked">>;
code_to_binary(424)                 -> <<"424 Failed Dependency">>;
code_to_binary(failed_dependency)   -> <<"424 Failed Dependency">>;
code_to_binary(425)                 -> <<"425 Unordered Collection">>;
code_to_binary(unordered_collection)-> <<"425 Unordered Collection">>;
code_to_binary(426)                 -> <<"426 Upgrade Required">>;
code_to_binary(upgrade_required)    -> <<"426 Upgrade Required">>;
code_to_binary(449)                 -> <<"449 Retry With">>;
code_to_binary(retry_with)          -> <<"449 Retry With">>;
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
code_to_binary(506)                 -> <<"506 Variant Also Negotiates">>;
code_to_binary(variant_also_negotiates) ->
                                       <<"506 Variant Also Negotiates">>;
code_to_binary(507)                 -> <<"507 Insufficient Storage">>;
code_to_binary(insufficient_storage)-> <<"507 Insufficient Storage">>;
code_to_binary(509)                 -> <<"509 Bandwidth Limit Exceeded">>;
code_to_binary(bandwidth_limit_exceeded)
                                    -> <<"509 Bandwidth Limit Exceeded">>;
code_to_binary(510)                 -> <<"510 Not Extended">>;
code_to_binary(not_extended)        -> <<"510 Not Extended">>;
code_to_binary(Code) ->
    Code.


%%%====================================================================
%%% APIs for working with crary records
%%%====================================================================

%% @doc Return the version part of {@link crary_req()}.
%% @spec(crary_req()) -> vsn()
vsn(#crary_req{vsn = Vsn}) ->
    Vsn.

%% @doc Return the method part of {@link crary_req()}, eg `"GET"'.
%% @spec(crary_req()) -> list()
method(#crary_req{method = Method}) ->
    Method.

%% @doc Return the uri part of {@link crary_req()}.
%% @spec(crary_req()) -> uri:uri()
uri(#crary_req{uri = Uri}) ->
    Uri.

%% @doc Return the headers part of {@link crary_req()}.
%% @spec(crary_req()) -> crary_headers:headers()
headers(#crary_req{headers = Headers}) ->
    Headers.

%% @doc Return the socket part of {@link crary_req()}.
%% @spec(crary_req()) -> crary_sock:sock()
sock(#crary_req{sock = Sock}) ->
    Sock.

%% There is a opts() form, but because of pattern matching/overloading
%% it is defined in an earlier section of this file.
