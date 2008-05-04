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
%%% @doc This module acts an an associative data-structure to hold
%%% headers. This can be used for both request as well as response
%%% headers.
%%%
%%% This module is fairly flexible with inputs. Stringish inputs can
%%% always be given in the form of iolists() or atoms(). Headerish
%%% inputs can always be given as headers(), tuple_list(), dict(),
%%% gb_trees(), or as a socket to read/parse them from. However,
%%% outputs will always be header() or string(). In the case of names,
%%% since HTTP header names are case insensitive they will always be
%%% returned lower cased.

-module(crary_headers).

-include("crary.hrl").

-export([new/0, new/1, from_sock/1, from_sock/2]).
-export([add/2, add/3, extend/2, has/2, lookup/2]).
-export([get/2, get/3]).
-export([to_list/1, foreach/2]).
-export([write/2]).

-define(EOL, <<"\r\n">>).

%%% @type headers() = term().
%%% Currently headers are backed by gb_trees, but please don't
%%% depend on this as it may change in the future.

%%% @type stringish() = iolist() | atom()
%%% @type headerish() = headers() | tuple_list() | dict() | gb_trees() |
%%%                     crary:crary_req()

%% @doc Create an empty headers() structure.
%% @spec () -> crary_headers()
new() ->
    gb_trees:empty().

%% @doc Create a {@link header()} structure from data. The data can
%% either be a socket or an existing datastructure including gb_trees,
%% tuplelists, dicts. If a socket was given the headers are {@link
%% crary_sock:read/2. read/2} off the socket, parsed, and returned in
%% the {@link header()}
%% @spec (headerish()) -> headers()
new({N, Ts}) when Ts == nil, N == 0; is_tuple(Ts), N > 0 ->
    %% is gb_tree, do nothing
    {N, Ts};
new(List) when is_list(List) ->
    lists:foldl(fun add/2, gb_trees:empty(), List);
new({dict,_,_,_,_,_,_,_,_} = Dict) ->
    lists:foldl(fun add/2, gb_trees:empty(), dict:to_list(Dict));
new(#crary_req{headers = Headers}) ->
    Headers.


%% @doc Create a {@link header()} structure from the socket. Opts is
%% used to set the timeout.
%% @spec (Sock) -> headers()
%%       Sock = crary_sock:sock() | crary:crary_req()
from_sock(#crary_req{sock = S, opts = Opts}) ->
    from_sock(S, Opts);
from_sock(S) ->
    read_headers(S, infinity, new(), crary_sock:read_line(S)).

%% @doc Create a {@link header()} structure from the socket. Opts is
%% used to set the timeout.
%% @spec (Sock, crary:proplist()) -> headers()
%%       Sock = crary_sock:sock() | crary:crary_req()
from_sock(S, Opts) ->
    read_headers(S, proplists:get_value(keep_alive_timeout, Opts),
                 new(), crary_sock:read_line(S, Opts)).

read_headers(_S, _Timeout, Headers, "") ->
    Headers;
read_headers(S, Timeout, Headers, NextHeaderLine) ->
    case crary_sock:read_line(S, Timeout) of
        %% this line is a continuation of the last header
        [WS | _] = L when WS == $\ ; WS == $\t ->
            read_headers(S, Timeout, Headers,
                         lists:flatten([NextHeaderLine, ?EOL, L]));
        L ->
            {Key, Val} = parse_header(NextHeaderLine),
            read_headers(S, Timeout, add(Key, Val, Headers), L)
    end.

parse_header(RawHeader) ->
    case string:chr(RawHeader, $:) of
        0 -> %% not fond
            throw({crary_error, {bad_header, RawHeader}});
        Idx ->
            FieldName = string:substr(RawHeader, 1, Idx - 1),
            FieldValue = string:substr(RawHeader, Idx + 1),
            {string:to_lower(FieldName), string:strip(FieldValue)}
    end.

%% @doc Return {@link headers()} extended with the specified header/headers.
%% @spec (NameValue | [NameValue], headerish()) -> headers()
%%       NameValue = {Name::stringish(), Value::stringish()}
add({Name, Value}, Headers) ->
    gb_trees:enter(to_lower_string(Name), Value, new(Headers));
add(KVList, Headers) when is_list(KVList) ->
    extend(KVList, Headers).

%% @doc Return {@link headers()} extended with the specified header.
%% @spec (Name::stringish(), Value::stringish(), headerish()) -> headers()
add(Name, Value, Headers) ->
    gb_trees:enter(to_lower_string(Name), Value, new(Headers)).

%% @doc Return {@link headers()} extended with the specified headers.
%% @spec ([{Name::stringish(), Value::stringish()}], headerish()) ->
%%       headers()
extend(KVList, Headers) ->
    lists:foldl(fun ({K, V}, Hs) ->
                        add(K, V, Hs)
                end, new(Headers), KVList).

%% @doc Return the value of the header given by the `Name'.
%% @spec (Name::stringish(), headerish(), Default::term()) ->
%%       string() | Default
get(Name, Headers, Default) when is_list(Name) ->
    try gb_trees:get(to_lower_string(Name), new(Headers))
    catch error:function_clause -> Default
    end.

%% @doc Return the value of the header given by the `Name'.
%% @spec (Name::stringish(), headerish()) -> string()
%% @throws {crary_headers_error, {name_not_found, Name}}
get(Name, Headers) when is_list(Name) ->
    try gb_trees:get(to_lower_string(Name), new(Headers))
    catch error:function_clause ->
            throw({crary_headers, {name_not_found, Name}})
    end.

%% @doc Does `Headers' include `Name'?
%% @spec (Name::stringish(), headerish()) -> bool()
has(Name, Headers) ->
    gb_trees:is_defined(to_lower_string(Name), new(Headers)).

%% @doc Does `Headers' include `Name'?
%% @spec (Name::stringish(), headerish()) -> bool()
lookup(Name, Headers) ->
    gb_trees:lookup(Name, new(Headers)).

%% @doc Convert `Headers' to a {@link tuple_list()}. Perfect for
%% printing or looping over.
%% @spec (headerish()) -> tuple_list()
%% @see foreach/2
to_list(Headers) when is_list(Headers) ->
    Headers;
to_list({dict,_,_,_,_,_,_,_,_} = Dict) ->
   dict:to_list(Dict);
to_list(Headers) ->
    gb_trees:to_list(new(Headers)).

%% @doc Call `F({K, V})' for each header in `Headers'.
%% @spec (function(), headerish()) -> ok
foreach(F, Headers) ->
    lists:foreach(F, to_list(Headers)).

%% @doc Write `Headers' to socket, including terminating blank line.
%% @spec (crary_sock:sock() | crary:crary_req(), headerish()) -> ok
write(S, Headers) ->
    foreach(fun ({K, V}) -> crary_sock:write(S, [K, $:, $\ , V, ?EOL]) end,
            Headers),
    % TODO: send timeouts (see manpage)
    crary_sock:write(S, ?EOL).

to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(Str) ->
    binary_to_list(iolist_to_binary(Str)).

to_lower_string(Name) ->
    string:to_lower(to_string(Name)).
