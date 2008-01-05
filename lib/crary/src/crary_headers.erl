-module(crary_headers).

-export([new/0, new/1, new/2, add/2, add/3, extend/2, has/2, lookup/2]).
-export([get/2, get/3, get_lower/2, get_lower/3]).
-export([to_list/1, foreach/2]).
-export([write/2]).

-define(EOL, <<"\r\n">>).

new() ->
    gb_trees:empty().

% is gb_tree, do nothing
new({N, Ts}) when Ts == nil, N == 0; is_tuple(Ts), N > 0 ->
    {N, Ts};
new(List) when is_list(List) ->
    lists:foldl(fun add/2, gb_trees:empty(), List);
new({dict,_,_,_,_,_,_,_,_} = Dict) ->
    lists:foldl(fun add/2, gb_trees:empty(), dict:to_list(Dict));
new(S) ->
    read_headers(S, infinity, new(), crary_sock:read_line(S)).

new(S, Opts) ->
    read_headers(S, proplists:get_value(keep_alive_timeout, Opts),
                 new(), crary_sock:read_line(S, Opts)).

read_headers(_S, _Timeout, Headers, "") ->
    Headers;
read_headers(S, Timeout, Headers, NextHeaderLine) ->
    case crary_sock:read_line(S, Timeout) of
        % this line is a continuation of the last header
        [WS | _] = L when WS == $\ ; WS == $\t ->
            read_headers(S, Timeout, Headers,
                         lists:flatten([NextHeaderLine, ?EOL, L]));
        L ->
            {Key, Val} = parse_header(NextHeaderLine),
            read_headers(S, Timeout, add(Key, Val, Headers), L)
    end.

parse_header(RawHeader) ->
    case string:chr(RawHeader, $:) of
        0 -> % not fond
            throw({crary_error, {bad_header, RawHeader}});
        Idx ->
            FieldName = string:substr(RawHeader, 1, Idx - 1),
            FieldValue = string:substr(RawHeader, Idx + 1), 
            {string:to_lower(FieldName), string:strip(FieldValue)}
    end.

add({<<Name/binary>>, Value}, Headers) ->
    gb_trees:enter(binary_to_list(Name), Value, new(Headers));
add({Name, Value}, Headers) ->
    gb_trees:enter(Name, Value, new(Headers));
add(KVList, Headers) when is_list(KVList) ->
    extend(KVList, Headers).

extend(KVList, Headers) ->
    lists:foldl(fun ({K, V}, Hs) ->
                        add(K, V, Hs)
                end, new(Headers), KVList).

add(<<Name/binary>>, Value, Headers) ->
    gb_trees:enter(binary_to_list(Name), Value, new(Headers));
add(Name, Value, Headers) ->
    gb_trees:enter(Name, Value, new(Headers)).

get(Name, Headers, Default) when is_list(Name) ->
    try gb_trees:get(Name, Headers)
    catch error:function_clause -> Default
    end.

get(Name, Headers) when is_list(Name) ->
    try gb_trees:get(Name, Headers)
    catch error:function_clause ->
            throw({crary_headers_error, {field_name_not_found, Name}})
    end.


get_lower(Name, Headers, Default) when is_list(Name) ->
    try string:to_lower(gb_trees:get(Name, Headers))
    catch error:function_clause -> Default
    end.

get_lower(Name, Headers) when is_list(Name) ->
    try string:to_lower(gb_trees:get(Name, Headers))
    catch error:function_clause ->
            throw({crary_headers_error, {field_name_not_found, Name}})
    end.

has(Name, Headers) ->
    gb_trees:is_defined(Name, new(Headers)).

lookup(Name, Headers) ->
    gb_trees:lookup(Name, new(Headers)).

to_list(Headers) when is_list(Headers) ->
    Headers;
to_list({dict,_,_,_,_,_,_,_,_} = Dict) ->
   dict:to_list(Dict);
to_list(Headers) ->
    gb_trees:to_list(new(Headers)).

foreach(F, Headers) ->
    lists:foreach(F, to_list(Headers)).

% write headers including terminating blank line
write(S, Headers) ->
    foreach(fun ({K, V}) -> crary_sock:write(S, [K, $:, $\ , V, ?EOL]) end,
            Headers),
    % TODO: send timeouts (see manpage)
    crary_sock:write(S, ?EOL).

