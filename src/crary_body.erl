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
%%% @doc This module provides functions for detecting, reading, and
%%% writing HTTP bodies and encodings such as chunking.
%%%
%%% If you try to do chunked writes for a pre-http-1.1 client, this
%%% module will do it by falling back to writing a streamed body with
%%% no `content-length'. Obviously the connection has to be closed to
%%% end the data stream. This means you can using a stream to
%%% incrementally generate a webpage, or write out a large file, and
%%% browsers such as w3m and lynx will still be able to work fine.

-module(crary_body).

%%% public
-export([has_body/1]).
-export([new_reader/1, new_writer/1, with_writer/2]).
-export([read/1, read/2, read_all/1]).
-export([write/2, done_writing/1, done_writing/2]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-include("crary.hrl").

-define(EOL, <<"\r\n">>).

-record(state, {buf = []}).

%% @type writer() = term().
%%    This is the handle to the writer that {@link new_writer/1} will
%%    return. Please treat is as a transparent value at the moment as
%%    the implementation of it can and will change in the future.
%% @type reader() = pid().

%%%====================================================================
%%% public
%%%====================================================================

%% @doc Does this request have a body that needs to be read? It determins
%% this by checking for `content-length' or `transfer-encoding' headers.
%% @spec (crary:crary_req()) -> bool()
has_body(Req) ->
    crary_headers:has("content-length", Req) orelse
        crary_headers:has("transfer-encoding", Req).

%% @doc Return a new chunk reader.
%% @spec (crary:crary_req()) -> reader()
new_reader(Req) ->
    gen_server:start_link(?MODULE, init, Req, []).

%% @doc Return a new chunk writer.
%%
%% It may be observed that currently this call just returns the same
%% `Req' that was passed to it. Don't depend on this, it will likely
%% be changed to a pid() or similar in the future for supporting
%% buffering or other encodings.
%% @spec (crary:crary_req()) -> writer()
%% @see with_writer/2
%% @see crary:r/4
new_writer(Req) ->
    Req.

%% @doc Call `F(Writer)' with a new writer, automatically closing the
%% writer when `F' returns, and writing an error message if `F' throws
%% an exception.
%%
%% Since the response line has almost certainly already been writen
%% out, the best this function can do is append an error message into
%% the output, and hope that it will be seen. If this is not good
%% behavior for your application, use a try/catch form in `F' to keep
%% errors from making it down the stack to here.
%%
%% @spec (crary:crary_req(), function()) -> pid()
with_writer(Req, F) ->
    W = new_writer(Req),
    try F(W)
    catch
        C:R -> %% headers were already sent, so just write the error message out
            Stack = erlang:get_stacktrace(),
            write(W, crary:internal_server_error_html(Req, C, R, Stack))
    after done_writing(W)
    end.

%% @doc Read and return the next available chunk.
%% @spec (reader()) -> binary()
read(S) ->
    gen_server:call(S, read, infinity).

%% @doc Read and return `Len' bytes.
%% @spec (reader(), integer()) -> binary()
read(S, Len) ->
    gen_server:call(S, {read, Len}, infinity).

%% @doc Read and return the full body. It doesn't matter if the body
%% is chunked or fixed length, this will read it all in and return it
%% as one binary. Probably great for `PUT' bodies for forms. Probably
%% not great for reading in a large amount of data.
%% @spec (crary:crary_req()) -> binary()
read_all(Req) ->
    MaxBytes = proplists:get_value(max_body_size, Req#crary_req.opts, infinity),
    case crary_headers:get("content-length", Req, null) of
        null ->
            exit(chunking_read_not_yet_supported),
            case crary_headers:has("transfer-encoding") of
                true ->
                    Reader = new_reader(Req),
                    read_all_(Reader, MaxBytes, read(Reader), []);
                    % todo either as:done_reading() and terminate reader,
                    % or have reader detect and do that itself (is that
                    % possible?
                false ->
                    throw({crary_body, {body_headers_not_found,
                                        "no content-length or transfer-encoding headers found"}})
            end;
        LenStr ->
            try crary_sock:read(Req, list_to_integer(LenStr))
            after crary_sock:done_reading(Req)
            end
    end.

%% @doc Write a chunk of data. At the moment, this data is immediately
%% written as a chunk, regardless of the size. In the future writes
%% may get buffered, probably with a configurable buffer size.
%% @spec (writer(), Data::iolist()) -> ok
write(_Req, "") ->
    ok;
write(#crary_req{vsn = Vsn} = Req, Data) when Vsn == {1, 0}; Vsn == {0, 9} ->
    crary_sock:write(Req, Data);
write(Req, Data) ->
    Len = iolist_size(Data),
    crary_sock:write(Req, [erlang:integer_to_list(Len, 16), ?EOL, Data, ?EOL]).

%% @doc Writing the closing chunk. For pre-http-1.1 streaming this also
%% closes the socket.
%% @spec (writer()) -> ok
done_writing(#crary_req{vsn = {1, 0}} = Req) ->
    %% since we were streaming, we can't keep-alive for http 1.0, even
    %% if that was requested
    crary_sock:close(Req);
done_writing(Req) ->
    crary_sock:write(Req, <<"0\r\n\r\n">>).

%% @doc Writing the `Trailers' and the closing chunk.
%% @spec (writer(), crary_headers:headerish()) -> ok
done_writing(Req, Trailers) ->
    crary_sock:write(Req, <<"0\r\n">>),
    crary_headers:write(Req, Trailers).

%%%====================================================================
%%% reader gen_server callbacks
%%%====================================================================

%% @private
init(Req) ->
    put(crary_sock, Req),
    {ok, #state{}}.

%% @private
handle_call(read, _From, State) ->
    {State2, Data} = read_(State, get(crary_sock)),
    {reply, Data, State2};

handle_call({read, Len}, _From, State) ->
    {State2, Data} = read_(State, get(crary_sock), Len),
    {reply, Data, State2}.

%% @private
handle_cast(R, _State) ->
    throw({unexpected_cast, R}).

%% @private
handle_info(R, _State) ->
    throw({unexpected_info, R}).

%% @private
terminate(Reason, _State) ->
    error_logger:error_report([{server, crary_body},
                               {terminate, io_lib:format("~p",[Reason])}]).

%% @private
code_change(_OldVsn, C, _Extra) ->
    {ok, C}.

%%%====================================================================
%%% private
%%%====================================================================

read_(#state{buf = []} = State, Req) ->
    case read_chunk_header_line_(Req) of
        0 ->
            Trailers = crary_headers:from_sock(Req),
            {State, {"", Trailers}};
        Len ->
            Data = crary_sock:read(Req, Len),
            <<"\r\n">> = crary_sock:read(Req, 2),
            {State, Data}
    end;
read_(#state{buf = Buf} = State, _Req) ->
    {State#state{buf = []}, Buf}.

read_no_trailers_(State, Req) ->
    case read_(State, Req) of
        {State2, {Data, _Trailers}} ->  {State2, Data};
        {State2, Data}              ->  {State2, Data}
    end.

read_chunk_header_line_(Req) ->
    Line = crary_sock:read_line(Req),
    erlang:list_to_integer(hd(string:sub_word(Line, 1, $;)), 16).

read_(#state{buf = Buf} = State, Req, Len) ->
    case iolist_size(Buf) of
        BLen when BLen > Len ->
            BBuf = iolist_to_binary(Buf),
            <<LData:Len/binary, RData/binary>> = BBuf,
            {State#state{buf = RData}, LData};
        BLen when BLen == Len ->
            {State#state{buf = []}, Buf};
        BLen when BLen < Len ->
            {State2, Data} = read_no_trailers_(State, Req),
            read_(Req, State2#state{buf = [Buf, Data]}, Len)
    end.

read_all_(_Reader, BytesLeft, _Data, _Acc) when BytesLeft < 0 ->
    throw({crary_error, body_too_long});
read_all_(_Reader, _BytesLeft, {"", Trailers}, Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Trailers};
read_all_(Reader, BytesLeft, Data, Acc) ->
    read_all_(Reader, BytesLeft - size(Data), read(Reader), [Data | Acc]).
