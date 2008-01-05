-module(crary_body).

%% public

-export([has_body/1]).
-export([new_reader/1, new_writer/1, with_writer/2]).
-export([read/1, read/2, read_all/1]).
-export([write/2, done_writing/1, done_writing/2]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-include("crary.hrl").

-define(EOL, <<"\r\n">>).

-record(state, {buf = []}).

%% public

has_body(#crary_req{headers = Headers}) ->
    crary_headers:has("content-length", Headers) orelse 
        crary_headers:has("transfer-encoding", Headers).

new_reader(Req) ->
    gen_server:start_link(?MODULE, init, Req, []).

new_writer(Req) ->
    Req.

with_writer(Req, F) ->
    W = new_writer(Req),
    try F(W)
    catch
        C:R -> %% headers were already sent, so just write the error message out
            Stack = erlang:get_stacktrace(),
            write(W, crary:internal_server_error_html(Req, C, R, Stack))
    after done_writing(W)
    end.


read(S) ->
    gen_server:call(S, read, infinity).

read(S, Len) ->
    gen_server:call(S, {read, Len}, infinity).

read_all(Req) ->
    MaxBytes = proplists:get_value(max_body_size, Req#crary_req.opts, infinity),
    case crary_headers:get("content-length", Req#crary_req.headers, null) of
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

write(_Req, "") ->
    ok;
write(#crary_req{vsn = Vsn} = Req, Data) when Vsn == {1, 0}; Vsn == {0, 9} ->
    crary_sock:write(Req, Data);
write(Req, Data) ->
    Len = iolist_size(Data),
    crary_sock:write(Req, [erlang:integer_to_list(Len, 16), ?EOL, Data, ?EOL]).

% since we were streaming, we can't keep-alive for http 1.0, even if
% that was requested
done_writing(#crary_req{vsn = {1, 0}} = Req) ->
    crary_sock:close(Req);
done_writing(Req) ->
    crary_sock:write(Req, <<"0\r\n\r\n">>).
		     
done_writing(Req, Trailers) ->
    crary_sock:write(<<"0\r\n">>),
    crary_headers:write(Req, Trailers).

%% gen_server callbacks

init(Req) ->
    put(crary_sock, Req),
    {ok, #state{}}.

handle_call(read, _From, State) ->
    {State2, Data} = read_(State, get(crary_sock)),
    {reply, Data, State2};

handle_call({read, Len}, _From, State) ->
    {State2, Data} = read_(State, get(crary_sock), Len),
    {reply, Data, State2}.

handle_cast(R, _State) ->
    throw({unexpected_cast, R}).

handle_info(R, _State) ->
    throw({unexpected_info, R}).

terminate(Reason, _State) ->
    error_logger:error_report([{server, crary_body},
			       {terminate, io_lib:format("~p",[Reason])}]).

code_change(_OldVsn, C, _Extra) ->
    {ok, C}.

%% private

read_(#state{buf = []} = State, Req) ->
    case read_chunk_header_line_(Req) of
	0 ->
	    Trailers = crary_headers:new(Req),
	    {State, {"", Trailers}};
	Len ->
	    Data = read(Req, Len),
	    "\r\n" = read(Req, 2),
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
    Line = crary_sock:read_line_(Req),
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
