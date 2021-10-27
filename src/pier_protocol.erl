% TODO: use pre-compiled binary pattern

-module(pier_protocol).
-include("pier.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    decode/1,
    encode/1
]).

-define(NL, <<"\r\n">>).

%% public
-spec decode(binary()) ->
    {ok, term(), binary()} | {error, not_enough_data}.

decode(Bin) ->
    decode_type(Bin).

-spec encode(list()) ->
    iolist().

encode(List) ->
    encode_array_bulk_string(List).

%% private
encode_array_bulk_string(List) ->
    Size = [<<$*>>, integer_to_list(length(List)),?NL],
    Elements = lists:map(fun encode_bulk_string/1, lists:map(fun to_binary/1, List)),
    [Size, Elements].

encode_bulk_string(B) when is_binary(B) ->
    [<<$$>>, integer_to_list(iolist_size(B)),?NL, B,?NL].

decode_array(Bin) ->
    case binary:split(Bin,?NL) of
        [_] ->
            {error, not_enough_data};
        [Size, Rest] ->
            % TODO: keep intermediary state
            Size2 = binary_to_integer(Size),
            decode_elements(Rest, Size2, [])
    end.

decode_bulk_string(<<"0\r\n\r\n", Rest/binary>>) ->
    {ok, {ok, undefined}, Rest};
decode_bulk_string(Bin) ->
    case binary:split(Bin,?NL) of
        [_] ->
            {error, not_enough_data};
        [Size, Rest] ->
            % TODO: keep intermediary state
            Size2 = binary_to_integer(Size) + 2,
            case size(Rest) >= Size2 of
                false ->
                    {error, not_enough_data};
                true ->
                    [String, Rest2] = binary:split(Rest,?NL),
                    {ok, {ok, String}, Rest2}
            end
    end.

decode_elements(Rest, 0, Acc) ->
    {ok, {ok, lists:reverse(Acc)}, Rest};
decode_elements(<<>>, _N, _Acc) ->
    {error, not_enough_data};
decode_elements(Bin, N, Acc) ->
    case decode_type(Bin) of
        {ok, {ok, Element}, Rest} ->
            decode_elements(Rest, N - 1, [Element | Acc]);
        {error, not_enough_data} ->
            {error, not_enough_data}
    end.

decode_error(Bin) ->
    case binary:split(Bin,?NL) of
        [_] ->
            {error, not_enough_data};
        [Error, Rest] ->
            {ok, {error, Error}, Rest}
    end.

decode_integer(Bin) ->
    case binary:split(Bin,?NL) of
        [_] ->
            {error, not_enough_data};
        [Integer, Rest] ->
            {ok, {ok, binary_to_integer(Integer)}, Rest}
    end.

decode_string(Bin) ->
    case binary:split(Bin,?NL) of
        [_] ->
            {error, not_enough_data};
        [String, Rest] ->
            {ok, {ok, String}, Rest}
    end.

decode_type(<<"+", Rest/binary>>) ->
    decode_string(Rest);
decode_type(<<"-", Rest/binary>>) ->
    decode_error(Rest);
decode_type(<<":", Rest/binary>>) ->
    decode_integer(Rest);
decode_type(<<"$", Rest/binary>>) ->
    decode_bulk_string(Rest);
decode_type(<<"*", Rest/binary>>) ->
    decode_array(Rest).

to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) ->
    integer_to_binary(X);
to_binary(X) ->
    term_to_binary(X).
