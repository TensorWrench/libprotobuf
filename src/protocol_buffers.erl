%% Author: jason
%% Created: Jun 10, 2011
%% Description: TODO: Add description to protocol_buffers
-module(protocol_buffers).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([decode/1,decode/3,encode/3,
         decode_varint/1,encode_varint/1,
         extract/2, cast/2, to_sint/1]).

%%
%% API Functions
%%

% -------------------------------------------------------------
%% @spec decode(binary()) -> [{pos_integer(),varint | fixed32 | fixed64,integer()} | {pos_integer(),length_encoded,binary()}]
%% @doc  Decodes a binary into a stream of key, wiretype, value tuples.
%% @end
% -------------------------------------------------------------
-spec decode(binary()) ->	[{pos_integer(),{varint ,pos_integer()}} | {pos_integer(),{length_encoded | fixed32 | fixed64,binary()}}].
decode(Binary) when is_binary(Binary) ->
  decode(Binary,[],fun array_translator/3).

% -------------------------------------------------------------
%% @spec decode(binary(),Accumulator::term(),Callback::function()) -> Accumulator::term()
%% @doc Decodes a binary by passing successive terms to the Callback.  The Callback function takes the parameter
%% Callback(Key,{WireType,Value},Acc) and returns a new Accumulator.
%% @end
% -------------------------------------------------------------
%% -spec decode(binary(),term(),
%%              fun((pos_integer(), {varint, pos_integer()} | {fixed32 | fixed64 | length_encoded , binary()}, any()) -> any()) -> term().
decode(<<>>,Acc,_) ->
  Acc;
decode(Binary,Acc,Callback) when is_binary(Binary), is_function(Callback)->
  {Int,Rest} = decode_varint(Binary),
  Wiretype = Int band 7,
  Key = Int bsr 3,
  {Value,Rest2} = extract(Wiretype,Rest),
  decode(Rest2,Callback(Key,{int_to_wiretype(Wiretype),Value},Acc),Callback).

% -------------------------------------------------------------
%% @spec decode_varint(binary()) -> {pos_integer(),binary()}
%% @doc Extracts a varint from the front of a binary.  Rest contains any left over content of the binary.
%% @end
% -------------------------------------------------------------
-spec decode_varint(binary()) ->	{integer(),binary()}.
decode_varint(<<0:1, Int:7 ,Rest/binary>>) ->
  {Int,Rest};
decode_varint(<<1:1,Int1:7,0:1,Int2:7,Rest/binary>>) ->
  {(Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,0:1,Int3:7,Rest/binary>>)  ->
  {(Int3 bsl 14) + (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,0:1,Int4:7,Rest/binary>>) ->
  {(Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,1:1,Int4:7,0:1,Int5:7,Rest/binary>>)->
  {(Int5 bsl 28) + (Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,1:1,Int4:7,1:1,Int5:7,0:1,Int6:7,Rest/binary>>)->
  {(Int6 bsl 35) + (Int5 bsl 28) + (Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,1:1,Int4:7,1:1,Int5:7,1:1,Int6:7,0:1,Int7:7,Rest/binary>>)->
  {(Int7 bsl 42) + (Int6 bsl 35) + (Int5 bsl 28) + (Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,1:1,Int4:7,1:1,Int5:7,1:1,Int6:7,1:1,Int7:7,0:1,Int8:7,Rest/binary>>)->
  {(Int8 bsl 49) + (Int7 bsl 42) + (Int6 bsl 35) + (Int5 bsl 28) + (Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,1:1,Int4:7,1:1,Int5:7,1:1,Int6:7,1:1,Int7:7,1:1,Int8:7,0:1,Int9:7,Rest/binary>>)->
  {(Int9 bsl 56) + (Int8 bsl 49) + (Int7 bsl 42) + (Int6 bsl 35) + (Int5 bsl 28) + (Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest};
decode_varint(<<1:1,Int1:7,1:1,Int2:7,1:1,Int3:7,1:1,Int4:7,1:1,Int5:7,1:1,Int6:7,1:1,Int7:7,1:1,Int8:7,1:1,Int9:7,0:1,Int10:7,Rest/binary>>)->
  {(Int10 bsl 63) + (Int9 bsl 56) + (Int8 bsl 49) + (Int7 bsl 42) + (Int6 bsl 35) + (Int5 bsl 28) + (Int4 bsl 21) + (Int3 bsl 14)+ (Int2 bsl 7) + Int1 ,Rest}.


% -------------------------------------------------------------
%% @spec encode(Field::integer(),Type,Value) -> iolist()
%%     Type = bool | int32 | int64 | uint32 | uint64 | sint32 | sint64 | fixed32 | fixed64 | sfixed32 | sfixed64 | length_encoded | float | double
%%     Value = true | false | integer() | binary() | float()
%% @doc Creates an iolist of the field, type, value set in proper protocol buffers style.
%% @end
% -------------------------------------------------------------
-spec encode(integer(),
             bool | int32 | int64 | uint32 | uint64 | sint32 | sint64 | fixed32 | fixed64 | sfixed32 | sfixed64 | length_encoded | float | double,
             boolean() | integer() | binary() | float()) ->	iolist().
encode(Field,bool,true) when is_integer(Field)->
  [encode_varint((Field bsl 3) bor 0),<<1>>];
encode(Field,bool,false) when is_integer(Field)->
  [encode_varint((Field bsl 3) bor 0),<<0>>];
encode(Field,bool,List) when is_list(List) ->
  [encode_varint((Field bsl 3) bor 2), encode_varint(length(List)), lists:map(fun(true) -> <<1>>; (false) -> <<0>> end,List)];

encode(Field,int32,Val) when is_integer(Field), is_integer(Val), Val =< 16#7fffffff, Val > -16#7fffffff ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(Val)];
encode(Field,int64,Val) when is_integer(Field), is_integer(Val), Val =< 16#7fffffffffffffff, Val > -16#7fffffffffffffff ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(Val)];

encode(Field,uint32,Val) when is_integer(Field), is_integer(Val), Val =< 16#ffffffff, Val >= 0 ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(Val)];
encode(Field,uint64,Val) when is_integer(Field), is_integer(Val), Val =< 16#ffffffffffffffff, Val >= 0 ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(Val)];

encode(Field,Type,List) when is_integer(Field), is_list(List), (Type =:= int32 orelse Type =:= int64 orelse Type =:= uint64 orelse Type =:= uint32) ->
  B=[encode_varint(X) || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];


encode(Field,sint32,Val) when is_integer(Field), is_integer(Val), Val < 0 ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(((-Val-1) bsl 1) bor 1)];
encode(Field,sint32,Val) when is_integer(Field), is_integer(Val), Val >= 0 ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(Val bsl 1)];

encode(Field,sint64,Val) when is_integer(Field), is_integer(Val), Val < 0 ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(((-Val-1) bsl 1) bor 1)];
encode(Field,sint64,Val) when is_integer(Field), is_integer(Val), Val >= 0 ->
  [encode_varint((Field bsl 3) bor 0),encode_varint(Val bsl 1)];

encode(Field,Type,List) when is_integer(Field), is_list(List), (Type =:= sint32 orelse Type =:= sint64) ->
  B=lists:map(fun(X) when X < 0 -> encode_varint(((-X-1) bsl 1) bor 1);
                 (X) when X >= 0 -> encode_varint(X bsl 1)
              end,List),
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,float,Val) when is_integer(Field), is_float(Val) ->
  [encode_varint((Field bsl 3) bor 5),<<Val:32/float>>];
encode(Field,float,List) when is_integer(Field), is_list(List) ->
  B=[<<X:32/float>> || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,double,Val) when is_integer(Field), is_float(Val) ->
  [encode_varint((Field bsl 3) bor 1),<<Val:64/float>>];
encode(Field,double,List) when is_integer(Field), is_list(List) ->
  B=[<<X:64/float>> || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,fixed32,Val) when is_integer(Field), is_integer(Val), Val =< 16#ffffffff, Val >= 0 ->
  [encode_varint((Field bsl 3) bor 5),<<Val:32/unsigned-little-integer>>];
encode(Field,fixed32,List) when is_integer(Field), is_list(List) ->
  B=[<<X:32/unsigned-little-integer>> || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,fixed64,Val) when is_integer(Field), is_integer(Val), Val =< 16#ffffffffffffffff, Val >= 0 ->
  [encode_varint((Field bsl 3) bor 1),<<Val:64/unsigned-little-integer>>];
encode(Field,fixed64,List) when is_integer(Field), is_list(List) ->
  B=[<<X:64/unsigned-little-integer>> || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,sfixed32,Val) when is_integer(Field), is_integer(Val),  Val =< 16#7fffffff, Val > -16#7fffffff ->
  [encode_varint((Field bsl 3) bor 5),<<Val:32/signed-little-integer>>];
encode(Field,sfixed32,List) when is_integer(Field), is_list(List) ->
  B=[<<X:32/signed-little-integer>> || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,sfixed64,Val) when is_integer(Field), is_integer(Val), Val =< 16#7fffffffffffffff, Val > -16#7fffffffffffffff ->
  [encode_varint((Field bsl 3) bor 1),<<Val:64/signed-little-integer>>];
encode(Field,sfixed64,List) when is_integer(Field), is_list(List) ->
  B=[<<X:64/signed-little-integer>> || X <- List],
  [encode_varint((Field bsl 3) bor 2),encode_varint(iolist_size(B)),B];

encode(Field,bytes,Val) -> encode(Field,length_encoded,Val);
encode(Field,string,Val) -> encode(Field,length_encoded,Val);
encode(Field,length_encoded,Val) when is_integer(Field) ->
  Bin = iolist_to_binary(Val),
  [encode_varint((Field bsl 3) bor 2),encode_varint(byte_size(Bin)),Bin]. 

%% @hidden
encode_varint(I) when I < 0 ->
  <<I2:64/unsigned-integer>> = <<I:64/unsigned-integer>>,
  encode_varint(I2,[]);
encode_varint(I) when I >= 0 ->
    encode_varint(I, []).

%% @hidden
encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).


%%
% 0 Varint  int32, int64, uint32, uint64, sint32, sint64, bool, enum
% 1 64-bit  fixed64, sfixed64, double
% 2 Length-delimited  string, bytes, embedded messages, packed repeated fields
% 3 Start group groups (deprecated)
% 4 End group groups (deprecated)
% 5 32-bit  fixed32, sfixed32, float
extract(0,Binary) when is_binary(Binary) ->
    decode_varint(Binary);
extract(1,<<Val:8/binary,Rest/binary>>) ->
    {Val,Rest};
extract(2,Binary) when is_binary(Binary) ->
    {Len,Raw}=decode_varint(Binary),
    case Len of
      0 -> {<<>>,Raw};
      L when L =< size(Raw) ->
          <<Data:Len/binary,Rest/binary>> = Raw,
%%           io:format("Decoding binary len = ~p, Data = ~p, Rest = ~p",[Len,Data, Rest]),
          {Data,Rest};
      _ ->
         error({not_enough_data,io_lib:format("Size is ~p, Requested is ~p of ~p ~n", [size(Raw),Len, Binary])})

    end;
extract(5,<<Val:4/binary,Rest/binary>>) ->
    {Val,Rest}.


% -------------------------------------------------------------
%% @spec cast(ToType,From) -> term()
%% @doc Casts a wiretype to a specific end type
%% @end
% -------------------------------------------------------------
-spec cast(
        bool | int32 | int64 | uint32 | uint64 | sint32 | sint64 | fixed32 | fixed64 | sfixed32 | sfixed64 | string | bytes | float | double,
        {fixed32,binary()} | {'fixed64',binary()} | {'length_encoded',binary()} | {'varint',pos_integer()}) ->	term().

cast(bool,{varint, V}) -> V /= 0;

cast(int32, {varint, V}) -> <<I:32/signed-integer>> = <<V:32/unsigned-integer>>, I; 
cast(int64, {varint, V}) -> <<I:64/signed-integer>> = <<V:64/unsigned-integer>>, I;

cast(uint32, {varint, V}) -> V;
cast(uint64, {varint, V}) -> V;

cast(sint32, {varint, V}) -> to_sint(V);
cast(sint64, {varint, V}) -> to_sint(V);

cast(fixed32, {fixed32,<<V:32/little-unsigned-integer>>}) -> V;
cast(fixed32, {length_encoded,<<V:32/little-unsigned-integer>>}) -> [V];
cast(fixed32, {length_encoded,<<V:32/little-unsigned-integer,Rest/binary>>}) -> [V | cast(fixed32, {length_encoded,Rest})];

cast(fixed64, {fixed64,<<V:64/little-unsigned-integer>>}) -> V;
cast(fixed64, {length_encoded,<<V:64/little-unsigned-integer>>}) -> [V];
cast(fixed64, {length_encoded,<<V:64/little-unsigned-integer,Rest/binary>>}) -> [V | cast(fixed64, {length_encoded,Rest})];

cast(sfixed32, {fixed32,<<V:32/little-signed-integer>>}) -> V;
cast(sfixed32, {length_encoded,<<V:32/little-signed-integer>>}) -> [V];
cast(sfixed32, {length_encoded,<<V:32/little-signed-integer,Rest/binary>>}) -> [V | cast(sfixed32, {length_encoded,Rest})];

cast(sfixed64, {fixed64,<<V:64/little-signed-integer>>}) -> V;
cast(sfixed64, {length_encoded,<<V:64/little-signed-integer>>}) -> [V];
cast(sfixed64, {length_encoded,<<V:64/little-signed-integer,Rest/binary>>}) -> [V | cast(sfixed64, {length_encoded,Rest})];

cast(string, {length_encoded,B}) -> B;
cast(bytes, {length_encoded,B}) -> B;

cast(float, {fixed32,<<V:32/float>>}) -> V;
cast(float, {length_encoded,<<V:32/float>>}) -> [V];
cast(float, {length_encoded,<<V:32/float,Rest/binary>>}) -> [V | cast(float, {length_encoded,Rest})];

cast(double, {fixed64,<<V:64/float>>}) -> V;
cast(double, {length_encoded,<<V:64/float>>}) -> [V];
cast(double, {length_encoded,<<V:64/float,Rest/binary>>}) -> [V | cast(double, {length_encoded,Rest})];

% since all the specialized types of packed, length encoded fields are handled, this catch-all will handle
% packed varints of various types
cast(_,{length_encoded, <<>>}) -> [];
cast(T,{length_encoded, Binary}) when is_binary(Binary) -> {V,Rest} = decode_varint(Binary), [ cast(T,{varint,V}) | cast(T,{length_encoded, Rest})].



% -------------------------------------------------------------
%% @spec to_sint(pos_integer()) -> integer()
%% @doc  Turns a positive integer into a zigzag encoded signed integer.
%% @end
% -------------------------------------------------------------
-spec to_sint(pos_integer()) ->	integer().
to_sint(Int) when is_integer(Int) ->
  case Int band 1 of
    0 -> (Int bsr 1);
    1 -> -(Int bsr 1) -1
  end.


int_to_wiretype(0) -> varint;
int_to_wiretype(1) -> fixed64;
int_to_wiretype(2) -> length_encoded;
%% int_to_wiretype(3) -> group_start;
%% int_to_wiretype(4) -> group_end;
int_to_wiretype(5) -> fixed32.

% not the most efficient, but does the expected thing of returning all fields in order without requiring the end user to reverse()
array_translator(Key,{WireType,Value},Acc) ->
  Acc ++ [{Key,{WireType,Value}}].