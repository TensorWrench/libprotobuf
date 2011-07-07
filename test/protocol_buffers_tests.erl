% Erlang pluging to Protocol Buffers
% Copyright 2011 Tensor Wrench LLC.  All rights reserved.
% https:%github.com/TensorWrench/libprotobuf

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
%     * Redistributions of source code must retain the above copyright
% notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above
% copyright notice, this list of conditions and the following disclaimer
% in the documentation and/or other materials provided with the
% distribution.
%     * Neither the name of Google Inc. nor the names of its
% contributors may be used to endorse or promote products derived from
% this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(protocol_buffers_tests).

%%
%% Include files
%%
-include_lib("libprotobuf/include/protocol_buffers_triq.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
dev_test_() ->
  [
    extract_varints(),
    decode(),
    encode(),
    to_sint(),
    decode_packed(),
    encode_packed(),
    encode_sint32(),
    encode_sint64()
  ].


extract_varints() ->
  [{"Simple varint test",?_assertMatch({150,<<>>},protocol_buffers:decode_varint(<<16#96,16#01>>))},
   {"Round trip, simple varint",?_assertMatch({150,<<>>},protocol_buffers:decode_varint(iolist_to_binary(protocol_buffers:encode_varint(150))))}
  ].

decode() ->
  [
   {"Decode a varint",?_assertMatch([{2,{varint,150}}],protocol_buffers:decode(<<2:5, 0:3 ,16#96, 16#01>>))},
   {"Decode a 64 bit int",?_assertMatch([{2,{fixed64,<<150:64/unsigned-little-integer>>}}],protocol_buffers:decode(<<2:5, 1:3 ,150:64/unsigned-little-integer>>))},
   {"Decode a string",?_assertMatch([{2,{length_encoded,<<"testing">>}}],protocol_buffers:decode(<<2:5, 2:3 ,16#07 ,16#74 ,16#65 ,16#73 ,16#74 ,16#69 ,16#6e ,16#67>>))},
   {"Decode a 32 bit int",?_assertMatch([{2,{fixed32,<<150:32/unsigned-little-integer>>}}],protocol_buffers:decode(<<2:5, 5:3 ,150:32/unsigned-little-integer>>))}
  ].

encode() ->
  [
   {"Encode true",?_assertMatch(<<2:5, 0:3, 1>>,iolist_to_binary(protocol_buffers:encode(2,bool,true)))},
   {"Encode false",?_assertMatch(<<2:5, 0:3, 0>>,iolist_to_binary(protocol_buffers:encode(2,bool,false)))},
   {"Encode a int32",?_assertMatch(<<2:5, 0:3 ,16#96, 16#01>>,iolist_to_binary(protocol_buffers:encode(2,int32,150)))},
   {"Encode a int64",?_assertMatch(<<2:5, 0:3 ,16#96, 16#01>>,iolist_to_binary(protocol_buffers:encode(2,int64,150)))},

   {"Encode a -1 int32",?_assertMatch(<<2:5, 0:3 ,16#ffffffffffffffffff01:80>>,iolist_to_binary(protocol_buffers:encode(2,int32,-1)))},
   {"Encode a -1 int64",?_assertMatch(<<2:5, 0:3 ,16#ffffffffffffffffff01:80>>,iolist_to_binary(protocol_buffers:encode(2,int64,-1)))},

   {"Encode a uint32",?_assertMatch(<<2:5, 0:3 ,16#96, 16#01>>,iolist_to_binary(protocol_buffers:encode(2,uint32,150)))},
   {"Encode a uint64",?_assertMatch(<<2:5, 0:3 ,16#96, 16#01>>,iolist_to_binary(protocol_buffers:encode(2,uint64,150)))},

   {"Encode a sint32",?_assertMatch(<<2:5, 0:3 , 2#10101011,2#00000010>>,iolist_to_binary(protocol_buffers:encode(2,sint32,-150)))},
   {"Encode a sint64",?_assertMatch(<<2:5, 0:3 , 2#10101011,2#00000010>>,iolist_to_binary(protocol_buffers:encode(2,sint64,-150)))},

   {"Encode a negative sint32",?_assertMatch(<<2:5, 0:3 ,2#10101100,2#00000010>>,iolist_to_binary(protocol_buffers:encode(2,sint32,150)))},
   {"Encode a negative sint64",?_assertMatch(<<2:5, 0:3 ,2#10101100,2#00000010>>,iolist_to_binary(protocol_buffers:encode(2,sint64,150)))},
   
   {"Encode a float",?_assertMatch(<<2:5,5:3,3.125:32/float>>,iolist_to_binary(protocol_buffers:encode(2,float,3.125)))},
   {"Encode a double",?_assertMatch(<<2:5,1:3,3.1415:64/float>>,iolist_to_binary(protocol_buffers:encode(2,double,3.1415)))},
   
   {"Encode a fixed32",?_assertMatch(<<2:5, 5:3 , 150:32/unsigned-little-integer>>,iolist_to_binary(protocol_buffers:encode(2,fixed32,150)))},
   {"Encode a fixed64",?_assertMatch(<<2:5, 1:3 ,150:64/unsigned-little-integer>>,iolist_to_binary(protocol_buffers:encode(2,fixed64,150)))},
   {"Encode a sfixed32",?_assertMatch(<<2:5, 5:3 ,150:32/signed-little-integer>>,iolist_to_binary(protocol_buffers:encode(2,sfixed32,150)))},
   {"Encode a sfixed64",?_assertMatch(<<2:5, 1:3 ,150:64/signed-little-integer>>,iolist_to_binary(protocol_buffers:encode(2,sfixed64,150)))},
   {"Encode a negative sfixed32",?_assertMatch(<<2:5, 5:3 ,-150:32/signed-little-integer>>,iolist_to_binary(protocol_buffers:encode(2,sfixed32,-150)))},
   {"Encode a negative sfixed64",?_assertMatch(<<2:5, 1:3 ,-150:64/signed-little-integer>>,iolist_to_binary(protocol_buffers:encode(2,sfixed64,-150)))},
   {"Encode a string",?_assertMatch(<<2:5, 2:3 ,16#07 ,16#74 ,16#65 ,16#73 ,16#74 ,16#69 ,16#6e ,16#67>>,iolist_to_binary(protocol_buffers:encode(2,length_encoded,<<"testing">>)))}
  ].

encode_sint32() ->
  [
    {"Encode Sint32 1 -> -1",?_assertMatch([_,<<1:8>>], protocol_buffers:encode(1,sint32,-1))},
    {"Encode Sint32 2 -> 1",?_assertMatch([_,<<2:8>>],protocol_buffers:encode(1,sint32,1))},
    {"Encode Sint32 3 -> -2",?_assertMatch([_,<<3:8>>],protocol_buffers:encode(1,sint32,-2))},
    {"Encode Sint32 4 -> 2",?_assertMatch([_,<<4:8>>], protocol_buffers:encode(1,sint32,2))},
    {"Encode Sint32 5 -> -3",?_assertMatch([_,<<5:8>>],protocol_buffers:encode(1,sint32,-3))},
    {"Encode Sint32 6 -> 3",?_assertMatch([_,<<6:8>>], protocol_buffers:encode(1,sint32,3))}
   ].

encode_sint64() ->
  [
    {"Encode Sint64 1 -> -1",?_assertMatch([_,<<1:8>>], protocol_buffers:encode(1,sint64,-1))},
    {"Encode Sint64 2 -> 1",?_assertMatch([_,<<2:8>>],protocol_buffers:encode(1,sint64,1))},
    {"Encode Sint64 3 -> -2",?_assertMatch([_,<<3:8>>],protocol_buffers:encode(1,sint64,-2))},
    {"Encode Sint64 4 -> 2",?_assertMatch([_,<<4:8>>], protocol_buffers:encode(1,sint64,2))},
    {"Encode Sint64 5 -> -3",?_assertMatch([_,<<5:8>>],protocol_buffers:encode(1,sint64,-3))},
    {"Encode Sint64 6 -> 3",?_assertMatch([_,<<6:8>>], protocol_buffers:encode(1,sint64,3))}
   ].

to_sint() ->
   [
    {"Sint 1 -> -1",?_assertMatch(-1, protocol_buffers:to_sint(1))},
    {"Sint 2 -> 1",?_assertMatch(1,protocol_buffers:to_sint(2))},
    {"Sint 3 -> -2",?_assertMatch(-2,protocol_buffers:to_sint(3))},
    {"Sint 4 -> 2",?_assertMatch(2, protocol_buffers:to_sint(4))},
    {"Sint 5 -> -3",?_assertMatch(-3,protocol_buffers:to_sint(5))},
    {"Sint 6 -> 3",?_assertMatch(3, protocol_buffers:to_sint(6))},
    {"Sint 4294967294 -> 2147483647", ?_assertMatch(2147483647, protocol_buffers:to_sint(4294967294))},
    {"Sint 4294967295 -> -2147483648", ?_assertMatch(-2147483648, protocol_buffers:to_sint(4294967295))}
   ].


decode_packed() ->
    [
      {"Decode a packed repeated integer",?_assertMatch([34,6,3,270,86942],protocol_buffers:cast(int32,{length_encoded,<<16#22,16#06,16#03,16#8E,16#02,16#9E,16#A7, 16#05>>}))},
      {"Decode packed repeated fixed32", ?_assertMatch([1,50,13,1000],protocol_buffers:cast(fixed32,{length_encoded,
                                                         <<1:32/little-integer,50:32/little-integer,13:32/little-integer,1000:32/little-integer>>}))},
      {"Decode packed repeated fixed64", ?_assertMatch([1,50,13,1000],protocol_buffers:cast(fixed64,{length_encoded,
                                                         <<1:64/little-integer,50:64/little-integer,13:64/little-integer,1000:64/little-integer>>}))}
    ].


encode_packed() ->
    [
      {"Decode a packed repeated integer",?_assertMatch(<<2:5, 2:3, 8:8, 16#22,16#06,16#03,16#8E,16#02,16#9E,16#A7, 16#05>>,
                                                        iolist_to_binary(protocol_buffers:encode(2,uint64,[34,6,3,270,86942])))},
      {"Decode packed repeated fixed32", ?_assertMatch(<<2:5, 2:3, 16:8, 1:32/little-integer,50:32/little-integer,13:32/little-integer,1000:32/little-integer>>,
                                                       iolist_to_binary(protocol_buffers:encode(2,fixed32,[1,50,13,1000]))
                                                         )},
      {"Decode packed repeated fixed64", ?_assertMatch(<<2:5, 2:3, 32:8, 1:64/little-integer,50:64/little-integer,13:64/little-integer,1000:64/little-integer>>,
                                                       iolist_to_binary(protocol_buffers:encode(2,fixed64,[1,50,13,1000]))
                                                         )}
    ].

varints_roundtrip_test_() ->
  {timeout, 60, 
    ?_assert(triq:check(?FORALL(I,uint64(),
                       {I,<<>>} =:= protocol_buffers:decode_varint(iolist_to_binary(protocol_buffers:encode_varint(I))))))}.

decode_roundtrip_test_() ->
  F=fun(Type) ->
    Label= lists:flatten(io_lib:format("Roundtrip on single ~p",[Type])),
    { Label,{timeout,60, 
    ?_assert(triq:check(?FORALL(I,protocol_buffers_triq:Type(),
                                begin
                                  %?debugFmt("Trying ~p with ~p",[Type,I]),
                                  B = iolist_to_binary(protocol_buffers:encode(1,Type,I)),
                                  %?debugFmt("Binary = ~p",[B]),
                                  [{1,Value}]=protocol_buffers:decode(B),
                                  %?debugFmt("Trying ~p with ~p =?= ~p ",[Type,I, protocol_buffers:cast(Type,Value)]),
                                  I =:= protocol_buffers:cast(Type, Value)
                                end)
                        ))}}
    end,
  lists:map(F,
            [int32,int64,uint32,uint64,sint32,sint64,fixed32,fixed64,sfixed32,sfixed64,bytes, double]
            ).

% todo: float, string
decode_roundtrip_packed_test_() ->
  F=fun(Type) ->
    Label= lists:flatten(io_lib:format("Roundtrip on packed ~p",[Type])),
    { Label,{timeout,60, 
    ?_assert(triq:check(?FORALL(I,list(protocol_buffers_triq:Type()),
                                begin
                                  %?debugFmt("Trying ~p with ~p",[Type,I]),
                                  B = iolist_to_binary(protocol_buffers:encode(1,Type,I)),
                                  %?debugFmt("Binary = ~p",[B]),
                                  % if I was an empty list (perfectly valid), then nothing was encoded and we should see nothing in return
                                  case I of
                                    [] -> I =:= protocol_buffers:decode(B);
                                    _ ->
                                      [{1,Value}]=protocol_buffers:decode(B),
                                      %?debugFmt("Trying ~p with ~p =?= ~p ",[Type,I, protocol_buffers:cast(Type,Value)]),
                                      I =:= protocol_buffers:cast(Type, Value)
                                  end
                                end)
                        ))}}
    end,
  lists:map(F,
            [int32,int64,uint32,uint64,sint32,sint64,fixed32,fixed64,sfixed32,sfixed64,double]
            ).
%todo: float


record_roundtrip_test_() ->
    {timeout, 60, 
     ?_assert(
      triq:check(
        ?FORALL({A,B,C},{uint64(),fixed32(),fixed64()},
           begin
             Bin = iolist_to_binary([protocol_buffers:encode(1, uint64, A),
                                     protocol_buffers:encode(2, fixed32, B),
                                     protocol_buffers:encode(3, fixed64, C)
                                     ]),
             [{1,{varint,A}},{2,{fixed32,<<B:32/unsigned-little-integer>>}},{3,{fixed64,<<C:64/unsigned-little-integer>>}}] =:= protocol_buffers:decode(Bin)
           end
        ) % forall
      ) % check
    )}. % assert

