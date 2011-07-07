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

-module(protocol_buffers_triq).

%%
%% Include files
%%
-include_lib("triq/include/triq.hrl").

-export([int32/0,int64/0,uint32/0,uint64/0,sint32/0,sint64/0,fixed32/0,fixed64/0,sfixed32/0,sfixed64/0,string/0,bytes/0, float/0, double/0]).

%% -define(DOM,'@').
%% -record(?DOM,
%%   {kind :: atom() | tuple(),
%%    pick   =fun error_pick/2    :: pick_fun(T),
%%    shrink =fun error_shrink/2  :: shrink_fun(T),
%%    empty_ok = true :: boolean()
%%   }).


%bool() is defined correctly by triq

int32() -> choose(-16#80000000, 16#7fffffff).
int64() -> choose(-16#8000000000000000, 16#7fffffffffffffff).

uint32() -> choose(0,16#ffffffff).
uint64() -> choose(0,16#ffffffffffffffff).

sint32() -> int32().
sint64() -> int64().

fixed32() -> uint32().
fixed64() -> uint64().

sfixed32() -> int32().
sfixed64() -> int64().

string() -> binary().

bytes() -> binary().

float() -> real().

double() -> real().

%% varint_bin() ->
%%   #?DOM{kind=varint,
%%         shrink=fun(Dom,Val) when Val >0 -> {Dom,Val-1};
%%                   (Dom,0) -> {Dom,0}
%%                end,
%%         pick=fun(Dom,SampleSize) ->
%%                   {Dom, protocol_buffers:encode_varint(random:uniform(SampleSize)-(SampleSize div 2))}
%%              end}.
%% 
%% fixed32_bin() ->
%%   binary(4).
%% 
%% fixed64_bin() ->
%%   binary(8).
