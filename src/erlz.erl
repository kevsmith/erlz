%% -------------------------------------------------------------------
%%
%% erlz: Erlang bindings for the fastlz compression library
%%
%% Copyright (c) 2007-2010 Hypothetical Labs, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-module(erlz).

-export([compress/1,
         decompress/1]).

compress(Data) when is_binary(Data) ->
    erlz_nifs:compress(Data).

decompress(Data) ->
    erlz_nifs:decompress(Data).

-ifdef(TEST).
small_test() ->
    T = <<"abcdefghijklmnopqrstuvwxyz0123456789">>,
    T1 = erlz:compress(T),
    case T1 =:= T of
        false ->
            T =:= erlz:decompress(T1);
        true ->
            throw({error, compression_failure})
    end.

medium_test() ->
    {N1, N2, N3} = erlang:now(),
    random:seed(N1, N2, N3),
    T = generate_text(8 * 1024),
    T1 = erlz:compress(T),
    case size(T1) < size(T) of
        true ->
            T =:= erlz:decompress(T1);
        false ->
            throw({error, compression_failure})
    end.

large_test() ->
    {N1, N2, N3} = erlang:now(),
    random:seed(N1, N2, N3),
    T = generate_text(64 * 1024),
    T1 = erlz:compress(T),
    case size(T1) < size(T) of
        true ->
            T =:= erlz:decompress(T1);
        false ->
            throw({error, compression_failure})
    end.

generate_text(Size) ->
    generate_text(Size, []).

generate_text(0, Accum) ->
    list_to_binary(Accum);
generate_text(Size, Accum) ->
    generate_text(Size - 1, [(random:uniform(27) + 95)|Accum]).

-endif.
