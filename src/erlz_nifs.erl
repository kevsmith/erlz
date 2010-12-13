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

-module(erlz_nifs).

-define(ERLZ_API_VERSION, "1.0").

-on_load(init/0).

-export([compress/1,
         decompress/1]).

init() ->
    PrivDir = case code:priv_dir(erlzo) of
                  {error, bad_name} ->
                      D = filename:dirname(code:which(?MODULE)),
                      filename:join([D, "..", "priv"]);
                  Dir ->
                      Dir
              end,
    SoName = filename:join([PrivDir, "erlz_nifs"]),
    erlang:load_nif(SoName, ?ERLZ_API_VERSION).

compress(_Data) ->
    throw({error, missing_nif}).

decompress(_Data) ->
    throw({error, missing_nifs}).
