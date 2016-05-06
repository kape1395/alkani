%/--------------------------------------------------------------------
%| Copyright 2016 Karolis Petrauskas
%|
%| Licensed under the Apache License, Version 2.0 (the "License");
%| you may not use this file except in compliance with the License.
%| You may obtain a copy of the License at
%|
%|     http://www.apache.org/licenses/LICENSE-2.0
%|
%| Unless required by applicable law or agreed to in writing, software
%| distributed under the License is distributed on an "AS IS" BASIS,
%| WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%| See the License for the specific language governing permissions and
%| limitations under the License.
%\--------------------------------------------------------------------

%%%
%%% Main application supervisor.
%%%
-module(alkani_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0]).
-export([init/1]).


%%% ============================================================================
%%% API functions.
%%% ============================================================================

%%
%% Create this supervisor.
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).



%%% ============================================================================
%%% Callbacks for supervisor.
%%% ============================================================================

%%
%% Supervisor initialization.
%%
init({}) ->
    {ok, {{one_for_all, 100, 10}, []}}.


