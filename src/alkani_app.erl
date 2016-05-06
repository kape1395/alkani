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
%%% OTP Application.
%%%
-module(alkani_app).
-behaviour(application).
-compile([{parse_transform, lager_transform}]).
-export([name/0, version/0, get_env/1, get_env/2, set_env/2]).
-export([start/2, stop/1]).

-define(APP, alkani).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Returns name of this application.
%%
name() ->
    ?APP.


%%
%%
%%
version() ->
    case lists:keyfind(?APP, 1, application:which_applications()) of
        {_App, _Type, Version}  -> Version;
        false                   -> undefined
    end.


%%
%%  Returns environment varianbles for this application.
%%
get_env(Name) ->
    application:get_env(?APP, Name).


%%
%%  Returns environment varianbles for this application.
%%
get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).


%%
%%  Updates application environment variables.
%%
set_env(Name, NewValue) ->
    application:set_env(?APP, Name, NewValue).



%%% ============================================================================
%%% Application callbacks
%%% ============================================================================


%%
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    alkani_sup:start_link().


%%
%%  Stop the application.
%%
stop(_State) ->
    ok.



%%% ============================================================================
%%% Helper functions.
%%% ============================================================================

