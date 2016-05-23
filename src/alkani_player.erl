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
%%% Represents a player, implements a web socket.
%%%
-module(alkani_player).
-compile([{parse_transform, lager_transform}]).
-export([start/1, new_state/6]).
-export([init/1, handle_message/1]).
-include("alkani.hrl").
-include_lib("yaws/include/yaws_api.hrl").


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Starts the particular instance of the websocket.
%%
start(_Args) ->
    Opts = [
        {origin, any},
        {keepalive, true},
        {callback, {basic, websocket}}
    ],
    {websocket, ?MODULE, Opts}.


new_state(WebsocketPID, PlayerName, PlayerSize, PlayerR, Players, Food) ->
    Event = encode(state, {PlayerName, PlayerSize, PlayerR, Players, Food}),
    ok = yaws_api:websocket_send(WebsocketPID, {text, jiffy:encode(Event)}),
    ok.


%%% ============================================================================
%%% Internal data structures.
%%% ============================================================================

%%
%%  State for the websocket process (`yaws_websocket`)
%%
-record(state, {
}).


%%% ============================================================================
%%% Callbacks for yaws.
%%% ============================================================================

%%
%%  Init for `yaws_websocket` and `gen_event`.
%%
init([_Arg, websocket]) ->
    WebsocketPID = self(),
    ok = alkani_game:add_player(WebsocketPID, <<"Name">>),
    lager:debug("The events websocket initialized, pid=~p", [WebsocketPID]),
    {ok, #state{}}.


%%
%%  Callback for yaws.
%%
handle_message({close, Status, Reason}) ->
    lager:debug("The events websocket is closed, status=~p, reason=~p.", [Status, Reason]),
    noreply;

handle_message({Type, Data}) ->
    lager:debug("The events websocket received a message, type=~p, data=~p.", [Type, Data]),
    noreply.


%%
%%
%%
encode({list, Mode}, List) when is_list(List) ->
    [ encode(Mode, E) || E <- List ];

encode(state, {PlayerName, PlayerSize, PlayerR, Players, Food}) ->
    {[
        {name, PlayerName},
        {size, PlayerSize},
        {r,    PlayerR},
        {players, encode({list, player}, Players)},
        {food, encode({list, food}, Food)}
    ]};

encode(player, #player{name = Name, pos_x = PosX, pos_y = PosY, size = Size}) ->
    {[
        {name,  Name},
        {x, PosX},
        {y, PosY},
        {r, Size},
        {s, null}
    ]};

encode(food, #food{pos_x = PosX, pos_y = PosY, size = Size}) ->
    {[
        {x, PosX},
        {y, PosY},
        {r, Size}
    ]};

encode(_Mode, undefined) ->
    null.
