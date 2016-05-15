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
%%%
%%%
-module(alkani_game).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, add_player/2, reset_food/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("alkani.hrl").

-define(MAX_FOOD, 150).
-define(AREA_WIDTH, 100).
-define(AREA_HEIGHT, 100).


%%% ============================================================================
%%% Public API.
%%% ============================================================================

%%
%%  Start this process.
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).


%%
%%
%%
add_player(Pid, Name) ->
    ok = gen_server:cast(?MODULE, {add_player, Pid, Name}),
    ok.


%%
%%
%%
reset_food() ->
    ok = gen_server:cast(?MODULE, reset_food).



%%% ============================================================================
%%% Internal data structures.
%%% ============================================================================

-record(state, {
    players :: [#player{}],
    food    :: [#food{}]
}).



%%% ============================================================================
%%% Callbacks for gen_server.
%%% ============================================================================

%%
%%  Initialize this server.
%%
init({}) ->
    _ = erlang:process_flag(trap_exit, true),
    State = #state{
        players = [],
        food = []
    },
    self() ! time_tick,
    {ok, State}.


%%
%%  Sync calls.
%%
handle_call(_Unknown, _From, State) ->
    {reply, undefined, State}.


%%
%%  Async messages.
%%
handle_cast({add_player, Pid, Name}, State = #state{players = Players}) ->
    true = erlang:link(Pid),
    Player = #player{
        name = Name,
        pid  = Pid,
        pos_x = random:uniform(?AREA_WIDTH) - 1,
        pos_y = random:uniform(?AREA_HEIGHT) - 1,
        size = 10
    },
    NewState = State#state{
        players = [Player | Players]
    },
    {noreply, NewState};

handle_cast(reset_food, State) ->
    NewState = State#state{food = []},
    {noreply, NewState};

handle_cast(_Unknown, State) ->
    {noreply, State}.


%%
%%  Unknown messages.
%%
handle_info(time_tick, State) ->
    NewFood = grow_food(State),
    NewState = update_players(State#state{food = NewFood}),
    lager:debug("Time tick, food=~p.", [length(NewFood)]),
    _ = erlang:send_after(1000, self(), time_tick),
    {noreply, NewState};

handle_info({'EXIT', From, _Reason}, State = #state{players = Players}) ->
    NewPlayers = lists:keydelete(From, #player.pid, Players),
    NewState = State#state{
        players = NewPlayers
    },
    {noreply, NewState};

handle_info(_Unknown, State) ->
    {noreply, State}.

%%
%%  Terminate a process.
%%
terminate(_Reason, _State) ->
    ok.


%%
%%  Code upgrades.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ============================================================================
%%% Internal functions.
%%% ============================================================================

%%
%%
%%
grow_food(#state{food = Food}) ->
    MakeNewFood = fun (_Num) ->
        #food{
            pos_x = random:uniform(?AREA_WIDTH) - 1,
            pos_y = random:uniform(?AREA_HEIGHT) - 1,
            size = 1
        }
    end,
    FoodCount = length(Food),
    AddedFood = lists:map(MakeNewFood, lists:seq(1, (?MAX_FOOD - FoodCount) div 100)),
    AddedFood ++ Food.


%%
%%
%%
update_players(State = #state{players = Players, food = Food}) ->
    SendStateToPlayer = fun (#player{pid = Pid}) ->
        ok = alkani_player:new_state(Pid, Players, Food)
    end,
    ok = lists:foreach(SendStateToPlayer, Players),
    State.


