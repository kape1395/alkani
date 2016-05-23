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
%%%
%%% Game area is of size
%%%
%%%     A = 1.
%%%
%%% Player size is defined as follows, in order to limit
%%% its size on the game area:
%%%
%%%            R_max * S_P
%%%     R_P = -------------
%%%              K + S_P
%%%
%%% where S_P is a number of food eated by the player P and
%%% R_P is a radius of the player. This definition implies that:
%%%
%%%     R_P < R_max
%%%
%%% Viewport size of a player should be proportional to its size:
%%%
%%%     V_P = R_P * V
%%%
%%% where V is a constant describing how much the viewport is bigger
%%% comparing to the player's size.
%%%
%%% Viewport should be smaller or equal to the game area:
%%%
%%%     V_P =< A
%%%
%%% threfore
%%%
%%%     R_P * V =< A,   R_P =< A/V   =>   R_max =< A/V
%%%
-module(alkani_game).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([start_link/0, add_player/2, reset_food/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("alkani.hrl").

-define(MAX_FOOD,  10000).
-define(TICK_RATE, 100).

-define(A,      1.0).       % Area size.
-define(V,      20.0).      % Viewport size, comparing to the player's size.
-define(R_max,  ?A/?V).     % Maximal radius if a player.
-define(K,      100).       % How fast half of the R_max is reached.
-define(INIT_SIZE, 10).     % Initial size of a player.

-define(FOOD_SPEED,     0.0005).   % Linear speed of the food.
-define(FOOD_ANGLE,     2.0).      % Angular speed of the food.
-define(FOOD_SIZE,      1).        % Size of the food elements.
-define(FOOD_GROW_PROB, 0.001).    % Probability for food to grow.
-define(FOOD_SIZE_MAX,  10).       % Maximal size of the food.


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
        name  = Name,
        pid   = Pid,
        pos_x = random:uniform(),
        pos_y = random:uniform(),
        size  = ?INIT_SIZE
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
    _ = erlang:send_after(?TICK_RATE, self(), time_tick),
    {DurationUS, NewState = #state{players = NewPlayers, food = NewFood}} = timer:tc(fun () ->
        NewFood = grow_food(State),
        NewState = update_players(State#state{food = NewFood}),
        NewState
    end),
    lager:debug("Time tick in ~pus, players=~p, food=~p.", [DurationUS, length(NewPlayers), length(NewFood)]),
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
            pos_x = random:uniform(),
            pos_y = random:uniform(),
            size = ?FOOD_SIZE,
            dir = random:uniform() * math:pi() * 2
        }
    end,
    MoveFood = fun (F = #food{pos_x = X, pos_y = Y, dir = D, size = S}) ->
        NewX = X + math:cos(D) * ?FOOD_SPEED,
        NewY = Y + math:sin(D) * ?FOOD_SPEED,
        F#food{
            pos_x = if
                NewX >= ?A -> NewX - ?A;
                NewX <  0  -> NewX + ?A;
                true       -> NewX
            end,
            pos_y = if
                NewY >= ?A -> NewY - ?A;
                NewY <  0  -> NewY + ?A;
                true       -> NewY
            end,
            size = case (random:uniform() < ?FOOD_GROW_PROB) andalso (S < ?FOOD_SIZE_MAX) of
                true  -> S + 1;
                false -> S
            end,
            dir = D + (rand:uniform() - 0.5) * ?FOOD_ANGLE
        }
    end,
    FoodCount = length(Food),
    AddedFood = lists:map(MakeNewFood, lists:seq(1, (?MAX_FOOD - FoodCount) div 100)),
    AddedFood ++ lists:map(MoveFood, Food).


%%
%%
%%
update_players(State = #state{players = []}) ->
    State;

update_players(State = #state{players = [Player | Players], food = Food}) ->
    {NewPlayers, NewFood} = update_players(Player, [], Players, Food),
    State#state{players = NewPlayers, food = NewFood}.

update_players(Player = #player{pid = Pid, name = Name, pos_x = PosX, pos_y = PosY, size = Size}, PrevPlayers, NextPlayers, Food) ->
    R_P = radius(Size),
    V_P = R_P * ?V,
    V_P2 = V_P / 2,
    %lager:debug("Player: pos=(~p, ~p), size=~p, R_P=~p, V_P=~p", [PosX, PosY, Size, R_P, V_P]),
    FoodFun = fun (F = #food{pos_x = F_Xn, pos_y = F_Yn, size = F_S}, {F_Vs, F_A, S}) ->
        F_X = F_Xn, % TODO
        F_Y = F_Yn, % TODO
        F_R = radius(F_S),
        case (erlang:abs(F_X - PosX) - F_R =< V_P2) andalso (erlang:abs(F_Y - PosY) - F_R =< V_P2) of
            true ->
                F_V = F#food{
                    pos_x = (F_X - PosX) / V_P,
                    pos_y = (F_Y - PosY) / V_P,
                    size = F_R / V_P
                },
                Dist = math:sqrt((PosX - F_X) * (PosX - F_X) + (PosY - F_Y) * (PosY - F_Y)),
                case Dist =< R_P of
                    true  -> {[F_V | F_Vs], F_A, S + F_S};
                    false -> {[F_V | F_Vs], [F | F_A], S}
                end;
            false ->
                {F_Vs, [F | F_A], S}
        end
    end,
    {ViewpointFood, NewFood, NewSize} = lists:foldl(FoodFun, {[], [], Size}, Food),
    NewPlayer = Player#player{
        size = NewSize
    },
    ok = alkani_player:new_state(Pid, Name, NewSize, R_P / V_P, [], ViewpointFood),
    case NextPlayers of
        []                          -> {lists:reverse([NewPlayer | PrevPlayers]), NewFood};
        [NextPlayer | OtherPlayers] -> update_players(NextPlayer, [NewPlayer | PrevPlayers], OtherPlayers, NewFood)
    end.


%%
%%
%%
radius(Size) ->
    ?R_max * Size / (?K + Size).


