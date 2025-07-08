-module(uno_ws_handler).
-behaviour(cowboy_websocket).
-include("include/uno_game.hrl").

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(GAME_TABLE, uno_state).
-define(NAME_TABLE, uno_name).
-define(HEARTBEAT_INTERVAL, 45000).
-define(HEARTBEAT_TIMEOUT, 60000).

%%% INIT

init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}, #{
        idle_timeout => ?HEARTBEAT_TIMEOUT,
        max_frame_size => 8192
    }}.

websocket_init(State) ->
    ensure_game_table(),
    ensure_name_table(),
    maybe_init_game(),
    Timer = erlang:start_timer(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {ok, State#{heartbeat_timer => Timer}}.

card_value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
card_value_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8).

websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State};

websocket_handle({text, <<"pong">>}, State) ->
    OldTimer = maps:get(heartbeat_timer, State, undefined),
    if OldTimer =/= undefined -> erlang:cancel_timer(OldTimer); true -> ok end,
    NewTimer = erlang:start_timer(?HEARTBEAT_INTERVAL, self(), heartbeat),
    {ok, State#{heartbeat_timer => NewTimer}};

websocket_handle({text, <<"join">>}, State) ->
    [{game, Game}] = ets:lookup(?GAME_TABLE, game),
    Game1 = uno_game:add_player(Game, self()),
    io:format("Nuevo jugador: ~p~n", [self()]),
    io:format("Total jugadores: ~p~n", [maps:keys(Game1#game.players)]),
    ets:insert(?NAME_TABLE, {self(), erlang:pid_to_list(self())}),
    ets:insert(?GAME_TABLE, {game, Game1}),
    broadcast_state(Game1),
    {reply, {text, jsx:encode(#{type => <<"joined">>, status => <<"ok">>})}, State};

websocket_handle({text, Msg}, State) ->
    io:format("Received message: ~p~n", [Msg]),
    case catch jsx:decode(Msg, [return_maps]) of
        Map = #{<<"type">> := <<"play">>, <<"card">> := [ColorBin, ValueBin]} ->
            try
                Color = binary_to_existing_atom(ColorBin, utf8),
                Value = case catch binary_to_integer(ValueBin) of
                    {'EXIT', _} -> binary_to_existing_atom(ValueBin, utf8);
                    Int -> Int
                end,
                ChosenColor = case Value of
                    wild -> get_chosen_color(Map);
                    draw4 -> get_chosen_color(Map);
                    _ -> Color
                end,
                Card = {Color, Value},
                [{game, Game0}] = ets:lookup(?GAME_TABLE, game),
                case Game0#game.turn =:= self() of
                    true ->
                        Game1Tmp = uno_game:play_card(Game0, self(), Card),
                        Game1 = case Value of
                            wild -> Game1Tmp#game{current_color = ChosenColor};
                            draw4 -> Game1Tmp#game{current_color = ChosenColor};
                            _ -> Game1Tmp
                        end,
                        ets:insert(?GAME_TABLE, {game, Game1}),
                        broadcast_state(Game1),
                        {reply, {text, jsx:encode(#{type => <<"play_result">>, status => <<"ok">>})}, State};
                    false ->
                        {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Not your turn!">>})}, State}
                end
            catch
                Error:Reason ->
                    io:format("Error playing card: ~p:~p~n", [Error, Reason]),
                    {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Invalid card">>})}, State}
            end;

        #{<<"type">> := <<"draw">>} ->
            [{game, Game0}] = ets:lookup(?GAME_TABLE, game),
            case Game0#game.turn =:= self() of
                true ->
                    try
                        {NewGame, DrawResult} = uno_game:draw_card(Game0, self()),
                        ets:insert(?GAME_TABLE, {game, NewGame}),
                        case DrawResult of
                            {reshuffled, Card} ->
                                self() ! {text, jsx:encode(#{type => <<"reshuffle">>, message => <<"Deck reshuffled">>})},
                                self() ! {text, jsx:encode(#{type => <<"draw_result">>, card => [atom_to_binary(element(1, Card), utf8), card_value_to_binary(element(2, Card))]})};
                            Card ->
                                self() ! {text, jsx:encode(#{type => <<"draw_result">>, card => [atom_to_binary(element(1, Card), utf8), card_value_to_binary(element(2, Card))]})}
                        end,
                        broadcast_state(NewGame),
                        {ok, State}
                    catch
                        _:_ ->
                            {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Deck is empty or cannot draw">>})}, State}
                    end;
                false ->
                    {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Not your turn!">>})}, State}
            end;

        #{<<"type">> := <<"declare_uno">>} ->
            [{game, Game0}] = ets:lookup(?GAME_TABLE, game),
            Game1 = uno_game:declare_uno(Game0, self()),
            ets:insert(?GAME_TABLE, {game, Game1}),
            broadcast_state(Game1),
            {reply, {text, jsx:encode(#{type => <<"uno_declared">>, status => <<"ok">>})}, State};

        #{<<"type">> := <<"reset">>} ->
            [{game, Game0}] = ets:lookup(?GAME_TABLE, game),
            Game1 = uno_game:reset(Game0),
            ets:insert(?GAME_TABLE, {game, Game1}),
            broadcast_state(Game1),
            {reply, {text, jsx:encode(#{type => <<"reset_result">>, status => <<"ok">>})}, State};

        _Other ->
            io:format("Unknown message: ~p~n", [Msg]),
            {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Unknown message type">>})}, State}
    end;

websocket_handle(_, State) ->
    {ok, State}.

get_chosen_color(Map) ->
    case maps:get(<<"chosen_color">>, Map, undefined) of
        undefined -> throw({error, missing_chosen_color});
        Bin -> binary_to_existing_atom(Bin, utf8)
    end.

%%% INFO

websocket_info({timeout, Timer, heartbeat}, State) ->
    case maps:get(heartbeat_timer, State, undefined) of
        Timer ->
            NewTimer = erlang:start_timer(?HEARTBEAT_INTERVAL, self(), heartbeat),
            {reply, {text, <<"ping">>}, State#{heartbeat_timer => NewTimer}};
        _ -> {ok, State}
    end;

websocket_info({text, Data}, State) ->
    {reply, {text, Data}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, State) ->
    case maps:get(heartbeat_timer, State, undefined) of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    try
        [{game, Game}] = ets:lookup(?GAME_TABLE, game),
        Players = maps:remove(self(), Game#game.players),
        NewTurn = case Game#game.turn =:= self() of
            true -> case maps:keys(Players) of [] -> undefined; [NextPlayer | _] -> NextPlayer end;
            false -> Game#game.turn
        end,
        Game1 = Game#game{players = Players, turn = NewTurn},
        ets:insert(?GAME_TABLE, {game, Game1}),
        ets:delete(?NAME_TABLE, self()),
        case maps:size(Players) > 0 of
            true -> broadcast_state(Game1);
            false -> ok
        end
    catch _:_ -> ok end,
    io:format("Jugador desconectado: ~p~n", [self()]),
    ok.

%%% HELPERS

ensure_game_table() ->
    case ets:info(?GAME_TABLE) of
        undefined -> ets:new(?GAME_TABLE, [named_table, public]);
        _ -> ok
    end.

ensure_name_table() ->
    case ets:info(?NAME_TABLE) of
        undefined -> ets:new(?NAME_TABLE, [named_table, public]);
        _ -> ok
    end.

maybe_init_game() ->
    case ets:lookup(?GAME_TABLE, game) of
        [] -> ets:insert(?GAME_TABLE, {game, uno_game:new()});
        _ -> ok
    end.

broadcast_state(Game) ->
    WinnerName = case Game#game.winner of
        undefined -> null;
        WPid -> case ets:lookup(?NAME_TABLE, WPid) of
            [{_, WName}] -> list_to_binary(WName);
            _ -> list_to_binary(erlang:pid_to_list(WPid))
        end
    end,
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true ->
                Hand = maps:get(Pid, Game#game.players, []),
                YourTurn = Pid =:= Game#game.turn,
                PileTop = case Game#game.pile of 
                    [] -> null; 
                    [Top | _] -> 
                        {Color, Value} = Top,
                        DisplayColor = case Value of
                            wild -> Game#game.current_color;
                            draw4 -> Game#game.current_color;
                            _ -> Color
                        end,
                        [atom_to_binary(DisplayColor, utf8), card_value_to_binary(Value)]
                end,
                ShowUno = length(Hand) =:= 2,
                Confetti = Game#game.uno_declared =:= Pid andalso length(Hand) =:= 1,
                MsgMap = #{
                    type => <<"state">>,
                    hand => lists:map(fun({Color, Value}) ->
                        [atom_to_binary(Color, utf8), card_value_to_binary(Value)]
                    end, Hand),
                    top_card => PileTop,
                    your_turn => YourTurn,
                    winner => WinnerName,
                    show_uno_button => ShowUno,
                    confetti => Confetti
                },
                Pid ! {text, jsx:encode(MsgMap)};
            false -> ok
        end
    end, maps:keys(Game#game.players)).
