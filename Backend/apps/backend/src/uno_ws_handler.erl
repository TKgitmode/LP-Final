-module(uno_ws_handler).
-behaviour(cowboy_websocket).
-include("include/uno_game.hrl").

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(GAME_TABLE, uno_state).
-define(NAME_TABLE, uno_name).

%%% INIT

init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    ensure_game_table(),
    ensure_name_table(),
    maybe_init_game(),
    {ok, State}.

%%% Función para convertir el valor de la carta a binary

card_value_to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
card_value_to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8).

%%% HANDLE INCOMING MESSAGES

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
        #{<<"type">> := <<"play">>, <<"card">> := [ColorBin, ValueBin]} ->
            try
                Color = binary_to_existing_atom(ColorBin, utf8),
                % Intentamos convertir el valor a entero, si falla asumimos átomo
                Value = case catch binary_to_integer(ValueBin) of
                    {'EXIT', _} -> binary_to_existing_atom(ValueBin, utf8);
                    Int -> Int
                end,
                Card = {Color, Value},
                [{game, Game0}] = ets:lookup(?GAME_TABLE, game),

                case Game0#game.turn =:= self() of
                    true ->
                        Game1 = uno_game:play_card(Game0, self(), Card),
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
                        {NewGame, Card} = uno_game:draw_card(Game0, self()),
                        ets:insert(?GAME_TABLE, {game, NewGame}),
                        % Enviar carta robada al jugador
                        self() ! {text, jsx:encode(#{type => <<"draw_result">>, card => [atom_to_binary(element(1, Card), utf8), card_value_to_binary(element(2, Card))]})},
                        broadcast_state(NewGame),
                        {ok, State}
                    catch
                        _:_ ->
                            {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Deck is empty or cannot draw">>})}, State}
                    end;
                false ->
                    {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Not your turn!">>})}, State}
            end;

        _Other ->
            io:format("Unknown message: ~p~n", [Msg]),
            {reply, {text, jsx:encode(#{type => <<"error">>, message => <<"Unknown message type">>})}, State}
    end;

websocket_handle(_, State) ->
    {ok, State}.

%%% INFO (GENERIC)

websocket_info({text, Data}, State) ->
    {reply, {text, Data}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
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
    WinnerName =
        case Game#game.winner of
            undefined -> null;
            WPid ->
                case ets:lookup(?NAME_TABLE, WPid) of
                    [{_, WName}] -> list_to_binary(WName);
                    _ -> list_to_binary(erlang:pid_to_list(WPid))
                end
        end,

    lists:foreach(fun(Pid) ->
        Hand = maps:get(Pid, Game#game.players, []),
        YourTurn = Pid =:= Game#game.turn,
        PileTop = case Game#game.pile of 
            [] -> null; 
            [Top | _] -> 
                {Color, Value} = Top,
                [atom_to_binary(Color, utf8), card_value_to_binary(Value)]
        end,
        MsgMap = #{
            type => <<"state">>,
            hand => lists:map(fun({Color, Value}) -> 
                [atom_to_binary(Color, utf8), card_value_to_binary(Value)] 
            end, Hand),
            top_card => PileTop,
            your_turn => YourTurn,
            winner => WinnerName
        },
        Pid ! {text, jsx:encode(MsgMap)}
    end, maps:keys(Game#game.players)).
