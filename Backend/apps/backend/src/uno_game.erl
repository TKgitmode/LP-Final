-module(uno_game).
-include("include/uno_game.hrl").

-export([new/0, shuffle/1, add_player/2, get_state/1, play_card/3, valid_play/2, draw_card/2, reset/1, declare_uno/2]).

%%% Crear un mazo estándar de cartas (con especiales y comodines)
generate_deck() ->
    Colors = [red, green, blue, yellow],
    Numbers = lists:seq(0,9),
    Specials = [skip, reverse, draw2],
    NumberCards = [{Color, Num} || Color <- Colors, Num <- Numbers],
    SpecialCards = [{Color, S} || Color <- Colors, S <- Specials],
    WildCards = lists:duplicate(4, {wild, wild}),
    Draw4Cards = lists:duplicate(4, {wild, draw4}),
    NumberCards ++ SpecialCards ++ WildCards ++ Draw4Cards.

new() ->
    rand:seed(exsplus, os:timestamp()),
    Deck = generate_deck(),
    ShuffledDeck = shuffle(Deck),
    #game{
        players = #{},
        deck = ShuffledDeck,
        pile = [],
        turn = undefined,
        winner = undefined,
        direction = 1,
        uno_declared = undefined
    }.

shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    {Elem, Rest} = take_random(List),
    shuffle(Rest, [Elem | Acc]).

take_random(List) ->
    Len = length(List),
    Pos = rand:uniform(Len),
    take_at(Pos, List).

take_at(1, [H|T]) -> {H, T};
take_at(N, [H|T]) when N > 1 ->
    {Elem, Rest} = take_at(N - 1, T),
    {Elem, [H|Rest]}.

add_player(Game, Pid) ->
    {Hand, NewDeck} = draw_cards(7, Game#game.deck, []),
    NewPlayers = maps:put(Pid, Hand, Game#game.players),
    NewGame = Game#game{players = NewPlayers, deck = NewDeck},
    case Game#game.turn of
        undefined -> NewGame#game{turn = Pid};
        _ -> NewGame
    end.

draw_cards(0, Deck, Acc) -> {lists:reverse(Acc), Deck};
draw_cards(_, [], Acc) -> {lists:reverse(Acc), []};
draw_cards(N, [H|T], Acc) -> draw_cards(N-1, T, [H|Acc]).

get_state(Game) -> Game.

next_player(CurrentPid, PlayerMap, Direction) ->
    Pids = maps:keys(PlayerMap),
    Pos = find_player_index(Pids, CurrentPid),
    Len = length(Pids),
    NextPos = case Direction of
        1 -> (Pos rem Len) + 1;
        -1 -> (Pos - 2 + Len) rem Len + 1
    end,
    lists:nth(NextPos, Pids).

find_player_index([Player|_], Player) -> 1;
find_player_index([_ | T], Player) -> 1 + find_player_index(T, Player).

play_card(Game, Pid, Card) ->
    Hand = maps:get(Pid, Game#game.players, []),
    case lists:member(Card, Hand) of
        false -> throw({invalid_play, not_in_hand});
        true ->
            % Validar solo si ya hay una carta en el pile
            Valid = case Game#game.pile of
                [] -> true;
                [TopCard | _] -> valid_play(Card, TopCard, Game#game.current_color)
            end,

            case Valid of
                false -> throw({invalid_play, not_matching});
                true ->
                    NewHand = lists:delete(Card, Hand),
                    {Color, Value} = Card,
                    NewPile = [Card | Game#game.pile],
                    Dir0 = Game#game.direction,
                    Players1 = maps:put(Pid, NewHand, Game#game.players),

                    {Players2, NewDeck2} =
                        case length(NewHand) =:= 1 andalso Game#game.uno_declared =/= Pid of
                            true ->
                                {PenaltyCards, DeckAfterPenalty} = draw_cards(2, Game#game.deck, []),
                                UpdatedHand = NewHand ++ PenaltyCards,
                                {maps:put(Pid, UpdatedHand, Players1), DeckAfterPenalty};
                            false ->
                                {Players1, Game#game.deck}
                        end,

                    Winner = case maps:get(Pid, Players2) of [] -> Pid; _ -> undefined end,

                    %% Captura el nuevo color actual (si es wild o draw4 lo eligió el cliente)
                    ChosenColor = case Value of
                        wild -> Color;
                        draw4 -> Color;
                        _ -> Color
                    end,

                    BaseGame = Game#game{
                        players = Players2,
                        pile = NewPile,
                        winner = Winner,
                        deck = NewDeck2,
                        uno_declared = undefined,
                        current_color = ChosenColor
                    },

                    case Value of
                        skip ->
                            SkipPid = next_player(Pid, Players2, Dir0),
                            NextPid = next_player(SkipPid, Players2, Dir0),
                            BaseGame#game{turn = NextPid};
                        reverse ->
                            Dir = -1 * Dir0,
                            PlayerCount = maps:size(Players2),
                            NextPid = case PlayerCount of
                                2 -> Pid;
                                _ -> next_player(Pid, Players2, Dir)
                            end,
                            BaseGame#game{turn = NextPid, direction = Dir};
                        draw2 ->
                            Target = next_player(Pid, Players2, Dir0),
                            {Drawn, NewDeck3} = draw_cards(2, NewDeck2, []),
                            THand = maps:get(Target, Players2, []) ++ Drawn,
                            UpdatedPlayers = maps:put(Target, THand, Players2),
                            NextPid = next_player(Target, UpdatedPlayers, Dir0),
                            BaseGame#game{
                                players = UpdatedPlayers,
                                deck = NewDeck3,
                                turn = NextPid
                            };
                        draw4 ->
                            Target = next_player(Pid, Players2, Dir0),
                            {Drawn4, NewDeck4} = draw_cards(4, NewDeck2, []),
                            THand4 = maps:get(Target, Players2, []) ++ Drawn4,
                            UpdatedPlayers4 = maps:put(Target, THand4, Players2),
                            NextPid4 = next_player(Target, UpdatedPlayers4, Dir0),
                            BaseGame#game{
                                players = UpdatedPlayers4,
                                deck = NewDeck4,
                                turn = NextPid4
                            };
                        _ ->
                            NextPid = next_player(Pid, Players2, Dir0),
                            BaseGame#game{turn = NextPid}
                    end
            end
    end.


valid_play({wild, _}, _) -> true;
valid_play({C1, V1}, {C2, V2}) -> C1 =:= C2 orelse V1 =:= V2 orelse V2 =:= none.
valid_play({Color, Value}, {TopColor, TopValue}, CurrentColor) ->
    Value == TopValue orelse
    Color == TopColor orelse
    Value == wild orelse
    Value == draw4 orelse
    Color == CurrentColor.

draw_card(Game, Pid) ->
    case maps:get(Pid, Game#game.players, undefined) of
        undefined ->
            throw({error, player_not_found});
        Hand ->
            case Game#game.deck of
                [] ->
                    case Game#game.pile of
                        [] -> {Game, none};
                        [Top | RestPile] ->
                            NewDeck = shuffle(RestPile),
                            NewGame = Game#game{
                                deck = NewDeck,
                                pile = [Top]
                            },
                            {NewerGame, Card} = draw_card(NewGame, Pid),
                            {NewerGame, {reshuffled, Card}}
                    end;
                [Card | RestDeck] ->
                    NewHand = [Card | Hand],
                    NewPlayers = maps:put(Pid, NewHand, Game#game.players),
                    TopCard = case Game#game.pile of
                        [] -> {none, none};
                        [Top | _] -> Top
                    end,
                    CanPlay = valid_play(Card, TopCard),
                    Dir = Game#game.direction,
                    NextTurn = if
                        CanPlay -> Pid;
                        true -> next_player(Pid, NewPlayers, Dir)
                    end,
                    {Game#game{
                        players = NewPlayers,
                        deck = RestDeck,
                        turn = NextTurn
                    }, Card}
            end
    end.

reset(Game) ->
    rand:seed(exsplus, os:timestamp()),
    Deck = shuffle(generate_deck()),
    PlayerPids = maps:keys(Game#game.players),
    {NewPlayers, FinalDeck} = deal_hands(PlayerPids, Deck, #{}),
    FirstPid = case PlayerPids of
        [Pid | _] -> Pid;
        [] -> undefined
    end,
    Game#game{
        players = NewPlayers,
        deck = FinalDeck,
        pile = [],
        turn = FirstPid,
        winner = undefined,
        direction = 1,
        uno_declared = undefined
    }.

deal_hands([], Deck, Acc) -> {Acc, Deck};
deal_hands([Pid | Rest], Deck, Acc) ->
    {Hand, NewDeck} = draw_cards(7, Deck, []),
    deal_hands(Rest, NewDeck, maps:put(Pid, Hand, Acc)).

declare_uno(Game, Pid) ->
    case maps:get(Pid, Game#game.players, []) of
        [_Card1, _Card2] ->
            Game#game{uno_declared = Pid};
        _ ->
            Game
    end.
