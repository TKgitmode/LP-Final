-module(uno_game).
-include("include/uno_game.hrl").

-export([new/0, shuffle/1, add_player/2, get_state/1, play_card/3, valid_play/2, draw_card/2]).

%%% Crear un mazo estándar de cartas (con especiales)
generate_deck() ->
    Colors = [red, green, blue, yellow],
    Numbers = lists:seq(0,9),
    Specials = [skip, reverse, draw2],
    NumberCards = [{Color, Num} || Color <- Colors, Num <- Numbers],
    SpecialCards = [{Color, S} || Color <- Colors, S <- Specials],
    NumberCards ++ SpecialCards.

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
        direction = 1
    }.

shuffle(List) ->
    shuffle(List, []).
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
            TopCard = case Game#game.pile of [] -> {none, none}; [Top|_] -> Top end,
            case valid_play(Card, TopCard) of
                false -> throw({invalid_play, not_matching});
                true ->
                    NewHand = lists:delete(Card, Hand),
                    NewPlayers = maps:put(Pid, NewHand, Game#game.players),
                    NewPile = [Card | Game#game.pile],
                    Winner = case NewHand of [] -> Pid; _ -> undefined end,
                    {Color, Value} = Card,
                    Dir0 = Game#game.direction,
                    case Value of
                        skip ->
                            SkipPid = next_player(Pid, NewPlayers, Dir0),
                            NextPid = next_player(SkipPid, NewPlayers, Dir0),
                            Game#game{
                                players = NewPlayers, pile = NewPile,
                                turn = NextPid, winner = Winner
                            };
                        reverse ->
                            Dir = -1 * Dir0,
                            NextPid = next_player(Pid, NewPlayers, Dir),
                            Game#game{
                                players = NewPlayers, pile = NewPile,
                                turn = NextPid, winner = Winner, direction = Dir
                            };
                        draw2 ->
                            Target = next_player(Pid, NewPlayers, Dir0),
                            {Drawn, NewDeck} = draw_cards(2, Game#game.deck, []),
                            THand = maps:get(Target, NewPlayers, []) ++ Drawn,
                            UpdatedPlayers = maps:put(Target, THand, NewPlayers),
                            NextPid = next_player(Target, UpdatedPlayers, Dir0),
                            Game#game{
                                players = UpdatedPlayers, pile = NewPile,
                                deck = NewDeck, turn = NextPid, winner = Winner
                            };
                        _ ->
                            NextPid = next_player(Pid, NewPlayers, Dir0),
                            Game#game{
                                players = NewPlayers, pile = NewPile,
                                turn = NextPid, winner = Winner
                            }
                    end
            end
    end.

valid_play({C1, V1}, {C2, V2}) -> C1 =:= C2 orelse V1 =:= V2 orelse V2 =:= none.

%% Robar una carta del mazo y agregarla a la mano del jugador
draw_card(Game, Pid) ->
    case maps:get(Pid, Game#game.players, undefined) of
        undefined ->
            throw({error, player_not_found});
        Hand ->
            case Game#game.deck of
                [] -> % Mazo vacío, no se roba carta
                    {Game, none};
                [Card | RestDeck] ->
                    NewHand = [Card | Hand],
                    NewPlayers = maps:put(Pid, NewHand, Game#game.players),
                    TopCard = case Game#game.pile of
                        [] -> {none, none};
                        [Top | _] -> Top
                    end,
                    % Revisar si se puede jugar la carta
                    CanPlay = valid_play(Card, TopCard),
                    Dir = Game#game.direction,
                    % Si no puede jugar, pasa turno
                    NextTurn = if
                        CanPlay -> Pid;
                        true -> next_player(Pid, NewPlayers, Dir)
                    end,
                    {% Actualiza estado y devuelve la carta robada
                     Game#game{
                        players = NewPlayers,
                        deck = RestDeck,
                        turn = NextTurn
                     }, Card}
            end
    end.
