-record(game, {
    players,      %% mapa Pid => Mano
    deck,         %% lista de cartas restantes
    pile,         %% pila de descarte
    turn,         %% pid del turno actual
    winner,       %% pid ganador
    direction = 1 %% 1 o -1 para indicar dirección del turno
}).
