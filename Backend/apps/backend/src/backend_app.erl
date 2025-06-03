-module(backend_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", uno_ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener, 
        [{port, 8080}], 
        #{env => #{dispatch => Dispatch}}
    ),
    io:format("Server listening on port 8080~n"),
    backend_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.