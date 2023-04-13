-module(csma_aloha).
-export([start/2, connect/2]).

-define(TX_INTERVAL, 500).
-define(COLLISION_WINDOW, 1000).
-define(MAX_RETRIES, 5).
-define(PACKET_SIZE, 512).


start(Host, Port) ->
    io:format("Starting CSMA/Aloha protocol~n"),
    spawn(csma_aloha, transmit, [0, Host, Port]).

connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, [{active, false}, {connect_timeout, 1000}]) of
        {ok, Socket} ->
            io:format("~p: Connected successfully to ~p on port ~p.~n", [self(), Host, Port]),
            Socket;
        {error, Reason} ->
            io:format("~p: Failed to connect to ~p on port ~p. Reason: ~p.~n", [self(), Host, Port, Reason]),
            exit(Reason)
    end.

transmit(RetryCount, Host, Port) ->
    io:format("~p: Transmitting data.~n", [self()]),
    case check_channel(Host) of
        busy ->
            io:format("~p: Channel is busy, waiting for fixed time.~n", [self()]),
            timer:sleep(1000),
            transmit(RetryCount+1, Host, Port);
        free ->
            io:format("~p: Channel is free, transmitting data.~n", [self()]),
            send_data(Host, Port),
            io:format("~p: Data transmitted successfully.~n", [self()])
    end,
    timer:sleep(500),
    transmit(0, Host, Port).

check_channel(Host) ->
    IsActive = is_active(Host),
    case rand:uniform(100) of
        N when N < 50, not IsActive ->
            free;
        _ ->
            busy
    end.

send_data(Host, Port) ->
    Socket = connect(Host, Port),
    gen_tcp:send(Socket, "Hello, World!"),
    ok.


busy() ->
    io:format("~p: Channel is busy.~n", [self()]),
    busy.

free() ->
    io:format("~p: Channel is free.~n", [self()]),
    free.
