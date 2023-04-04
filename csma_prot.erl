-module(csma_prot).

-export([start/0]).

% Инициализация условных констант для имитации различных случайных условий передачи
-define(TX_INTERVAL, 500).
-define(COLLISION_WINDOW, 1000).

% Запуск трёх процессов для разных устройств
start() ->
    spawn(fun() -> tx(1) end),
    spawn(fun() -> tx(2) end),
    spawn(fun() -> tx(3) end).

tx(Id) ->
    wait(),
    transmit(Id),
    receive
        {collision, Id} ->
            io:format("~p: Collision detected, waiting and retrying.~n", [Id]),
            wait(),
            tx(Id);
        {ack, Id} ->
            io:format("~p: Acknowledgment received.~n", [Id])
    end,
    tx(Id).

% Имитация случайного времени передачи данных
wait() ->
    timer:sleep(rand:uniform(?TX_INTERVAL)).

% Функция передачи данных
transmit(Id) ->
    io:format("~p: Transmitting data.~n", [Id]),
    timer:sleep(rand:uniform(?COLLISION_WINDOW)),
    ok = check_channel(),
    io:format("~p: Data transmitted successfully.~n", [Id]).

% Проверка активности канала
check_channel() ->
% Ограничение количества попыток повторной передачи
    case rand:uniform(100) of
        N when N < 10 ->
            ok;
        _ ->
            {collision, self()}
    end.

