-module(role_test).
-behaviour(role_worker).
-export([start/3, stop/1, to_term/3, from_term/2, ctrl/2, open_connection/2]).

stop(_) -> ok.

start(Role_ID, Mod_ID, MM) ->
    role_worker:start(?MODULE, Role_ID, Mod_ID, MM).

ctrl(_,Cfg) -> Cfg.


to_term(Tail, Chunk, Cfg) ->

  L = list_to_binary([Tail, Chunk]), 
  case binary:split(L, [<<"\r\n">>, <<"\n">>]) of 
  % Переписать команды CSV
    <<"sensor_data_f">> ->
      [[{get_sensor_data, {file, "get_remote_file_data"}}], [], [], <<>>, Cfg];
    <<"sensor_data(s)">> ->
      [[{get_sensor_data, {sensor, <<"get_remote_sensor_data">>}}], [], [], <<>>, Cfg];
    _ ->
      io:format("Else ~p~n", []),
      [[{raw, L}], [], [], <<>>, Cfg]
  end.

% Sensor_data - реализуемая команда
% Доделать обработку данных для case-конструкции (done)
% Рассмотреть парсинг в role_at/role_ml

from_term(_, _)             -> {error, term_not_supported}.
