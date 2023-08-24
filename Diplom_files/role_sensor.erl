-module(role_sensor).
-behaviour(role_worker).
-export([start/3, stop/1, to_term/3, from_term/2, ctrl/2, open_connection/2]).

stop(_) -> ok.

start(Role_ID, Mod_ID, MM) ->
    role_worker:start(?MODULE, Role_ID, Mod_ID, MM).

ctrl(_,Cfg) -> Cfg.


to_term(Tail, Chunk, Cfg) ->
  L = list_to_binary([Tail, Chunk]),
  Regexp1 = ([^ ]*),([^ ]*),([^ ]*),
  {match, [R1, P2, Yaw]} = re:run(L, Regexp1, [{capture, [1,2,3], binary}]),
  
  [[{gyro_data, {R1, P2, Yaw "перевести во float"}}], [], [], <<>>, Cfg];
  
   
  case binary:split(L, [<<"\r\n">>, <<"\n">>]) of 
    <<"sensor_data(f)">> ->
      [[{get_sensor_data, {file, FilePath}}], [], [], <<>>, Cfg];
    <<"sensor_data(s)">> ->
      [[{get_sensor_data, {sensor, _}}], [], [], <<>>, Cfg];
    _ ->
      [[{raw, L}], [], [], <<>>, Cfg]
  end.

% Sensor_data - реализуемая команда
% Доделать обработку данных для case-конструкции (done)
% Рассмотреть парсинг в role_at/role_ml

from_term(_, _)             -> {error, term_not_supported}.
