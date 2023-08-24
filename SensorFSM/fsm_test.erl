-module(fsm_test).
-compile({parse_transform, pipeline}).
-behaviour(fsm).

-include_lib("evins/include/fsm.hrl").

-export([start_link/1, trans/0, final/0, init_event/0]).
-export([init/1, handle_event/3, stop/1]).
-export([handle_idle/3, handle_alarm/3]).
-export([handle_choose_source/3]).


% Cостояния: reading_data - чтение данных из датчика, send_data - отправка ответа, когда данные собраны

% reading_data ---> idle            error
% reading_data ---> send_data       success

% reading_data ----> reading_data   get_data_from_file
% reading_data ----> reading_data   get_data_from_sensor

% idle         ----> idle           internal
% idle         ----> reading_data   get_data_from_file
% idle         ----> reading_data   get_data_from_sensor

% send_data    ----> idle           data_transmited
% send_data    ----> reading_data   timeout_get_data 

% send_data    ----> send_data (set timeout)  get_data_from_file
% send_data    ----> send_data (set timeout)  get_data_from_sensor


% {состояние, [{событие1, состояние1}, {событие1, состояние1}, ...{...}]}

-define(TRANS, [
  {idle, [{internal, idle}, 
          {reading_data, send_data}]},
  {reading_data, [{internal, send_data},
                  {internal, get_data_from_file},
                  {internal, get_data_from_sensor}]}, 
  {send_data, [{internal, idle}]},
  {alarm, []}
]).

start_link(SM) -> fsm:start_link(SM).
init(SM)       -> SM.
trans()        -> ?TRANS.
final()        -> [alarm].
init_event()   -> internal.
stop(_SM)      -> ok.

%% Old ---------------------------------------------------------------
%handle_event(MM, SM, Term) ->
%  io:format("EVENT = ~p~n", [Term]),
%  Local_address = share:get(SM, local_address),
%  case Term of
%    {timeout, Event} ->
%      fsm:run_event(MM, SM#sm{event=Event}, {});
%    {raw, D} ->
%      io:format("MyData = ~p~n", [D]),
%      SM;
%    {get_sensor_data} ->
%      % Сторона источника - расширяемая команда, введённая в интерфейс
%      send_at_get_sensor_data(SM, 2, <<"get_sensor_data_from_file">>);
%    {async,{pid,Pid},{recvim,_,A,Dst,_,_,_,_,_,D}} when A == Local_address ->	 
%      share:put(SM, {destination, Dst}),
%     fsm:run_event(MM, SM#sm{event=reading_data}, {}); 
%        % Парсинг ответа get_sensor_from_file в D (done?)
%         % Дополнить проверкой, откуда взяты данные (done?)
%       case D of
%        <<"get_sensor_data_from_file">> ->
%         Data = read_sensor_data_from_file(),
%         fsm:run_event(MM, fsm:set_event(SM, Data), {});
%          
%        <<"get_sensor_data_from_sensor">>->
%         Data = read_sensor_data_from_sensor(),
%          fsm:run_event(MM, fsm:set_event(SM, Data), {});
%        _ ->
%          io:format("Unhandled data event = ~p~n", [D]),
%          SM
%      end;
%    Term ->
%      io:format("Unhandled event = ~p~n", [Term]),
%      SM
%  end.

% New----------------------------------------------------------------------
handle_event(MM, SM, Term) ->
  io:format("EVENT = ~p~n", [Term]),
  Local_address = share:get(SM, local_address),
  State = SM#sm.state,
  
  case Term of
     {timeout, get_data_from_file} ->
      % Обработка: продление тайм-аута (если в том же состоянии), повторная попытка отправки (при переходе в idle)
      fsm:run_event(MM, SM#sm{event=Event}, {}),  
      SM;
      % ...
    {timeout, Event} ->
      fsm:run_event(MM, SM, {}); % Обход {timeout,answer_timeout} - игнорирование всех тайм-аутов, кроме вышеописанных
    {raw, D} ->
      io:format("MyData = ~p~n", [D]),
      SM;
    %{get_sensor_data} ->
      %send_at_get_sensor_data(SM, 2, <<"get_sensor_data_from_file">>);
    {gyro_data, {Roll, Pitch, Yaw}} ->
      share:put(SM, gyro_data, {Roll, Pitch, Yaw}),
      SM;
     
    {async,{pid,Pid},{recvim,_,A,Dst,_,_,_,_,_,CMD}} when A == Local_address->	 % Пришедшая команда по акустике с запросом
      share:put(SM, destination, Dst),
      case CMD of
      <<"get_remote_sensor_data">> ->
         Data = read_sensor_data_from_sensor(),
         io:format("Your data: ~p~n", [Data]),
         share:put(SM, last_file_data, Data),
         fsm:run_event(MM, fsm:set_event(SM, get_data_from_sensor), {State});
      <<"get_remote_file_data">> ->
         Data = read_sensor_data_from_file(FilePath),
         share:put(SM, last_sensor_data, Data),
         fsm:run_event(MM, fsm:set_event(SM, get_data_from_file), {State});
      _->
        SM
      end;
      
    {get_sensor_data, {sensor, CMD}} ->
      send_at_get_sensor_data(SM, 4, CMD);
      
    {get_sensor_data, {file, CMD}} ->
      send_at_get_sensor_data(SM, 4, CMD);
      
    Term ->
      io:format("Unhandled event = ~p~n", [Term]),
      SM
  end.

%% Обработчик idle и alarm-----------------------------------------------------------
%handle_idle(_MM, SM, eps) ->
%  fsm:run_event(_MM, fsm:set_event(SM, send_data), {});
%handle_idle(_MM, SM, _Term) ->
%  SM.

% reading_data ----> idle           error
% idle         ----> idle           internal
% send_data    ----> idle           data_transmited


% idle         ----> idle           internal
% idle         ----> reading_data   get_data_from_file
% idle         ----> reading_data   get_data_from_sensor


handle_idle(_MM, #sm{event = internal} = SM, _Term) ->
  fsm:set_event(SM, eps);
handle_idle(_MM, #sm{event = error} = SM, _Term) ->
  fsm:set_event(SM, eps); % gen_error 
handle_idle(_MM, #sm{event = data_transmited} = SM, _Term) ->
  fsm:set_event(SM, eps);
handle_idle(_MM, SM, _Term) ->
  SM.


% reading_data ---> idle            error
% reading_data ---> send_data       success

% reading_data ----> reading_data   get_data_from_file
% reading_data ----> reading_data   get_data_from_sensor
% send_data    ----> reading_data   timeout_get_data


handle_reading_data(_MM, #sm{event = Event} = SM, {reading_data}) when Event == get_data_from_file; Event == get_data_from_sensor ->
   % Busy рассчёт timeout / постоянный T   
   %fsm:clear_timeout(__, backoff),
   %update_backoff(__, decrement)	
   fsm:set_event(SM, eps); 
handle_reading_data(_MM, #sm{event = get_data_from_file} = SM, _Term) ->
  {Event, Bin} = read_sensor_data_from_file(), % Event = ok/error
  share:put(SM, last_bin_data, Bin),
  fsm:set_event(SM, Event);
handle_reading_data(_MM, #sm{event = get_data_from_sensor} = SM, _Term) ->
  {Event, Bin} = read_sensor_data_from_sensor(), % Event = ok/error
  share:put(SM, last_bin_data, Bin),
  fsm:set_event(SM, Event);
handle_reading_data(_MM, SM, _Term) ->
  SM.

% reading_data ----> send_data                 success
% send_data    ----> send_data (set timeout)  get_data_from_file
% send_data    ----> send_data (set timeout)  get_data_from_sensor

handle_send_data(_MM,  #sm{event = success} = SM, _Term)->
   A = share:get(SM, destination),
   Bin = share:get(SM, last_bin_data),
   fsm:cast(SM, at, {send, {at,{pid,p0},"*SENDIM",A,ack,Bin}});
handle_send_data(_MM,  #sm{event = get_data_from_file} = SM, _Term)->
   fsm:set_timeout(SM#sm{event = eps}, {s, 1}, get_data_from_file); 
handle_send_data(_MM,  #sm{event = get_data_from_sensor} = SM, _Term)->
   fsm:set_timeout(SM#sm{event = eps}, {s, 1}, get_data_from_sensor);
handle_send_data(_MM,  SM, _Term)->
   SM.


handle_alarm(_MM, SM, _Term) ->
  exit({alarm, SM#sm.module}).

%% Чтение данных из файла-----------------------------------------
read_sensor_data_from_file() ->
  Data = read_lines("/mnt/hgsf/GyroLogg.txt"),
  binary_to_list(Data).

read_file(FilePath) ->
    {ok, File} = file:open(FilePath, [read]),
    read_lines(File, 10, []).

read_lines(_, 0, Acc) ->
    lists:reverse(Acc);
read_lines(File, N, Acc) ->
    case file:read_line(File) of
        {ok, Line} ->
            read_lines(File, N - 1, [Line | Acc]);
        eof ->
            lists:reverse(Acc)
    end.

%% Чтение данных напрямую из датчика ------------------------------------
read_sensor_data_from_sensor() ->
  share:get(SM, gyro_data).
 

%% Формирование AT-команды и её отправка----------------------------------------------
send_at_get_sensor_data(SM, A, CMD)->
  % Если не работает p0 --> 0
  fsm:cast(SM, at, {send, {at,{pid,p0},"*SENDIM",A,ack,CMD}}) when is_binary(CMD) == true.

