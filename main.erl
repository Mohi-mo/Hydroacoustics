-module(main).
%-import(gold, [encode_gold/3, decode_gold/3]).
-export([bit_write/3, bit_read/3, ip_creator/0, parse_packet/1]).


% Формирование - отправка пакета
% Редкатирование заголовка / данных (done)
% Шифрование Голдом

% Принятие пакета
% Дешифровка
% Парсинг

% A = исходное значение в битах
% Mask = некоторая маска
% BS = Значение, которое нужно добавить
% PosBS = позиция в битах, куда нужно добавить
% StSize - кол-во изменяемых бит
%
% 0 8 16 24 32 - шпаргалка
%
% ---------------------------------------------------------------------------------------------------------



bit_write(Num, DestNum, Pos) ->
% Запись:
%     А: Подготовка исходного числа
% 	1. Исходное значение
%	2. Сдвиг влево на StSize (кол-во изменяемых бит)
%	3. Инвертирование (лог НЕ) для формирования маски с учётом StSize (кол-во изменяемых бит)
% 	4. Обратный сдвиг маски (чтобы маска была соотнесена с позицией исходного значения)
%	5. Инвертирование (лог НЕ) маски
% 	6. Логическое И маски с исходным значением для его обнуления
%
%     Б: Формирование нужного значения 
%	7. Сдвиг исходного значения на PosBS
% 	8. Логическое ИЛИ получившегося обнулённого исходного значения с нужным
%
%  Итог: перезаписано определённое кол-во бит на определённой позиции

   BinChSym = integer_to_list(DestNum, 2),
   ChSymNum = length(BinChSym),
   Mask = 1023 bsl ChSymNum,
   SMask = bnot ((bnot Mask) bsl (Pos-1)), 
   Zer = Num band SMask,
   SDes_Num = DestNum bsl (Pos-1),
   Result = Zer bor SDes_Num,

   io:format("Result = \~p\~n" , [integer_to_list(Result,2)]),
   io:format("Position =  \~p\~n" , [ChSymNum]).


bit_read(A, PosBs, StSize) ->
% Чтение:
% 	1. Исходное значение
%	2. Сдвиг вправо к PosBS  (позиции, которую нужно прочитать)
%	3. Формирование маски с учётом StSize (кол-во бит, которые нужно прочитать)
%	4. Логическое И со сдвинутым значением и маской
%
% Итог: получен желаемый элемент

   N = PosBs - 1,
   Shift = A bsr N,
   Mask = bnot (1023 bsl StSize), 
   Res = Shift band Mask,

  % bin_Res = list_to_integer(Res,2),
   io:format("Result = \~p\~n" , [integer_to_list(Res,2)]),
   io:format("Mask =  \~s\~n" , [io_lib:format("~.2B",[Mask])]).


% Парсинг IP-пакета --------------------------------------------------------------------
parse_packet(BinPacket) ->
    <<VerIhl:8, TOS:8, PacketLen:16, ID:16, FlagsFragOff:16, TTL:8,
      Protocol:8, Checksum:16, SrcAddr:32, DestAddr:32, _/binary>> = BinPacket,
    Version = VerIhl band 16#f0,
    HeaderLen = (VerIhl band 16#0f) * 4, % умножаем на 4, так как размер задается в 32-битных словах
    Flags = FlagsFragOff band 16#e000,
    FragmentOffset = FlagsFragOff band 16#1fff,
    {Version, HeaderLen, TOS, PacketLen, ID, Flags, FragmentOffset, TTL, Protocol, Checksum, inet:ntoa([SrcAddr]), inet:ntoa([DestAddr])},
    
    case Protocol of
    6 -> % TCP-сегмент
        <<TcpHeader:HeaderLen/binary, Payload/binary>> = binary:part(BinPacket, HeaderLen + 1, PacketLen - HeaderLen),
      {SrcPort, DestPort, SeqNum, AckNum, DataOffset, Flags, WindowSize, Checksum, UrgentPtr} = binary:decode_unsigned(TcpHeader, [network, unsigned, 16, network, unsigned, 16, network, unsigned, 32, network, unsigned, 32, network, unsigned, 4, network, unsigned, 12, network, unsigned, 16, network, unsigned, 16, network, unsigned, 16]),
        
      %PayloadText = binary_to_list(Payload),
      %io:format("TCP сегмент: ~p ~> ~p, длина: ~p, данные: ~s~n", [SrcPort, DestPort, length(Payload), PayloadText]);
        AtomPayload = binary_to_existing_atom(Payload, utf8),
        io:format("Payload: ~p~n", [AtomPayload]);
    _ -> % Какой-то другой протокол верхнего уровня
        ok
    end.
   

calculate_checksum(Header) ->
    %% Преобразуем бинарный заголовок к списку байтов
    Bytes = binary_to_list(Header),

    %% Добавляем нулевой байт, если длина нечетная
    Bytes1 = case length(Bytes) rem 2 of
                  1 -> Bytes ++ [0];
                  _ -> Bytes
            end,

    %% Разбиваем список байтов на 16-битные слова
    Words = lists:flatmap(fun(X) -> [X bsr 8, X band 16#ff] end, Bytes1),

    %% Суммируем 16-битные слова
    Sum = lists:foldl(fun(X, Acc) -> Acc + X end, 0, Words),

    %% Складываем биты переноса
    Checksum = (Sum band 16#ffff) + (Sum bsr 16),

    %% Инвертируем результат
    final_checksum(Checksum).

%% Инвертирует биты контрольной суммы
final_checksum(Checksum) ->
    Final = ((Checksum bsl 8) band 16#ff00) bor ((Checksum bsr 8) band 16#00ff),
    Final band 16#ffff.

ip_creator() ->
  
   Version = 4, % Версия протокола (IPv4) 					4 бита
   HeaderLen = 5, % Длина заголовка IP-пакета (в 32-битных словах)		4 бита
   Tos = 0, % Type of Service (0 = Normal)					8 бит
   TotalLen = 20 + 10, % Общая длина IP-пакета (заголовок + 10 байт данных)	16 бит
   Id = 0, % Идентификатор							16 бит
   Flags = 0, % Флаги								3 бита
   FragOffset = 0, % Смещение фрагмента						13 бит
   Ttl = 64, % Время жизни (TTL)						8 бит
   Protocol = 6, % Протокол следующего уровня (TCP = 6, UDP = 17)		8 бит
   %Checksum = 0, % Контрольная сумма (неизвестна на этом этапе)			16 бит
   SourceIP = "192.168.0.1", % IP-адрес отправителя
   DestinationIP = "192.168.0.2", % IP-адрес получателя

    Header = <<Version:4, HeaderLen:4, 0:8, TotalLen:16, Id:16, Flags:3, FragOffset:13, Ttl:8, Protocol:8, 0:16, 192:8, 168:8, 0:8, 1:8, 192:8, 168:8, 0:8, 2:8>>,
      
    % Получаем контрольную сумму заголовка
    Checksum = calculate_checksum(Header),
    
    % Изменяем соответствующий байт заголовка
    IpHeaderWithChecksum = <<Version:4, HeaderLen:4, Tos:8, TotalLen:16, Id:16, Flags:3, FragOffset:13, Ttl:8, Protocol:8, Checksum:16, 192:8, 168:8, 0:8, 1:8, 192:8, 168:8, 0:8, 2:8>>,
    
    % Формирование полезной нагрузки IP-пакета (10 байт данных)
    Payload = <<"Hello">>,
    
    % Соединяем заголовок и полезную нагрузку
    Packet = <<IpHeaderWithChecksum/binary, Payload/binary>>.
  
   
