-module(tcp_client).

-export([parse/1]).
-export([start/2]).

-define(TCP_OPTIONS, [list, {active, false}]). % active false - вечное чтение


parse("") ->
    [];
parse(String) ->
    Binary = erlang:iolist_to_binary(String),
    case binary:split(Binary, <<",">>, [global]) of
        [Token1, Token2 | Tokens] -> [binary_to_list(Token1) | parse(binary_to_list(Token2)) ++ [binary_to_list(Token) || Token <- Tokens]];
        [Token] -> [binary_to_list(Token)];
        [] -> []
    end.

send_data(Socket, Msg) ->
    gen_tcp:send(Socket, list:flatten([Msg, "\r\n"])).
    
read_data(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [Data]),
            read_data(Socket);
        {error, Reason} ->
            io:format("Error reading data: ~p~n", [Reason])
    end.

start(IP, Port) ->
    case gen_tcp:connect(IP, Port, ?TCP_OPTIONS) of
        {ok, Socket} ->
            io:format("Socket: ~p~n", [Socket]),
            %loop(Socket);
            read_data(Socket);
        {error, Reason} ->
            io:format("Wrong connection: ~p~n", [Reason])
    end.
    
%start(IP, Port, Msg) ->
%    case gen_tcp:connect(IP, Port, ?TCP_OPTIONS) of
%        {ok, Socket} ->
%            io:format("Socket: ~p~n", [Socket]),
%            gen_tcp:send(Socket, Msg),
%            inet:setopts(Socket, [{active, true}]),
%            loop(Socket);
%        {error, Reason} ->
%             io:format("Wrong connection: ~p~n", [Reason])
%    end.


%loop(Socket) ->
%    case user_default:read_line("") of
%        {error, _} ->
%            gen_tcp:close(Socket);
%        {ok, "quit\n"} ->
%            gen_tcp:close(Socket);
%        {ok, Message} ->
%            send_data(Socket, Message),
%            loop(Socket)
%   end.



