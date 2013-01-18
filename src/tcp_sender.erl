%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean-Philippe Langlois <jpl@opscode.com>
%% @copyright 2013 Opscode Inc.

%% ------------------------------------------------------------------
%% @doc tcp_sender gen_server = generic tcp client.
%% ------------------------------------------------------------------

-module(tcp_sender).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, send/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% Config: host, port
%% Optionally: connect_timeout_ms (default 2000), tcp_options
%% For tcp_options, see
%% http://www.erlang.org/doc/man/gen_tcp.html#connect-3
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

send(Pid, Message) ->
    gen_server:call(Pid, {send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, { socket :: inet:socket() }).

init(Config) ->
    error_logger:info_msg("tcp_sender starting with ~p~n", [Config]),
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config),
    ConnectTimeout = proplists:get_value(connect_timeout_ms, Config, 2000),
    TcpOptions = proplists:get_value(tcp_options, Config,
                                     [binary, {packet, raw}, {active, false},
                                      {keepalive, true}, {nodelay, true},
                                      {send_timeout, 2000}]),
    case gen_tcp:connect(Host, Port, TcpOptions, ConnectTimeout) of
        {ok, Socket} ->
            State = #state{socket = Socket},
            {ok, State};
        {error, Reason} ->
            error_logger:error_msg("Failed to connect to server on ~s:~B: ~p~n", [Host, Port, Reason]),
            {stop, {error, failed_connect}}
    end.
handle_call({send, Message}, _From, #state{socket = Socket} = State) ->
    error_logger:info_msg("sending: ~p~n", [Message]),
    case gen_tcp:send(Socket, Message) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            case Reason of
                timeout ->
                    error_logger:error_msg("Timed out sending.");
                closed ->
                    error_logger:error_msg("Socket disconnected unexpectedly.");
                Reason ->
                    error_logger:error_msg("Unexpected error occurred while sending: ~p~n", [Reason])
            end,
            {stop, {shutdown, send_error}, State}
    end;
handle_call(Request, _From, State) ->
    error_logger:info_msg("Unexpected message: handle_call ~p", [Request]),
    {noreply, ok, State}.

handle_cast(Msg, State) ->
    error_logger:info_msg("Unexpected message: handle_cast ~p", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_msg("Unexpected message: handle_info ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined ->
            ok;
        _Else ->
            gen_tcp:close(Socket)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
