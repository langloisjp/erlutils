%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Jean-Philippe Langlois <jpl@opscode.com>
%% @copyright 2013 Opscode Inc.

%% ------------------------------------------------------------------
%% @doc A module to listen for and accept tcp connections, and forward
%% line-based messages to a configured function.
%%
%% Driven from application configuration. Uses the following options:
%% - tcp_listen_port
%% - handler_module
%% - handler_function
%% ------------------------------------------------------------------

-module(tcp_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Port, Fun) ->
    gen_server:start_link(?MODULE, [Port, Fun], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Port, HandlerFunction]) ->
    error_logger:info_msg("tcp_server starting on port ~p~n", [Port]),

    %% we want to know when we're shut down so we can kill
    %% the listening process
    process_flag(trap_exit, true),
    ListenSocket = case gen_tcp:listen(Port, [{active, false}, {packet, line}]) of
        {ok, LS} -> LS;
        Other -> throw({tcp_server_error, cannot_create_listen_socket, Other})
    end,
    Pid = start_listener(ListenSocket, HandlerFunction),
    {ok, Pid}.

terminate(_Reason, ListenerPid) ->
    error_logger:info_msg("tcp_server shutting down...~n"),
    exit(ListenerPid, normal),
    ok.

handle_info(Msg, State) ->
    error_logger:info_msg("tcp_server:handle_info: ~p (state: ~p)~n", [Msg, State]),
    {noreply, State}.

%% gen_server boilerplate
handle_call(_Request, _From, State) ->  {noreply, ok, State}.
handle_cast(_Msg, State) ->             {noreply, State}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Start the connection listener.
start_listener(Socket, Fun) ->
    ServerPid = self(),
    spawn_link(fun() -> wait_for_connection(Socket, Fun, ServerPid) end).

%% Wait for connection on ListenSocket.
%% Spawn process to handle new connection.
%% Repeat.
wait_for_connection(ListenSocket, Fun, ServerPid) ->
    %error_logger:info_msg("tcp_server waiting for connection..."),
    Socket = case gen_tcp:accept(ListenSocket) of
        {error, closed} ->
            error_logger:info_msg("tcp_server:wait_for_connection: listening socket closed"),
            exit(normal);
        {ok, S} -> S
    end,
    {ok, {Address, Port}} = inet:sockname(Socket),
    error_logger:info_msg("tcp_server accepting new connection from ~p:~p (~p)~n", [Address, Port, Socket]),

    Pid = spawn(fun() -> 
        %% link to server Pid so we can know when the process exits
        link(ServerPid),
        handle_connection(Socket, Fun)
    end),

    %% In order for gen_tcp messages to reach the correct
    %% process (the one we just spawned), we need to do this...
    gen_tcp:controlling_process(Socket, Pid),

    %% Now continue to listen for incoming connections...
    wait_for_connection(ListenSocket, Fun, ServerPid).

%% Handle an incoming connection.
handle_connection(Socket, HandlerFunction) ->
    %error_logger:info_msg("tcp_server:handle_connection(~p)", [Socket]),
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            {ok, {Address, Port}} = inet:sockname(Socket),
            %error_logger:info_msg("tcp_server:handle_connection:message:~p~n", [Msg]),
            case catch(HandlerFunction({os:timestamp(), Address, Port, Msg})) of
                Response when is_binary(Response) ->
                    gen_tcp:send(Socket, Response);
                Other ->
                    error_logger:info_msg("tcp_server:unexpected response from handler function: ~p~n", [Other])
            end;
        {tcp_closed, _} ->
            error_logger:info_msg("tcp_server:handle_connection:close(~p)~n", [Socket]),
            gen_tcp:close(Socket),
            exit(closed);
        Other ->
            error_logger:info_msg("tcp_server:handle_connection:unexpected msg: ~p~n", [Other])
    end,
    handle_connection(Socket, HandlerFunction).
