-module(cbsnippets_example_websocket, [Req, SessionId]).
-behaviour(boss_service_handler).

%%%%%%%%%%
%% records
%%%%%%%%%%

-record(state,{}).


%%%%%%
%% API
%%%%%%

-export([
	init/0, 
	handle_incoming/4,
	handle_join/3,
	handle_close/4,
	handle_broadcast/2,
	handle_info/2,
	terminate/2
]).


%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%

init() ->
	% websocket_registry_sup:start_link(),
    {ok, #state{}}.

handle_join(ServiceName, WebsocketPid, State) ->
	error_logger:info_msg(
		"[WS JOIN][~p|~s|~s]",
		[WebsocketPid, shorten_session_id(SessionId), ServiceName]
	),
	% websocket_registry:add_websocket(SessionId, ServiceName, WebsocketPid),
    {noreply, State}.

handle_close(Reason, ServiceName, WebsocketPid, State) ->
    error_logger:info_msg(
		"[WS EXIT][~p|~s|~s] [~p]",
		[WebsocketPid, shorten_session_id(SessionId), ServiceName, Reason]
	),
	% websocket_registry:delete_websocket(WebsocketPid),
    {noreply, State}.

handle_incoming(ServiceName, WebsocketPid, Message, State) ->
    error_logger:info_msg(
		"[WS MSG] [~p|~s|~s] ~p",
		[WebsocketPid, shorten_session_id(SessionId), ServiceName, Message]
	),
    {noreply, State}.

handle_broadcast(Message, State) ->
	error_logger:info_msg(
		"[WS RX B][~p]",
		[Message]
	),
	{noreply, State}.

handle_info(ping, State) -> {noreply, State};
handle_info(state, State) -> {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, State) ->
    error_logger:info_msg(
		"[WX TERM][~p] [~p]",
		[Reason, State]
	),
    ok.


%%%%%%%%%%%%%%%%%%%%
%% utility functions
%%%%%%%%%%%%%%%%%%%%

shorten_session_id(SessionId) ->
	list_to_binary(
		[binary:part(SessionId, 0, 20), <<"..">>]
	).


