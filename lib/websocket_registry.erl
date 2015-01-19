-module(websocket_registry).

-behaviour(gen_server).

%% API
-export([
	start_link/0,
	add_websocket/3,	% session id, service, pid
	delete_websocket/2,	% session id, service
	delete_websocket/1,	% pid
	get_websocket/2,	% session id, service
	get_websockets/1,	% service
	get_websockets/0]
).

%% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link()		-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_websocket		(Sid, Service, Pid)		-> gen_server:cast(?MODULE, {add_websocket, Sid, Service, Pid}).
delete_websocket	(Sid, Service)			-> gen_server:cast(?MODULE, {delete_websocket, Sid, Service}).
delete_websocket	(Pid) when is_pid(Pid)	-> gen_server:cast(?MODULE, {delete_websocket, Pid}).
get_websocket		(Sid, Service) when is_binary(Sid) and is_list(Service)	-> gen_server:call(?MODULE, {get_websocket, Sid, Service}).
get_websockets		(Service)				-> gen_server:call(?MODULE, {get_websockets, Service}).
get_websockets		()						-> gen_server:call(?MODULE, get_websockets).


%%% init
init([]) ->
	error_logger:info_msg("~p:init([]): starting pid ~p", [?MODULE, self()]),
    {ok, #state{}}.

%%% gen_server callbacks

handle_cast({add_websocket, SessionId, ServiceName, Pid}, State) ->
	error_logger:info_msg(
		"~s:~p adding websocket: ~s, ~s, ~p",
		[?MODULE, self(), SessionId, ServiceName, Pid]
	),
	gproc:reg({n,l,{SessionId, ServiceName}}, Pid),
	{noreply, State};

handle_cast({delete_websocket, SessionId, ServiceName}, State) ->
	gproc:unreg({n,l,{SessionId,ServiceName}}),
	{noreply, State};

handle_cast({delete_websocket, Pid}, State) when is_pid(Pid) ->
	error_logger:info_msg(
		"~s:~p deleting websocket: ~p",
		[?MODULE, self(), Pid]
	),
	[[{n,l,{SessionId,ServiceName}},_MyPid,Pid]] = gproc:select([{{'_', '_', Pid}, [], ['$$']}]),
	gproc:unreg({n,l,{SessionId,ServiceName}}),
	{noreply, State}.

%% generic
%handle_call(_Msg, State) -> error_logger:info_msg("FAIL: generic handle_cast"), {noreply, State}.

handle_call({get_websocket, SessionId, ServiceName}, _From, State) ->
	Pid = gproc:get_value({n,l,{SessionId,ServiceName}}),
	{reply, Pid, State};

handle_call({get_websockets, ServiceName}, _From, State) ->
	% retrieve Pids from storage
	Key = {'_',ServiceName},
	GprocKey = {'_','_',Key},
	MatchHead = {GprocKey,'_','_'},
	Pids = do_gproc_select(MatchHead),
	{reply, Pids, State};

handle_call(get_websockets, _From, State) ->
	% retrieve Pids from storage
	Pids = do_gproc_select({'_','_','_'}),
	{reply, Pids, State}.

do_gproc_select(MatchHead) ->
	GprocResults = gproc:select([{MatchHead,[],['$$']}]),
	[{SessionId,ServiceName,Pid}||[{n,l,{SessionId,ServiceName}},_MyPid,Pid]<-GprocResults].

%% generic
%handle_call(_Request, _From, State) -> error_logger:info_msg("FAIL: generic handle_call"), {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
