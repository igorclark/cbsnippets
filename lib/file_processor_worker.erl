-module(file_processor_worker).
-behaviour(gen_server).

-export([
	process/3
]).

-export([
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {}).

-define(PROCESSOR_UPDATES_WS_NAME, "/websocket/processor_updates").

start_link() ->
	% this is a simple_one_for_one worker processor, with potentially >1
	% process running concurrently, so we use start_link/3 so that no name
	% is register()ed - otherwise we'd get already_started errors here
	gen_server:start_link(?MODULE, [], []).

%% pass in the list of files uploaded from your Boss controller's Req:post_files().
%% i think you could parallelise this further by spawing a file_processor_worker
%% for each uploaded file in your controller - i didn't, because my app restricts
%% on the client side to only one file uploaded at a time
process(Pid, FilePathList, BossSessionId) when is_list(FilePathList) ->
	gen_server:cast(Pid, {convert, FilePathList, BossSessionId}).

init([]) ->
	error_logger:info_msg("~s:~p:init([])", [?MODULE, self()]),
	{ok, #state{}}.

%% here's where the work happens
handle_cast({convert, FilePathList, BossSessionId}, State) when is_list(FilePathList) ->
	% use your own process() function on each file coming in
	ProcessedFiles	= [your_file_processor:process(File)||File<-FilePathList],
	% work out who to notify based on session id
	WebsocketPid	= websocket_registry:get_websocket(BossSessionId, ?PROCESSOR_UPDATES_WS_NAME),
	% create proplists for the processed files
	JsonRecords		= your_utils:create_proplists(ProcessedFiles), % e.g. [[{file,NewFile1},{size,NewSize1}]]
	% minor formatting for Boss encoder
	JsonProplist	= [{files, JsonRecords}],
	% might want to make this a bit more robust, i haven't spent time working out reliable
	% OTP-ish/-derived ways to monitor whether messages are really getting to WS pids,
	% or failing somewhere along the line
	WebsocketPid ! {text, boss_json:encode(JsonProplist, boss_files:model_list(my_app_name))},
	% our work here is done, we may fade gracefully into the ether.
	% {stop, normal} is OTP "i'm done".
	{stop, normal, State};

%% we shouldn't be getting any other async messages, so let's alert if we do
handle_cast	(Msg, State)
	-> error_logger:info_msg(
		"FAIL: generic handle_cast: ~p",
		[Msg]
	),
	{noreply, State}.

% shouldn't be getting synchronous msgs
handle_call	(_Msg, _From, State)		-> {noreply, State}.
% shouldn't be getting out-of-band msgs
handle_info	(_Info, State)		-> {noreply, State}.
terminate	(Reason, _State)			->
	error_logger:info_msg("~s:~p terminating for reason: ~p", [?MODULE, self(), Reason]),
	ok.
code_change	(_OldVsn, State, _Extra)	-> {ok, State}.
