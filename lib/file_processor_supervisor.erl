-module(file_processor_supervisor).
-behaviour(supervisor).

%% API
-export([
	start_link/0,
	start_processor/0
]).

%% supervisor callbacks
-export([
	init/1
]).

%% macros
-define(SUPERVISOR_MODULE, ?MODULE).
-define(WORKER_MODULE, file_processor_worker).

start_link() ->
	error_logger:info_msg("~s starting: my pid is ~p", [?SUPERVISOR_MODULE, self()]), % supervisor PID
	supervisor:start_link({local, ?SUPERVISOR_MODULE}, ?SUPERVISOR_MODULE, []).

start_processor() ->
	error_logger:info_msg("~s: process ~p starting processor", [?SUPERVISOR_MODULE, self()]), % calling process PID
	supervisor:start_child(?SUPERVISOR_MODULE, []).

init([]) ->
	Processor = {
		?WORKER_MODULE,	% Id to register
		{?WORKER_MODULE, start_link, []},	%M/F/A to spawn
		temporary,	% child restart type
		brutal_kill, % child shutdown type
		worker,	% child type
		[?WORKER_MODULE] % list of involved modules, for hot code swaps
	},
	Children = [Processor],
	RestartStrategy = {
		simple_one_for_one, % spawn new processes with this child spec on request
		0, % no more than this many restarts in ...
		1 % ... this many seconds
	},
	{ok, {RestartStrategy, Children}}.

