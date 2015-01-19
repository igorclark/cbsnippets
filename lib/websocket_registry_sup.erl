-module(websocket_registry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% macros
-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%% API functions

% call this in your websocket controller, if you only have one,
% or maybe in priv/init/*.erl if you have more
start_link() ->
	error_logger:info_msg(
		"~s:start_link(): ~p starting websocket registry supervisor",
		[?MODULE, self()]
	),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callback functions
init([]) ->
	application:ensure_started(gproc),
    {ok, {{one_for_one, 5, 10}, [?CHILD(websocket_registry, websocket_registry, worker, [])]}}.
