%%%-------------------------------------------------------------------
%%% @author Julian Thatcher
%%% @copyright (C) 2013, Julian Thtacher
%%% @doc
%%% Galerlxy node supervisor. Responsible for starting all the
%%% services a Galerlxy node requires.
%%% @end
%%% Created : 2013-02-01 19:34:37.901757
%%%-------------------------------------------------------------------

-module(galerlxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, { {one_for_one, 5, 10}, [
		% Gateway supervisor
		{
			gateway_sup,
			{net_gateway_sup, start_link, []},
			permanent,
			2000,
			supervisor,
			[net_gateway]
		}
		% TODO: Zone supervisor
	]} }.

