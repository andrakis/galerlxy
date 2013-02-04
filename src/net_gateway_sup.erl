%% @doc Net Gateway Supervisor
%%
%% TODO: The tutorial I worked from implemented two supervisors in
%%       the one module, which is why init/1 has two distinctly
%%       different function clauses.
%%       At some point these should probably be split into seperate
%%       modules for ease of readability / maintainability.

-module(net_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_client/0, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_client() ->
	supervisor:start_child(net_gateway_client, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, { {one_for_one, 5, 10}, [
		% Gateway server
		{
			gateway,
			{net_gateway, start_link, []},
			permanent,
			2000,
			worker,
			[net_gateway]
		},
		% Gateway client instance
		{
			net_gateway_client,
			{supervisor, start_link, [{local, net_gateway_client}, ?MODULE, [client]]},
			permanent,
			infinity,
			supervisor,
			[]
		}
	]} };

init([client]) ->
	{ok,
		{ {simple_one_for_one, 5, 60}, [
			% TCP client
			{
				undefined,
				{net_gateway_client, start_link, []},
				temporary,
				2000,
				worker,
				[]
			}
		]}
	}.
