%%%-------------------------------------------------------------------
%%% @author Julian Thatcher
%%% @copyright (C) 2013, Julian Thtacher
%%% @doc
%%% A gateway - what a client connects to, where it gets a worker,
%%% and its method of communicating with the world.
%%% The gateway spawns a net_worker to handle the connection, on some
%%% available node, and directs the client to connect to that.
%%% @end
%%% Created : 2013-02-02 13:34:37.901757
%%%-------------------------------------------------------------------
-module(net_gateway).

-behaviour(gen_server).

-define(GEN_SERVER, 1).
-include("gen_types.hrl").
-undef(GEN_SERVER).
-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

-define(DEFAULT_PORT, 2200).

-record(state, {
	listener,   % Listening socket
	acceptor    % Asynchronous acceptor's internal reference
}).

-type call() ::
	% The various call messages handled
	term().

-type call_reply() ::
	% The various call message replies
	term().

-type cast() ::
	% The various cast messages handled
	term().

-type info() ::
	% The various info messages handled
	% inet_async result
	{inet_async, ListSock::term(), Ref::term, Result::term()} |
	term().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> start_link().
start_link() ->
	Port = galerlxy:get_config(gateway, port, ?DEFAULT_PORT),
	start_link(Port).

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(Port::integer()) -> start_link().
start_link(Port) when is_integer(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the server
%% @private
%%--------------------------------------------------------------------
-spec init([Port::integer()]) -> init().
init([Port]) ->
	process_flag(trap_exit, true),
	Options = [
		binary, {packet, 2}, {reuseaddr, true},
		{keepalive, true}, {backlog, 30}, {active, false}],
	case gen_tcp:listen(Port, Options) of
		{ok, Listen_Socket} ->
			% Create first accepting process
			{ok, Ref} = prim_inet:async_accept(Listen_Socket, -1),
			error_logger:info_msg("(~p/~p) Ready and willing.~n",
				[?MODULE, self()]),
			{ok, #state{
				listener = Listen_Socket,
				acceptor = Ref
			}};
		{error, Reason} ->
			{stop, Reason}
	end.


%%--------------------------------------------------------------------
%% @doc Handling call messages
%% @private
%%--------------------------------------------------------------------
-spec handle_call(Request::call(), From::term(), State::term()) ->
	handle_call().
handle_call(Request, _From, State) ->
	{stop, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
%% @private
%%--------------------------------------------------------------------
-spec handle_cast(Request::cast(), State::term()) -> handle_cast().
handle_cast(Msg, State) ->
	{stop, {unknown_cast, Msg}, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
%% @private
%%--------------------------------------------------------------------
-spec handle_info(Info::info(), State::term()) -> handle_info().
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
		#state{ listener = ListSock, acceptor = Ref } = State) ->
	error_logger:info_msg("(~p/~p) Got a connection~n",
		[?MODULE, self()]),
	try
		case set_sockopt(ListSock, CliSocket) of
			ok -> ok;
			{error, Reason} -> exit({set_sockopt, Reason})
		end,

		% New client connected - spawn a new process to handle the
		% connection. The process handles talking to the client, and
		% may redirect them to another node.
		{ok, Pid} = net_gateway_sup:start_client(),
		gen_tcp:controlling_process(CliSocket, Pid),
		% Notify it that it owns the socket
		net_gateway_client:set_socket(Pid, CliSocket),

		% Signal the network driver that we are ready to accept another
		% connection.
		NewRef = case prim_inet:async_accept(ListSock, -1) of
			{ok, Ref1} ->
				Ref1;
			{error, Ref1} ->
				exit({async_accept, inet:format_error(Ref1)})
		end,

		{noreply, State#state{ acceptor = NewRef }}
	catch exit:Why ->
		error_logger:error_msg("(~p) Error in async accept: ~p~n", [Why]),
		{stop, Why, State}
	end;
handle_info(Info, State) ->
	error_logger:info_msg("(~p) Unknown info: ~p~n", [Info]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), FinalState::term()) -> _.
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @private
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), State::term(), Extra::term()) ->
	{ok, NewState::term()}.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Taken from prim_inet. We are merely copying some socket
%% options from the listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
	true = inet_db:register_socket(CliSocket, inet_tcp),
	Options = [active, nodelay, keepalive, delay_send, priority, tos],
	case prim_inet:getopts(ListSock, Options) of
		{ok, Opts} ->
			case prim_inet:setopts(CliSocket, Opts) of
				ok ->
					ok;
				Error ->
					gen_tcp:close(CliSocket),
					Error
			end;
		Error ->
			gen_tcp:close(CliSocket),
			Error
	end.


-ifdef(TEST).
%%%===================================================================
%%% Unit tests
%%%===================================================================

-endif.


