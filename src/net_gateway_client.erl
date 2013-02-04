%%%-------------------------------------------------------------------
%%% @author Julian Thatcher
%%% @copyright (C) 2013, Noble Samurai
%%% @doc
%%% Handles a client connection to the gateway. Performs the following:
%%%  - Authenticates the user
%%%  - Looks up the user's current zone
%%%  - Ensures the zone is up and ready
%%%  - If the zone is on this node:
%%%    - Adds itself to the zone as the client
%%%    - Manages communication between zone and client
%%%  - Otherwise:
%%%    - Redirects the user to the appropriate gateway
%%%    - Closes connection
%%% @end
%%% Created : 2013-02-02 15:47:19.584555
%%%-------------------------------------------------------------------
-module(net_gateway_client).

-behaviour(gen_fsm).

-define(GEN_FSM, 1).
-include("gen_types.hrl").
-undef(GEN_FSM).
-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0]).
-export([set_socket/2, wait_for_auth/2, zone_setup/2, client/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).
-export([wait_for_socket/2]).
-define(SERVER, ?MODULE).

-define(TIMEOUT, 15000).

-record(state, {
	socket,    % Client socket
	addr,      % Client address
	nonce  :: binary(),
	zone   :: pid()
}).

% The various send_event messages handled
-type event() ::
	term().

-type wait_for_socket_event() ::
	{socket_ready, Socket::port()}.

% The various info messages handled
-type info() ::
	{tcp, Socket::port(), Data::binary()} |
	term().

% The various sync_send_event messages handled
-type sync_event() ::
	term().

% The various sync_send_event replies
-type sync_reply() ::
	term().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
-spec start_link() -> start_link().
start_link() ->
	error_logger:info_msg("~p starting up~n", [?MODULE]),
	gen_fsm:start_link(?MODULE, [], []).

%% @doc Set the socket for this client.
-spec set_socket(Pid::pid(), Socket::port()) -> _.
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
	gen_fsm:send_event(Pid, {socket_ready, Socket}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @private
%%--------------------------------------------------------------------
-spec init(Args::term()) -> init().
init([]) ->
	process_flag(trap_exit, true),
	error_logger:info_msg("(~p/~p) Ready.~n",
		[?MODULE, self()]),
	{ok, wait_for_socket, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%% @private
%%--------------------------------------------------------------------
-spec state_name(Event::event(), State::term()) -> send_event().
state_name(Event, State) ->
	{stop, {unknown_event, Event}, State}.

%% @doc Wait for a socket.
-spec wait_for_socket(Event::wait_for_socket_event(), State::term()) ->
		send_event().
wait_for_socket({socket_ready, Socket}, State0) ->
	% Now we own the socket
	inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
	{ok, {IP, Port}} = inet:peername(Socket),
	error_logger:info_msg("(~p/~p) Now ready to receive from ~p~n",
		[?MODULE, self(), {IP, Port}]),
	State1 = State0#state{
		socket = Socket,
		addr = IP
	},
	{next_state, wait_for_auth, State1};
wait_for_socket(Data, State) ->
	elog("wait_for_socket: Unknown data: ~p~n", [Data]),
	{next_state, wait_for_socket, State}.

%% @doc Wait for user authentication.
wait_for_auth({data, {request, nonce}}, #state{ socket = S } = State0) ->
	Nonce = gson:nonce(),
	log("Requesting nonce, using: ~p~n", [Nonce]),
	Response = gson:encode({nonce, Nonce}),
	log("Responding with: ~p~n", [Response]),
	ok = gen_tcp:send(S, Response),
	State1 = State0#state{
		nonce = Nonce
	},
	{next_state, wait_for_auth, State1, ?TIMEOUT};
wait_for_auth({data, {login, Username, Password}}, #state{ socket = S, nonce = Nonce } = State0) ->
	case {Username, crypto:md5([Nonce, <<"password">>])} of
		{daedalus, Password} ->
			gen_tcp:send(S, gson:encode({info, <<"Setting up zone...">>})),
			% simulate zone wait
			timer:sleep(2000),
			gen_tcp:send(S, gson:encode(ready)),
			{next_state, client, State0};
		_ ->
			gen_tcp:send(S, gson:encode(fail)),
			{stop, normal, State0}
	end;
wait_for_auth(timeout, State) ->
	error_logger:error_msg("(~p/~p) Client connection timeout~n",
		[?MODULE, self()]),
	{stop, normal, State};
wait_for_auth(Data, State) ->
	elog("wait_for_auth: Unknown data: ~p~n", [Data]),
	{next_state, wait_for_auth, State, ?TIMEOUT}.

%% @doc Wait for zone to be setup.
zone_setup(Data, State) ->
	elog("zone_setup: Unknown data: ~p~n", [Data]),
	{next_state, zone_setup, State}.

%% @doc Handle communication between client and zone.
client({data, disconnect}, #state{ socket = S } = State) ->
	log("Client disconnecting, stopping server."),
	gen_tcp:send(S, gson:encode(bye)),
	{stop, normal, State};
client(Data, State) ->
	elog("client: Unknown data: ~p~n", [Data]),
	{next_state, client, State}.

%%--------------------------------------------------------------------
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%% @private
%%--------------------------------------------------------------------
-spec state_name(Event::sync_event(), From::term(), State::term()) ->
		sync_send_event().
state_name(Event, _From, State) ->
	{stop, {unknown_sync_event, Event}, State}.

%%--------------------------------------------------------------------
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%% @private
%%--------------------------------------------------------------------
-spec handle_event(Event::event(), StateName::atom(), State::term()) ->
		handle_event().
handle_event(Event, StateName, State) ->
	{stop, {unkown_all_event, StateName, Event}, State}.

%%--------------------------------------------------------------------
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%% @private
%%--------------------------------------------------------------------
-spec handle_sync_event(Event::term(), From::term(), StateName::atom(),
		State::term()) -> handle_sync_event().
handle_sync_event(Event, _From, StateName, State) ->
	{stop, {unknown_sync_event, StateName, Event}, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%% @private
%%--------------------------------------------------------------------
-spec handle_info(Info::info(), StateName::atom(), State::term()) ->
		fsm_info().
handle_info({tcp, Socket, Bin}, StateName, #state{ socket = Socket } = State) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	% Decode the data first
	Data = gson:decode(Bin),
	?MODULE:StateName({data, Data}, State);

handle_info({tcp_closed, Socket}, _StateName, #state{ socket = Socket } = State) ->
	error_logger:info_msg("(~p/~p) Client disconnected~n", [
		?MODULE, self()]),
	{stop, normal, State};

handle_info(Info, StateName, State) ->
	error_logger:info_msg("(~p) Unknown info received in state ~p: ~p~n",
		[?MODULE, StateName, Info]),
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), StateName::atom(), FinalState::term())
		-> _.
terminate(_Reason, _StateName, #state{ socket = Socket, zone = Zone }) ->
	(catch gen_tcp:close(Socket)),
	if
		is_pid(Zone) -> zone_server:remove_client(self());
		true -> noop
	end,
	ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
%% @private
%%--------------------------------------------------------------------
-spec code_change(OldVsn::term(), StateName::atom(), State::term(),
		Extra::term()) -> {ok, NewStateName::atom(), NewState::term()}.
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
log(Message) -> log(Message, []).

log(Message, Parameters) ->
	error_logger:info_msg("(~p/~p) " ++ Message,
		[?MODULE, self() | Parameters]),
	ok.

elog(Message, Parameters) ->
	error_logger:error_msg("(~p/~p) " ++ Message,
		[?MODULE, self() | Parameters]),
	ok.

-ifdef(TEST).
%%%===================================================================
%%% Unit tests
%%%===================================================================

-endif.


