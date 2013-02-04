%%%
%%% Define some standard erlang types for common input and output values.
%%%
-ifndef(GEN_TYPES_HRL).
-define(GEN_TYPES_HRL, 1).

%% Shared between gen modules
-type start_link() ::
	{ok, Pid::pid()} |
	ignore |
	{error, Reason::term()}.

-type init() ::
	{ok, State::term()} |
	{ok, State::term(), Timeout::timeout()} |
	ignore |
	{stop, Reason::term()}.

%% gen_server
-ifdef(GEN_SERVER).
	-type handle_call() ::
		{reply, Reply::call_reply(), NewState::term()} |
		{reply, Reply::call_reply(), NewState::term(), Timeout::timeout()} |
		{noreply, NewState::term()} |
		{noreply, NewState::term(), Timeout::timeout()} |
		{stop, Reason::term(), FinalState::term()} |
		{stop, Reason::term(), Reply::term(), FinalState::term()}.

	-type handle_cast() ::
		{noreply, NewState::term()} |
		{noreply, NewState::term(), Timeout::timeout()} |
		{stop, Reason::term(), FinalState::term()}.

	-type handle_info() ::
		{noreply, NewState::term()} |
		{noreply, NewState::term(), Timeout::timeout()} |
		{stop, Reason::term(), FinalState::term()}.
-endif.

%% gen_fsm
-ifdef(GEN_FSM).
	-type send_event() ::
		{next_state, NextStateName::atom(), NextState::term()} |
		{next_state, NextStateName::atom(), NextState::term(), Timeout::timeout()} |
		{stop, Reason::term(), FinalState::term()}.

	-type sync_send_event() ::
		{next_state, NextStateName::atom(), NextState::term()} |
		{next_state, NextStateName::atom(), NextState::term(), Timeout::timeout()} |
		{reply, Reply::sync_reply(), NextStateName::atom(), NextState::term()} |
		{reply, Reply::sync_reply(), NextStateName::atom(), NextState::term(), Timeout::timeout()} |
		{stop, Reason::term(), FinalState::term()} |
		{stop, Reason::term(), Reply::sync_reply(), FinalState::term()}.

	% These are gen_fsm events that have similar return values, so we'll just reuse them
	-type handle_event() :: send_event().
	-type handle_sync_event() :: sync_send_event().
	-type fsm_info() :: send_event().

-endif.

-endif.


