%% Simple test client
%% Connects to localhost and attempts authentication.
%% Disconnects after a short while.
-module(client).

-define(username, daedalus).
-define(password, <<"password">>).

-export([go/0]).
-export([connect/1, get_nonce/1, authenticate/1, authenticate_wait/1, client/1]).

-import(gson, [encode/1, decode/1]).

-record(state, {
	address = {127, 0, 0, 1},
	socket,
	nonce,
	state = connect :: connect | get_nonce | authenticate | redirect | client
}).

go() ->
	go(#state{}).

go(#state{ state = StateName } = State0) ->
	case ?MODULE:StateName(State0) of
		{next_state, NextStateName, State1} ->
			go(State1#state{ state = NextStateName });
		loop ->
			go(State0);
		{stop, Reason} ->
			exit(Reason)
	end.

connect(State0) ->
	{ok, S} = gen_tcp:connect({127,0,0,1}, 2200, [binary, {packet, 2}]),
	{next_state, get_nonce, State0#state{ socket = S }}.

get_nonce(#state{ socket = S, nonce = undefined } = State0) ->
	gen_tcp:send(S, encode({request, nonce})),
	Data = receive
		{tcp, _Port, Message} ->
			decode(Message)
	after 2000 ->
		exit(nonce_timeout)
	end,
	case Data of
		{nonce, Nonce} -> 
			{next_state, authenticate, State0#state{ nonce = Nonce }}
	end.

authenticate(#state{ socket = S, nonce = Nonce } = State0) ->
	EncryptedPassword = crypto:md5([Nonce, ?password]),
	gen_tcp:send(S, encode({login, ?username, EncryptedPassword})),
	{next_state, authenticate_wait, State0}.

authenticate_wait(#state{} = State0) ->
	Data = receive
		{tcp, _Port, Message} -> decode(Message)
		after 20000 -> exit(authenticate_timeout)
	end,
	case Data of
		{info, Info} ->
			io:format("SERVER: ~p~n", [Info]),
			loop;
		fail ->
			{stop, authentication_failure};
		ready ->
			{next_state, client, State0}
	end.

client(#state{ socket = S }) ->
	Data = receive
		{tcp, _Port, Message} -> decode(Message)
		after 4000 ->
			io:format("Idled enough, now I'm disconnecting~n"),
			gen_tcp:send(S, encode(disconnect)),
			{stop, success}
	end,
	case Data of
		{info, Info} ->
			io:format("SERVER: ~p~n", [Info]),
			loop;
		{stop, Reason} ->
			{stop, Reason}
	end.
