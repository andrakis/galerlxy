%% @doc GSON: Galerlxy Standard Object Notation
%%
%% Provides a conversion to/from binary format.
%% TODO: Make this work outside of erlang.
%%       That is, it uses BIFs that may not be easy to implement elsewhere.

-module(gson).

-export([encode/1, decode/1, nonce/0]).

%% @doc Encode the given term to a binary format that can be decoded via
%% decode/1.
-spec encode(Term::any()) -> binary().
encode(Term) ->
	term_to_binary(Term).

%% @doc Decode the given binary data into a native Erlang term.
-spec decode(Data::binary()) -> any().
decode(Data) ->
	binary_to_term(Data).

%% @doc Create a security context nonce, which may be used to encode client
%%      communications.
%%      This nonce is guarranteed to be unique thanks to the use of now(),
%%      but this function in general isn't all that nice.
%%      Consider the problem that now() will slowly offset the clock timestamp.
-spec nonce() -> binary().
nonce() ->
	encode([make_ref(), now()]).
