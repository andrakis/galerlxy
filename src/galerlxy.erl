-module(galerlxy).

-export([get_env/1, get_env/2, get_config/3]).

%% @doc Get a Galerlxy environment value. These are sourced from the the
%%      configuration in config/galerlxy.config.
%%      Returns undefined if not found.
-spec get_env(Key::atom()) -> term() | undefined.
get_env(Key) ->
	get_env(Key, undefined).


%% @doc Get a Galerlxy environment value. These are sourced from the the
%%      configuration in config/galerlxy.config.
%%      Returns Default if not found.
-spec get_env(Key::atom(), Default) -> term() | Default.
get_env(Key, Default) when is_atom(Key) ->
	case application:get_env(galerlxy, Key) of
		{ok, Value} -> Value;
		_ -> Default
	end.

%% @doc Get a configuration value from the config file. This function assumes
%%      that the configuration file contains {Section, [{Key, Value}, ...]}.
-spec get_config(Section::atom(), Key::atom(), Default) -> term() | Default.
get_config(Section, Key, Default) ->
	Options = get_env(Section, []),
	case lists:keyfind(Key, 1, Options) of
		{Key, Value} -> Value;
		false -> Default
	end.
