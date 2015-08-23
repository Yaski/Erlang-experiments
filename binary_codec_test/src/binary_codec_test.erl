-module(binary_codec_test).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start/0,
    stop/0
]).

-define(APPS, [compiler, syntax_tools, sync, neotoma, erlydtl, binary_codec_test]).

-export([
  update_language/0,
  generate_codecs/0
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    ok = ensure_started(?APPS),
    ok = sync:go().

stop() ->
	  sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)).

update_language() ->
  codec_generator_server:update_language().

generate_codecs() ->
  codec_generator_server:generate_codecs().

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error, {already_started, App}} -> ensure_started(Apps)
    end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).
