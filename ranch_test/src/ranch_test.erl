-module(ranch_test).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([
    start/0,
    stop/0
]).

-define(APPS, [sync, ranch, ranch_test]).

-export([test/0, start_listening/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    ok = ensure_started(?APPS),
    ok = sync:go().

stop() ->
	  sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

test() -> 
	gen_server:call(?SERVER, test).

start_listening() ->
  gen_server:cast(?SERVER, start_listening).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(test, _From, State) ->
    {reply, "Hello world!", State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
