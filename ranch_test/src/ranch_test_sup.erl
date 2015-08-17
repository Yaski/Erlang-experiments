-module(ranch_test_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_listen/0]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%    {ok, {{one_for_one, 3, 10}, [?CHILD(ranch_test_sup, worker)]}}.
  {ok, {{one_for_one, 3, 10}, [
    {ranch_test, {ranch_test_sup, start_listen, []}, transient, 5000, worker, [ranch_test]},
    ?CHILD(battle_server, worker)
  ]}}.

start_listen() ->
    Port = 8080,
    NumAcceptors = 200,
    io:format("start"),
    ranch:start_listener(my_pool, NumAcceptors,
        ranch_tcp, [{port, Port}],
        test_protocol, []).
