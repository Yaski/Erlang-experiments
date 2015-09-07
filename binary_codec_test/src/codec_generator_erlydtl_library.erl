%% API
-behaviour(erlydtl_library).
-export([inventory/1, version/0]).

-export([filter_packets_by_params/2]).

version() -> 1.

inventory(tags) -> [];
inventory(filters) ->
  [filter_packets_by_params].

filter_packets_by_params(Value, Params) ->
  lists:filter(
    fun (Packet) ->
      proplists:get_value(params, Packet) == Params end,
    Value
  ).
