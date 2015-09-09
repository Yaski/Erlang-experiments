-module(codec_generator_erlydtl_library).
%% API
-behaviour(erlydtl_library).
-export([inventory/1, version/0]).

-export([filter_packets/2, pascal_case/1, snake_case/1, package_case/1]).

version() -> 1.

inventory(tags) -> [];
inventory(filters) ->
  [filter_packets, pascal_case, snake_case, package_case].

filter_packets(Value, Params) ->
  lists:filter(
    fun (Packet) ->
      proplists:get_value(params, Packet) == Params end,
    Value
  ).

pascal_case(Value) when is_binary(Value) -> pascal_case(binary_to_list(Value));
pascal_case(Value) ->
  Str = string:tokens(Value, "_"),
  Val = [[string:to_upper(X) | Y] || [X | Y] <- Str],
  string:join(Val, "").

snake_case(Value) when is_binary(Value) -> snake_case(binary_to_list(Value));
snake_case(Value) ->
  string:to_lower(Value).

package_case(Value) when is_binary(Value) -> package_case(binary_to_list(Value));
package_case(Value) ->
  Str = string:tokens(Value, "_"),
  string:join(Str, "").
