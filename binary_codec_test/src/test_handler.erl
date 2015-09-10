-module(test_handler).

%% API
-export([encode/1, decode/1]).

% -= SERVER =-

-record(connect, {}).

-record(say, {
  user = 0 :: byte(),
  setting = 0 :: integer(),
  distance = 0 :: float(),
  message = <<>> :: binary()
}).

% -= CLIENT =-

-record(listen, {}).

-record(receive_message, {
  session = 0 :: byte(),
  distance = 0 :: float(),
  text = <<>> :: binary()
}).

% if no vars
encode(#listen{} = Packet) ->
  ok;

% if vars
% last element
encode(#say{} = Packet) ->
  ok.

decode(Packet) ->
  % parse tag
  % parse based on tag
  <<Tag, Rest>> = Packet,
  decode(Tag, Rest).

% if no vars
decode(0, Data) ->
  #connect{};
% if vars
% last element

decode(1, Data) ->
  #say{}.
