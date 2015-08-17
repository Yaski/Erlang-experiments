-module(test_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-define(W_CONNECT, 10).
-define(W_POSITION, 15).

-define(R_INIT, 130).
-define(R_POSITION, 135).
-define(R_REMOVE, 140).

-record(state, {socket, transport, session = 0, route = 0}).

start_link(Ref, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
  {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
  io:format("accept~n"),
  ok = ranch:accept_ack(Ref),
  loop(#state{socket = Socket, transport = Transport}),
  io:format("protocol finished~n").

loop(#state{route = 0} = State) ->
  loopMessages(State#state{route = 1});
loop(#state{route = 1} = State) ->
  loopSocket(State#state{route = 0}).

loopMessages(State) ->
  receive
    {move, Session, Position} ->
      send_move(State, Session, Position),
      loop(State);
    {destroy, Session} ->
      send_destroy(State, Session),
      loop(State);
    _ ->
      loop(State)
  after 100 ->
    loop(State)
  end.

send_move(#state{transport = Transport, socket = Socket} = State, Session, Position) ->
  {X, Y, Z} = Position,
  Data = <<?R_POSITION:8, Session:8, X:8, Y:8, Z:8>>,
%  format_session(State, "Send move from ~p ~n", [Session]),
  send_message(Transport, Socket, Data).

send_destroy(#state{transport = Transport, socket = Socket} = State, Session) ->
  format_session(State, "Send destroy from ~p ~n", [Session]),
  send_message(Transport, Socket, <<?R_REMOVE:8, Session:8>>).

loopSocket(#state{transport = Transport, socket = Socket} = State) ->
  case Transport:recv(Socket, 2, 100) of
    {ok, Data} ->
      <<Size:16/integer>> = Data,
      NewState = process_query(State, Size),
      loop(NewState);
    {error, timeout} ->
      loop(State);
    _ ->
      format_session(State, "close~n"),
      battle_server:destroy(State#state.session),
      ok = Transport:close(Socket)
  end.

process_query(State, Size = 0) ->
  format_session(State, "Wrong packet size : ~p ~n", [Size]),
  State;
process_query(#state{transport = Transport, socket = Socket} = State, Size) ->
  {ok, RawData} = Transport:recv(Socket, Size, 10000),
  <<Type:8/integer, Data/binary>> = RawData,
%  format_session(State, "Receive ~p Size ~p ~n", [Type, Size]),
  {NewState, Result} = process_query(State, Type, Data),
  send_message(Transport, Socket, Result),
  NewState.

process_query(State, ?W_POSITION, Data) ->
  <<Session:8/integer, X:8/integer, Y:8/integer, Z:8/integer>> = Data,
  battle_server:move(Session, {X, Y, Z}),
  {State, <<>>};

process_query(State, ?W_CONNECT, _Data) ->
  {Session, Color, Position} = battle_server:login(self()),
  {R, G, B} = Color,
  {X, Y, Z} = Position,
  NewState = State#state{session = Session},
  format_session(NewState, "Init session ~n"),
  {NewState, <<?R_INIT:8, Session:8, R:8, G:8, B:8, X:8, Y:8, Z:8>>}.

send_message(Transport, Socket, Data) ->
  Size = byte_size(Data),
  if
    Size > 0 -> Transport:send(Socket, <<Size:16, Data/binary>>);
    true -> ok
  end.

format_session(State, Message) ->
  format_session(State, Message, []).

format_session(State, Message, Params) ->
  Session = State#state.session,
  if
    Session > 0 -> io:format("~p: " ++ Message, [Session | Params]);
    true -> io:format("//: " ++ Message, Params)
  end.
