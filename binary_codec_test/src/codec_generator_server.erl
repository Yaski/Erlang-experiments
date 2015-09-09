-module(codec_generator_server).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tokenizer_module, parser_module}).

-export([
  update_language/0,
  generate_codecs/0
]).

%%%===================================================================
%%% API
%%%===================================================================

update_language() ->
  gen_server:cast(?MODULE, {update_language}).

generate_codecs() ->
  gen_server:cast(?MODULE, {generate_codecs}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Any, _From, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(stop, State) ->
  {stop, State};

handle_cast({update_language}, State) ->
  {TokenizerName, ParserName} = generate_language(application:get_env(binary_codec_test, codec_language_parser, leex)),
  codec_generator_server:generate_codecs(),
  {noreply, State#state{tokenizer_module = TokenizerName, parser_module = ParserName}};

handle_cast({generate_codecs}, State) ->
  Fun = fun(F, AccIn) -> gen_codec(F, State), AccIn end,
  filelib:fold_files(application:get_env(binary_codec_test, codec_src_dir, "priv"), ".*\.dsc", true, Fun, []),

  io:format("~ngenerate_codecs~n"),
  {noreply, State}.

generate_language(leex) ->
  {ok, TokenizerFile} = leex:file(application:get_env(binary_codec_test, codec_language_leex_file, "src/codec_leex.xrl")),
  {ok, ParserFile} = yecc:file(application:get_env(binary_codec_test, codec_language_yecc_file, "src/codec_yecc.yrl"), [{verbose, true}]),

  {ok, TokenizerName, TokenizerBinary} = compile:file(TokenizerFile, [binary, report, debug_info]),
  {module, TokenizerName} = code:load_binary(TokenizerName, TokenizerFile, TokenizerBinary),

  {ok, ParserName, ParserBinary} = compile:file(ParserFile, [binary, report, debug_info]),
  {module, ParserName} = code:load_binary(ParserName, ParserFile, ParserBinary),
  {TokenizerName, ParserName};
generate_language(neotoma) ->
  SrcParser = application:get_env(binary_codec_test, codec_language_neotoma_file, "src/codec_neotoma.peg"),
  ok = neotoma:file(SrcParser, [{verbose, true}]),
  ParserFile = filename:rootname(SrcParser) ++ ".erl",
  {ok, ParserName, ParserBinary} = compile:file(ParserFile, [binary, report, debug_info]),
  {module, ParserName} = code:load_binary(ParserName, ParserFile, ParserBinary),
  {undefined, ParserName}.

gen_codec(File, State) ->
  Templates = application:get_env(binary_codec_test, codec_templates, []),
  case State#state.tokenizer_module of
    Module when Module =/= undefined ->
      Tokens1 = scan_file(File, State#state.tokenizer_module),
      OutName1 = filename:rootname(File) ++ ".tokens.txt",
      ok = file:write_file(OutName1, io_lib:fwrite("~p", [Tokens1])),
      ParserModule = State#state.parser_module,
      OutName2 = filename:rootname(File) ++ ".parser.txt",
      Tokens2 = case ParserModule:parse(Tokens1) of
        {ok, Tokens} ->
          gen_codecs_by_templates(File, prepare_tokens(File, Tokens), Templates),
          Tokens;
        {error, {Line_number, _Module, Message}} ->
          {error, {line, Line_number}, Message}
      end,
      ok = file:write_file(OutName2, io_lib:fwrite("~p", [Tokens2]));
    _ ->
      OutName = filename:rootname(File) ++ ".parser.txt",
      Tokens2 = case (State#state.parser_module):file(File) of
        Tokens when is_list(Tokens) ->
          gen_codecs_by_templates(File, prepare_tokens(File, Tokens), Templates),
          Tokens;
        {_, _, Message} ->
          [{syntax_error}, Message]
      end,
      ok = file:write_file(OutName, io_lib:fwrite("~p", [Tokens2]))
  end.

prepare_tokens(File, Tokens) ->
  Packets = generate_packets_tags(proplists:get_value(packets, Tokens)),
  NewTokens = [{packets, Packets} | proplists:delete(packets, Tokens)],
  [{codecname, filename:basename(File, ".dsc")} | NewTokens].

generate_packets_tags(Packets) -> lists:reverse(generate_packets_tags(Packets, 1, [])).
generate_packets_tags([], _Index, Acc) -> Acc;
generate_packets_tags([Packet | Rest], Index, Acc) ->
  NewPacket = [{tag, Index} | Packet],
  generate_packets_tags(Rest, Index + 1, [NewPacket | Acc]).

%gen_codecs_by_templates(_One, _Two, _Three) -> ok.
gen_codecs_by_templates(_File, _Tokens, []) -> ok;
gen_codecs_by_templates(File, Tokens, [{Key, Module, Filename} | Rest]) ->
%  OutName = filename:join([application:get_env(binary_codec_test, codec_out_dir, "codecs"), filename:basename(File, ".dsc") ++ Ext]),
  OutName = filename:join([application:get_env(binary_codec_test, codec_out_dir, "codecs"), gen_filename_by_template(Filename, Tokens)]),
  io:format("Render template ~p ~s~n", [Key, OutName]),
  {ok, List} = Module:render(Tokens),
  OutList = remove_empty_lines(List),
  ok = file:write_file(OutName, OutList),
  gen_codecs_by_templates(File, Tokens, Rest).

gen_filename_by_template(Template, Tokens) ->
  {ok, Module} = erlydtl:compile_template(Template, codec_filename_template, [
    {default_libraries, [codec_generator_erlydtl_library]},
    {libraries, [
      {codec_generator_erlydtl_library, codec_generator_erlydtl_library}
    ]},
    {out_dir, false}
  ]),
  {ok, List} = Module:render(Tokens),
  binary_to_list(iolist_to_binary(List)).

remove_empty_lines(List) ->
  {ok,MP} = re:compile("^(?:[\t ]*(?:\r?\n|\r))+", [multiline, {newline, anycrlf}]),
  Lines = re:split(iolist_to_binary(List), MP, [{return, list}, trim]),
  string:join(Lines, "\r\n").

scan_file(FileName, Scanner) ->
    {ok, InFile} = file:open(FileName, [read]),
    Acc = loop_tokens(InFile, Scanner, []),
    Result = lists:reverse(Acc),
    file:close(InFile),
    Result.

loop_tokens(InFile, Scanner, Acc) ->
    case io:request(InFile,{get_until,prompt,Scanner,token,[1]}) of
        {ok,Token,_EndLine} ->
            loop_tokens(InFile, Scanner, [Token | Acc]);
        {error,token} ->
            io:format("Error while scanning file with module ~s ~n", [Scanner]),
            Acc;
        {eof,_} ->
            Acc
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
