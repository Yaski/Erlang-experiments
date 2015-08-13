-module(image_test).
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

-define(APPS, [crypto, erl_img, image_test]).

-export([test/0, test_image/0]).

-include_lib("erl_img/include/erl_img.hrl").

-define(SRC_FILE, "./priv/example_image.png").
-define(DST_FILE, "./priv/example_image2.png").

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

test_image() -> 
	gen_server:call(?SERVER, test_image).
	
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(test, _From, State) ->
    {reply, "Hello world!", State};

handle_call(test_image, _From, State) ->
    {reply, test_erl_img(), State};
	
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

test_erl_img() ->
	% Read
	{ok, IMG} = erl_img:load(?SRC_FILE),
	
	% Scale
	IMG1 = erl_img:scale(IMG, 0.5), % scale factor

	% Get the MIME type
	Mime = erl_img:mime_type(IMG),
	
	% Crop
%	IMG2 = erl_img:crop(IMG1, 
%			IMG1#erl_image.width / 2,  % new width
%			IMG1#erl_image.height / 2, % new height
%			IMG1#erl_image.width / 4,  % x offset
%			IMG1#erl_image.height / 4), % y offset

	% convert to PNG
	IMG3 = IMG1#erl_image{ type = image_png, filename = ?DST_FILE },
	
	% Write
	erl_img:save(IMG3),
	["Saved image", Mime].

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
