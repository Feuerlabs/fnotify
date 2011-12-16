%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    launch netlink_drv
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(fnotify_drv).
-export([start/0]).
-export([activate/1, activate/2, deactivate/1]).
-export([watch/2, watch/3, unwatch/2]).

-define(FNOTIFY_ADD_WATCH,  1).
-define(FNOTIFY_DEL_WATCH,  2).
-define(FNOTIFY_ACTIVATE,   3).

-define(FNOTIFY_REP_OK,     0).
-define(FNOTIFY_REP_ERROR,  1).

-ifdef(debug).
-define(dbg(F, A), io:format((F), (A))).
-else.
-define(dbg(F, A), ok).
-endif.

start() ->
    Port = open(),
    activate(Port),
    Port.

deactivate(Port) ->
    activate(Port, 0).    

activate(Port) ->
    activate(Port, -1).

activate(Port, N) when is_integer(N), N >= -1, N < 16#ffff ->
    do_reply(erlang:port_control(Port, ?FNOTIFY_ACTIVATE, <<N:16>>)).

watch(Port, Path) ->
    watch(Port, Path, 16#ff).

watch(Port,Path, Mask) ->
    BinPath = if is_binary(Path) -> Path;
		 true -> list_to_binary(Path)
	      end,
    Arg = <<Mask,BinPath/binary>>,
    do_reply(erlang:port_control(Port, ?FNOTIFY_ADD_WATCH, Arg)).

unwatch(Port, Wd) ->
    Arg =  <<Wd:32>>,
    do_reply(erlang:port_control(Port, ?FNOTIFY_DEL_WATCH, Arg)).

open() ->
    Driver = "fnotify_drv",
    Path = code:priv_dir(fnotify),
    ?dbg("load_driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    erlang:open_port({spawn_driver, Driver}, [binary]);
	{error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    erlang:error(Error)
    end.

do_reply([?FNOTIFY_REP_OK]) ->
    ok;
do_reply([?FNOTIFY_REP_OK,W3,W2,W1,W0]) ->
    <<Wd:32/signed-big-integer>> = <<W3,W2,W1,W0>>,
    {ok, Wd};
do_reply([?FNOTIFY_REP_ERROR | Err]) ->
    {error, list_to_atom(Err)}.

    

	
