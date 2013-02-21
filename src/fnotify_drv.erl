%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @doc
%%%    launch fnotify_drv
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

-define(FLAG_CREATE,   16#01).
-define(FLAG_DELETE,   16#02).
-define(FLAG_MODIFY,   16#04).  %% write|extend|modify
-define(FLAG_ATTRIB,   16#08).  %% attribute was changed
-define(FLAG_LINK,     16#10).  %% link count changed
-define(FLAG_RENAME,   16#20).  %% moved renamed
-define(FLAG_REVOKE,   16#40).

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
    watch(Port, Path, [default]).

watch(Port, Path, Flags) ->
    BinPath = if is_binary(Path) -> Path;
		 true -> list_to_binary(Path)
	      end,
    Mask = flags(Flags),
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

flags(Fs) -> flags(Fs,0).

flags([create|Fs], Mask) -> flags(Fs, ?FLAG_CREATE bor Mask);
flags([delete|Fs], Mask) -> flags(Fs, ?FLAG_DELETE bor Mask);
flags([modify|Fs], Mask) -> flags(Fs, ?FLAG_MODIFY bor Mask);
flags([attrib|Fs], Mask) -> flags(Fs, ?FLAG_ATTRIB bor Mask);
flags([link|Fs], Mask) -> flags(Fs, ?FLAG_LINK bor Mask);
flags([rename|Fs], Mask) -> flags(Fs, ?FLAG_RENAME bor Mask);
flags([revoke|Fs], Mask) -> flags(Fs, ?FLAG_REVOKE bor Mask);
flags([default|Fs], Mask) -> 
    flags(Fs, ?FLAG_CREATE bor ?FLAG_DELETE bor ?FLAG_RENAME bor Mask);
flags([], Mask) -> Mask.

