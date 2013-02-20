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
%%%    fnotify client
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(fnotify).
-export([watch/1, unwatch/1]).
-export([is_dir/1, status/0]).

-include_lib("kernel/include/file.hrl").

-define(SERVER, fnotify_srv). 

watch(Path) ->
    gen_server:call(?SERVER, {watch,self(),Path}).

unwatch(Ref) ->
    gen_server:call(?SERVER, {unwatch, Ref}).

is_dir(Path) ->
    case file:read_file_info(Path) of
	{ok,Info} ->
	    Info#file_info.type =:= directory;
	Error ->
	    Error
    end.

status() ->
    gen_server:call(?SERVER, status).
