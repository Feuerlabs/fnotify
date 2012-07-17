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
%%%    Start/stop the "right" fnotify server
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(fnotify_srv).

%% API
-export([start/0]).
-export([start_link/0]).
-export([stop/0]).

-define(SERVER, fnotify_srv).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.

start() ->
    Module = fnotify_module(),
    gen_server:start({local, ?SERVER}, Module, [], []).

start_link() ->
    Module = fnotify_module(),
    gen_server:start_link({local, ?SERVER}, Module, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

fnotify_module() ->
    case os:type() of
	{unix,linux} -> fnotify_inotify_srv;
	{unix,darwin} -> fnotify_kevent_srv
    end.



