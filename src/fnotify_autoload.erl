%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc
%%%    code autoloader
%%% @end
%%% Created :  4 Dec 2011 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(fnotify_autoload).

-behaviour(gen_server).

%% -define(debug, true).
%% API
-export([start_link/0,start_link/1]).
-export([start/0,start/1]).
-export([stop/0]).
-export([dirs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state,
	{
	  path_filter = [], %% prefix path filter
	  path_ref = []     %% list of {Ref,Path}
	}).

-ifdef(debug).
-define(dbg(F, A), io:format("~s:~w: "++(F),[?FILE,?LINE|(A)])).
-else.
-define(dbg(F, A), ok).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

start() ->
    start([]).

start(Options) ->
    application:start(fnotify),
    Spec = {?MODULE, {?MODULE, start_link, Options},
	    permanent, 5000, worker, [?MODULE]},
    supervisor:start_child(fnotify_sup,  Spec).

stop() ->
    gen_server:call(?SERVER, stop).

dirs() ->
    gen_server:call(?SERVER, dirs).

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
init([Options]) ->
    %% Filter is supposed to remove watching changes in otp installation library
    %% I assume the -pa or ERL_LIBS are used to referer to user libraries
    %% this saves a lot of os resources.
    Filter = 
	case proplists:get_bool(watch_lib_dir, Options) of
	    false -> 
		[code:lib_dir()];
	    true ->
		[]
	end,
    PathRef = watch_dir_list(Filter, code:get_path()),
    {ok, #state{ path_filter=Filter, path_ref=PathRef}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(dirs, _From, State= #state {path_ref = Dirs}) ->
    io:format("Watched directories:\n ~p\n", [Dirs]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    io:format("Bad call: ~p\n", [_Request]),
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% add path message (from autopath)
handle_cast({add_autopath,Path}, State) ->
    ?dbg("adding autopath directory '~s'\n", [Path]),
    case watch_dir(Path, State#state.path_ref) of
	{true,PathRef1} ->
	    code:add_path(Path),
	    {noreply, State#state { path_ref = PathRef1}};
	{false,_} ->
	    {noreply, State}
    end;
handle_cast({del_autopath,Path}, State) ->
    ?dbg("deleting autopath directory '~s'\n", [Path]),
    case lists:keytake(Path, 2, State#state.path_ref) of
	false ->
	    ?dbg("autopath not watched '~s'\n", [Path]),
	    {noreply, State};
	{value,{_,Ref},PathRef1} ->
	    fnotify:unwatch(Ref),
	    _Res = code:del_path(Path),
	    ?dbg("autopath  '~s' deleted res=~w\n", [Path,_Res]),
	    {noreply, State#state { path_ref = PathRef1}}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(FEvent={fevent,_Ref,_Flags,_Path,_Name}, State) ->
    ?dbg("got event: ~p\n", [FEvent]),
    handle_erl_path(FEvent, State);
handle_info(_Info, State) ->
    ?dbg("got info: ~p\n", [_Info]),
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
terminate(_Reason, State) ->
    lists:foreach(
      fun({Ref,_Path}) ->
	      fnotify:unwatch(Ref)
      end, State#state.path_ref),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

watch_dir_list(Filter, DirList0) ->
    lists:foldl(
      fun(Dir,DirList) ->
	      Exclude = lists:any(fun (F) -> lists:prefix(F, Dir) end, Filter),
	      if Exclude ->
		      DirList;
		 true ->
		      {_,DirList1} = watch_dir(Dir, DirList),
		      DirList1
	      end
      end, [], DirList0).

watch_dir(Dir, WList) ->
    case fnotify:watch(Dir) of
	{ok,Ref} ->
	    ?dbg("watch path: ~s\n", [Dir]),
	    {true,[{Ref,Dir}|WList]};
	_Error ->
	    ?dbg("error unable to watch: '~s' error:~p\n",[Dir,_Error]),
	    {false,WList}
    end.

%%
%% Some file among the code paths has been change
%% If it is a beam file that has been created then load it
%% FIXME: ignore bea# create
%%        ignore bea# delete
%%        handle name.app file creations (reload app)?
%%        if . is in the code path try to handle . dynamically?
%%
handle_erl_path(_FEvent={fevent,_Ref,Flags,Path,Name}, State) ->
    case was_created(Flags) of
	true ->
	    case filename:extension(Name) of
		".beam" ->
		    BaseName = filename:basename(Name, ".beam"),
		    FileName = filename:join(Path,BaseName),
		    Module = list_to_atom(BaseName),
		    WasLoaded = case code:is_loaded(Module) of
				    {file, _OldFiledName} -> true;
				    false -> false
				end,
		    code:purge(Module),
		    case code:load_abs(FileName) of
			{module,_Mod} ->
			    if WasLoaded ->
				    io:format("module '~s' reloaded\n", [_Mod]);
			       true ->
				    io:format("module '~s' loaded\n", [_Mod])
			    end,
			    {noreply,State};
			{error, Reason} ->
			    io:format("unable to auto-load module ~s, ~p\n",
				      [BaseName,Reason]),
			    {noreply,State}
		    end;
		_Ext ->
		    ?dbg("ignore event: ~p\n", [_FEvent]),
		    {noreply,State}
	    end;
	false ->
	    ?dbg("ignore event: ~p\n", [_FEvent]),
	    {noreply,State}
    end.

was_created(Flags) ->
    case lists:member(create, Flags) of
	true ->
	    true;
	false ->
	    case lists:member(moved_to, Flags) of
		true ->
		    true;
		false ->
		    false
	    end
    end.
