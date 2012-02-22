%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     auto code:path 
%%%     - monitor Libs=$ERL_LIBS ++ [code:lib_dir()]
%%%     - each Lib in existing Libs monitor path Lib
%%%
%%%     update code loader path accordingly
%%% @end
%%% Created : 11 Feb 2012 by Tony Rogvall <tony@rogvall.se>

-module(fnotify_autopath).

-behaviour(gen_server).

-define(debug, true).

%% API
-export([start_link/0]).
-export([start/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-ifdef(debug).
-compile(export_all).
-endif.

-define(SERVER, ?MODULE). 

%%
%% path to watch, if path does not exist then 
%% parents are scanned to supervise the path creation
%%
-record(path_ref,
	{
	  ref,          %% watch ref
	  type,         %% app | lib
	  loaded=false, %% true if type=app and autoload is set
	  path,         %% currently watch driectory
	  target        %% target directory to watch ([] if src is the target)
	}).

-record(state,
	{
	  lib_refs = []   %% #path_ref{}
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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

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
init([]) ->
    fnotify_srv:start(),
    %% FIXME: make all directories absolute paths
    Libs0 = case os:getenv("ERL_LIBS") of
		false -> [];
		ErlLibs -> string:tokens(ErlLibs, ":")
	    end,
    Libs = [code:lib_dir()] ++ Libs0,
    %% load all known libraries
    LibRef = watch_dir_list(Libs, lib),
    %% load all known application paths,
    %% Do not auto loaded them, assume they are loaded by code server
    %% We should probably update this with code path status
    LibRef1 = 
	lists:foldl(
	  fun(P = #path_ref { type=lib, path=Path, target = [] },Acc) ->
		  case file:list_dir(Path) of
		      {error,enoent} ->
			  [P|Acc];
		      {ok,List} ->
			  List1 = lists:map(
				    fun(Name) -> filename:join(Path,Name) end,
				    List),
			  watch_dir_list(List1, [P|Acc], app);
		      _Error ->
			  [P|Acc]
		  end;
	     (P, Acc) ->
		  [P|Acc]
	  end, [], LibRef),
    %% supervise L or parent of L to supervise creation or destruction
    %% supervise L in Libs for new subdirectories added or removed
    {ok, #state{ lib_refs=LibRef1 }}.

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
handle_call(_Request, _From, State) ->
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
handle_info(FEvent={fevent,Ref,_Flags,_Path,_Name}, State) ->
    ?dbg("got event ~p\n", [FEvent]),
    %% fixme: handle all matches!
    case lists:keysearch(Ref, #path_ref.ref, State#state.lib_refs) of
	false ->
	    {noreply, State};
	{value, PathRef} ->
	    case PathRef#path_ref.target of
		[] ->
		    case PathRef#path_ref.type of
			lib ->
			    handle_erl_libs(FEvent, PathRef, State);
			app ->
			    handle_erl_apps(FEvent, PathRef, State)
		    end;
		_DirList ->
		    resolve_erl_libs(FEvent,PathRef, State)
	    end
    end;
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
terminate(_Reason, State) ->
    lists:foreach(
      fun(#path_ref{ref=Ref}) -> fnotify:unwatch(Ref)
      end, State#state.lib_refs).

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

watch_dir_list(Ds, Type) ->
    watch_dir_list(Ds, [], Type).

watch_dir_list([Dir0|Ds], PathRefs, Type) ->
    Dir = filename:absname(Dir0),
    case fnotify:is_dir(Dir) of
	true ->
	    {_,PathRefs1} = watch_dir(Dir, [], PathRefs, Type),
	    watch_dir_list(Ds, PathRefs1, Type);
	false ->
	    %% here we may add a hook to watch if file (or whatever)
	    %% is converted into a directory
	    io:format("ignore '~s' not a directory\n", [Dir]),
	    watch_dir_list(Ds, PathRefs, Type);
	{error,enoent} ->
	    %% find first existing parent and add a watch list
	    _ParentList=[Parent|Target] = target_list(Dir,true),
	    ?dbg("delayed: watching ~s ~p\n", [Type,_ParentList]),
	    {_,PathRefs1} = watch_dir(Parent, Target, PathRefs, Type),
	    watch_dir_list(Ds, PathRefs1, Type);
	_Error ->
	    %% fixme check in pathRef for ref first
	    io:format("unable to wath '~s' error:~p\n",
		      [Dir, _Error]),
	    watch_dir_list(Ds, PathRefs, Type)
    end;
watch_dir_list([], PathRefs, _Type) ->
    lists:reverse(PathRefs).

watch_dir(Dir, Target, PathRefs, Type) ->
    case fnotify:watch(Dir) of
	{ok,Ref} ->
	    io:format("watching directory ~p '~p' target=~p\n", 
		      [Dir,Type,Target]),
	    P = #path_ref{ref=Ref,type=Type,path=Dir,target=Target},
	    {true,[P|PathRefs]};
	_Error ->
	    %% fixme check in pathRef for ref first
	    io:format("unable to watch ~s '~s' error:~p\n",
		      [Type, Dir, _Error]),
	    {false,PathRefs}
    end.

%%
%%  Construct a list of all parent paths upto root
%%  Optionally filter all existing directories away
%%  except for one.
%%
target_list(Dir, _LastExisting=true) ->
    TargetList = target_list(Dir),
    strip_existing(TargetList);
target_list(Dir, false) ->
    target_list(Dir).

%%
%% Generate a ancestor list
%%
target_list(Dir) ->
    Items = filename:split(Dir),
    target_list_(Items, [], []).

target_list_([Item|Items], Dir, DirList) ->
    Dir1 = filename:join(Dir, Item),
    target_list_(Items, Dir1, [Dir1|DirList]);
target_list_([], _Dir, DirList) ->
    lists:reverse(DirList).

%% strip away existing directories but ONE.
strip_existing(Ds) ->
    strip_existing(Ds, "").

strip_existing(Ds0=[D|Ds],Exist) ->
    case fnotify:is_dir(D) of
	true -> strip_existing(Ds, D);
	_ -> [Exist|Ds0]
    end;
strip_existing([],Exist) ->
    [Exist].
%%
%% Handle application path 
%%
handle_erl_apps({fevent,_Ref,Flags,Path,Name}, PathRef, State) ->
    case was_created(Flags) of
	true when Name =:= "ebin" ->
	    if PathRef#path_ref.loaded ->
		    {noreply, State };
	       true ->
		    EBin = filename:join(Path,Name),
		    ?dbg("autopath add: ~p\n", [EBin]),
		    gen_server:cast(fnotify_autoload, {add_autopath,EBin}),
		    Ref = PathRef#path_ref.ref,
		    PathRef1=PathRef#path_ref { loaded=true },
		    Refs=lists:keyreplace(Ref,#path_ref.ref,
					  State#state.lib_refs,PathRef1),
		    {noreply, State#state { lib_refs = Refs }}
	    end;
	true ->
	    {noreply, State };
	false ->
	    case was_deleted(Flags) of
		true when Name =:= "ebin" ->
		    EBin = filename:join(Path,Name),
		    ?dbg("autopath del: ~p\n", [EBin]),
		    gen_server:cast(fnotify_autoload, {del_autopath,EBin}),
		    {noreply, State };
		_ ->
		    {noreply, State}
	    end
    end.
	    
%%
%% Find Ref among paths, add or delete paths
%%
handle_erl_libs({fevent,_Ref,Flags,Path,Name}, _PathRef, State) ->
    case was_created(Flags) of
	true ->
	    PathName = filename:join(Path,Name),
	    Ls = monitor_app_path(PathName, State#state.lib_refs),
	    {noreply, State#state{lib_refs = Ls}};
	false ->
	    case was_deleted(Flags) of
		true when Name =:= "ebin" ->
		    gen_server:cast(fnotify_autoload, {del_autopath, Path}),
		    {noreply, State };
		_ ->
		    {noreply, State}
	    end
    end.

monitor_app_path(Path, Ls0) ->
    case fnotify:is_dir(Path) of
	true ->
	    ?dbg("~s is a directory\n", [Path]),
	    %% start monitor PathName for creation/deletion of ebin
	    case watch_dir(Path,[],Ls0, app) of
		{true,Ls1=[L|Ls]} ->
		    ?dbg("watch directory ~s\n", [Path]),
		    %% check if ebin already exist!
		    EBinPath = filename:join(Path, "ebin"),
		    case fnotify:is_dir(EBinPath) of
			true ->
			    ?dbg("~s already exist add to autoload\n",
				 [EBinPath]),
			    gen_server:cast(fnotify_autoload, 
					    {add_autopath, EBinPath}),
			    [L#path_ref {loaded=true}|Ls];
			_ ->
			    Ls1
		    end;
		{false,Ls1} ->
		    Ls1
	    end;
	_Error ->
	    ?dbg("monitor ~s is not directory ~p\n", [Path,_Error]),
	    Ls0
    end.
    
%%
%% Resolve an unresolve path name move watch 
%%
resolve_erl_libs({fevent,_Ref,Flags,Path,Name}, PathRef, State) ->
    case was_created(Flags) of
	true ->
	    PathName = filename:join(Path,Name),
	    case PathRef#path_ref.target of
		[PathName|Target] ->  %% move on step towards target
		    Ref0 = PathRef#path_ref.ref,
		    fnotify:unwatch(Ref0),
		    case fnotify:watch(PathName) of
			{ok,Ref1} ->
			    PathRef1=PathRef#path_ref { ref=Ref1,target=Target},
			    Refs=lists:keyreplace(Ref0,#path_ref.ref,
						  State#state.lib_refs,
						  PathRef1),
			    {noreply,State#state { lib_refs=Refs}};
			_Error ->
			    ?dbg("unabled to watch '~s' error:~p\n", 
				 [PathName, _Error]),
			    {noreply,State}
		    end;
		_Target ->
		    ?dbg("error, path '~s' not in target ~p\n", 
			 [PathName, _Target]),
		    {noreply,State}
	    end;
	false ->
	    %% FIXME handle delete
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

was_deleted(Flags) ->
    case lists:member(delete, Flags) of
	true ->
	    true;
	false ->
	    case lists:member(moved_from, Flags) of
		true ->
		    true;
		false ->
		    false
	    end
    end.
