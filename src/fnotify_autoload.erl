%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    code autoloader
%%% @end
%%% Created :  4 Dec 2011 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(fnotify_autoload).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%
%% FIXME: watch directories in ERL_LIBS and code:lib_dir()
%%        and update code:path  (fnotify_autopath)
%%

-record(state,
	{
	  libs_ref = [],  %% list of {Ref,Path}
	  path_ref = []   %% list of {Ref,Path}
	}).

-ifdef(debug).
-define(dbg(F, A), io:format((F), (A))).
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
    Libs0 = case os:getenv("ERL_LIBS") of
		false -> [];
		ErlLibs -> string:tokens(ErlLibs, ":")
	    end,
    Libs = [code:lib_dir()] ++ Libs0,
    LibRef = watch_dir_list(Libs),
    PathRef = watch_dir_list(code:get_path()),
    {ok, #state{ libs_ref=LibRef, path_ref=PathRef}}.

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
    case lists:keysearch(Ref, 1, State#state.libs_ref) of
	{value, _} ->
	    handle_erl_libs(FEvent, State);
	false ->
	    handle_erl_path(FEvent, State)
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

watch_dir_list(Ds) ->
    lists:foldl(
		fun(P,Ps) ->
			case fnotify:watch(P) of
			    {ok,Ref} ->
				?dbg("watch path: ~s\n", [P]),
				[{Ref,P}|Ps];
			    _Error ->
				?dbg("error unable to watch: ~s\n",[P]),
				Ps
			end
		end, [], Ds).
%%
%% Some file among the code paths has been change
%% If it is a beam file that has been created then load it
%% FIXME: ignore bea# create
%%        ignore bea# delete
%%        handle name.app file creations (reload app)?
%%        if . is in the code path try to handle . dynamically?
%%
handle_erl_path(_FEvent={fevent,_Ref,Flags,Path,Name}, State) ->
    case lists:member(create, Flags) of
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
			ok ->
			    ?dbg("load_abs ok\n", []),
			    {noreply,State};
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

handle_erl_libs({fevent,_Ref,Flags,Path,Name}, State) ->
    case lists:member(create, Flags) of
	true ->
	    %% - check if Name is a newly added directory 
	    %% - check if Name/ebin is a directory 
	    %%   if not then put watch(Name) and wait for ebin to be create
	    PathName = filename:join(Path,Name),
	    case fnotify:is_dir(PathName) of
		true ->
		    ?dbg("~s is a directory\n", [PathName]),
		    EBinPathName = filename:join(PathName, "ebin"),
		    case fnotify:is_dir(EBinPathName) of
			true ->
			    ?dbg("~s is a directory\n", [EBinPathName]);
			false ->
			    ?dbg("~s is not a directory\n", [EBinPathName])
		    end,
		    {noreply, State};
		false ->
		    {noreply, State}
	    end;
	false ->
	    {noreply, State}
    end.
