%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    kqueue based file event server
%%% @end
%%% Created :  3 Dec 2011 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(fnotify_kevent_srv).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").

-record(watch,
	{
	  pid,     %% pid watching
	  ref,     %% watch ref / monitor
	  wd,      %% watch descriptor
	  is_dir,  %% path is a directory
	  path,    %% path watched
	  dir_list %% directory list
	}).
	  
-record(state, 
	{
	  port,
	  watch_list = []
	}).

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
    Port = fnotify_drv:start(),
    fnotify_drv:activate(Port, 1),  %% activate once
    {ok, #state{ port = Port}}.

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
handle_call({watch,Pid,Path}, _From, State) when is_pid(Pid) ->
    case fnotify_drv:watch(State#state.port, Path) of
	{ok, Wd} ->
	    Ref = monitor(process, Pid),
	    IsDir = fnotify:is_dir(Path),
	    ListDir   = list_dir(IsDir, Path),
	    W = #watch { pid=Pid, ref=Ref, wd=Wd, path=Path, 
			 is_dir=IsDir, dir_list=ListDir },
	    Ws = [W|State#state.watch_list],
	    {reply, {ok,Ref}, State#state { watch_list=Ws }};
	Error ->
	    {reply, Error, State}
    end;
handle_call({unwatch,Ref}, _From, State) ->
    case lists:keytake(Ref, #watch.ref, State#state.watch_list) of
	{value, W, Ws} ->
	    fnotify_drv:unwatch(State#state.port, W#watch.wd),
	    demonitor(Ref, [flush]),
	    {reply, ok, State#state { watch_list = Ws }};
	false ->
	    {reply, {error,enoent}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = {error, bad_call},
    {reply, Reply, State}.

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
handle_info({fevent,Wd,Flags,Path,Name}, State) ->
    %% find all watch that as Wd as watch id
    Ws = lists:foldl(
	   fun(W,Ws1) ->
		   if W#watch.wd =:= Wd ->
			   if W#watch.is_dir ->
				   [dir_event(W,Flags)|Ws1];
			      true ->
				   W#watch.pid ! 
				       {fevent,W#watch.ref,Flags,Path,Name},
				   [W|Ws1]
			   end;
		      true ->
			   [W|Ws1]
		   end
	   end, [], State#state.watch_list),
    fnotify_drv:activate(State#state.port, 1),
    State1 = State#state { watch_list = Ws },
    {noreply, State1};
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    io:format("process down pid=~w, reason=~w\n", [_Pid,_Reason]),
    case lists:keytake(Ref, #watch.ref, State#state.watch_list) of
	{value, W, Ws} ->
	    fnotify_drv:unwatch(State#state.port, W#watch.wd),
	    {noreply, State#state { watch_list = Ws }};
	false ->
	    {noreply, State}
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
    erlang:port_close(State#state.port),
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

dir_event(W, Flags) ->
    case lists:member(write, Flags) of
	true ->
	    ListDir = list_dir(true, W#watch.path),
	    Added = ListDir -- W#watch.dir_list,
	    Removed = W#watch.dir_list -- ListDir,
	    {Renamed,Added1,Removed1} =	renamed(Added,Removed,[],[]),
	    lists:foreach(
	      fun({Name,_I}) ->
		      W#watch.pid ! 
			  {fevent,W#watch.ref,[create],W#watch.path,Name}
	      end, Added1),
	    lists:foreach(
	      fun({Name,_I}) ->
		      W#watch.pid ! 
			  {fevent,W#watch.ref,[delete],W#watch.path,Name}
	      end, Removed1),
	    lists:foreach(
	      fun({Name,OldName,I}) ->
		      W#watch.pid ! 
			  {fevent,W#watch.ref,[moved_from,{cookie,I}],
			   W#watch.path,OldName},
		      W#watch.pid ! 
			  {fevent,W#watch.ref,[moved_to,{cookie,I}],
			   W#watch.path,Name}
	      end, Renamed),
	    if Added1 =:= [], Removed1 =:= [], Renamed =:= [] ->
		    W#watch.pid ! 
			{fevent,W#watch.ref,Flags,W#watch.path,[]};
	       true ->
		    ok
	    end,
	    W#watch { dir_list = ListDir };
	false ->
	    W#watch.pid ! 
		{fevent,W#watch.ref,Flags,W#watch.path,[]},
	    W
    end.
	
renamed(Added,[R={Name,I}|Removed],Removed1,Renamed) ->
    case lists:keytake(I,2,Added) of
	{value,{NewName,I},Added1} ->
	    renamed(Added1,Removed,Removed1,[{NewName,Name,I}|Renamed]);
	false ->
	    renamed(Added,Removed,[R|Removed1],Renamed)
    end;
renamed(Added, [], Removed1, Renamed) ->
    {Renamed,Added,Removed1}.
    
list_dir(true, Path) ->
    case file:list_dir(Path) of
	{ok, Files} ->
	    lists:map(fun(Name) ->
			      FileName = filename:join(Path,Name),
			      case file:read_file_info(FileName) of
				  {ok,Info} ->
				      {Name, Info#file_info.inode};
				  _Error ->
				      {Name, 0}
			      end
		      end, Files);
	_ ->
	    []
    end;
list_dir(false, _Path) ->
    [].
