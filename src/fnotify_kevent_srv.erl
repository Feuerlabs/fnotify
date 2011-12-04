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

%% API
-export([start/0]).
-export([start_link/0]).
-export([watch/1, unwatch/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(watch,
	{
	  pid,   %% pid watching
	  ref,   %% watch ref / monitor
	  wd,    %% watch descriptor
	  path,  %% path watched
	  list   %% directory list
	}).
	  
-record(state, 
	{
	  port,
	  watch_list = []
	}).

%%%===================================================================
%%% API
%%%===================================================================
watch(Path) ->
    gen_server:call(?SERVER, {watch,self(),Path}).

unwatch(Ref) ->
    gen_server:call(?SERVER, {unwatch, Ref}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
	    W = #watch { pid=Pid, ref=Ref, wd=Wd, path=Path, list=[]},
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
    lists:foreach(
      fun(W) ->
	      if W#watch.wd =:= Wd ->
		      W#watch.pid ! 
			  {fevent,W#watch.ref,Flags,Path,Name};
		 true ->
		      ok
	      end
      end, State#state.watch_list),
    fnotify_drv:activate(State#state.port, 1),
    {noreply, State};
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================





