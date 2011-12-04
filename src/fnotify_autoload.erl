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

-record(state, 
	{
	  path_ref = []   %% list of {Path,Ref}
	}).

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
    %% Fixme refresh / hook code path
    PathRef = lists:foldl(
		fun(P,Ps) ->
			case fnotify:watch(P) of
			    {ok,Ref} ->
				io:format("watch path: ~s\n", [P]),
				[{P,Ref}|Ps];
			    _Error ->
				Ps
			end
		end, [], code:get_path()),
    {ok, #state{path_ref=PathRef}}.

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
handle_info(FEvent={fevent,_Ref,Flags,Path,Name}, State) ->
    case lists:member(create, Flags) of
	true ->
	    case filename:extension(Name) of
		".beam" ->
		    BaseName = filename:basename(Name, ".beam"),
		    FileName = filename:join(Path,BaseName),
		    code:purge(list_to_atom(BaseName)),
		    case code:load_abs(FileName) of
			ok ->
			    {noreply,State};
			{module,_Mod} ->
			    {noreply,State};
			{error, Reason} ->
			    io:format("unable to auto-load module ~s, ~p\n",
				      [BaseName,Reason]),
			    {noreply,State}
		    end;
		_Ext ->
		    io:format("ignore event: ~p\n", [FEvent]),	    
		    {noreply,State}
	    end;
	false ->
	    io:format("ignore event: ~p\n", [FEvent]),	    
	    {noreply,State}
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
      fun({_Path,Ref}) ->
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
