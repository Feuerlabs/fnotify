%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
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
