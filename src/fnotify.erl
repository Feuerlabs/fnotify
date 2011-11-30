%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    fnotify client
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(fnotify).
-export([watch/1, unwatch/1]).

-define(SERVER, fnotify_srv). 

watch(Path) ->
    gen_server:call(?SERVER, {watch,self(),Path}).

unwatch(Ref) ->
    gen_server:call(?SERVER, {unwatch, Ref}).
