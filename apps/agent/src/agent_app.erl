%%%-------------------------------------------------------------------
%% @doc agent public API
%% @end
%%%-------------------------------------------------------------------

-module(agent_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  ok = lager:start(),
  ok = sync:go(),
  ok = application:start(ranch),
  {ok, _} = ranch:start_listener(agent, 10,
    ranch_tcp, [{port, 5555}],
    agent_protocol, []
  ),
  agent_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
