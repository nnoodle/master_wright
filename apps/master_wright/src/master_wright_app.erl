-module(master_wright_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(
                master_wright_listener, ranch_tcp,
                #{num_acceptors => application:get_env(master_wright, num_acceptors, 10),
                  max_connections => application:get_env(master_wright, max_connections, 1024),
                  socket_opts => [{port, application:get_env(master_wright, port, 27016)}]},
                master_wright_protocol, []),
    master_wright_ban:init(),
    master_wright_client_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
