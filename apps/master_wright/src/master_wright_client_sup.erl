-module(master_wright_client_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Socket, Transport) ->
    supervisor:start_child(?SERVER, [Socket, Transport]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    Client = #{id => master_wright_client,
               start => {master_wright_client, start_link, []},
               restart => temporary,
               shutdown => brutal_kill,
               modules => [master_wright_client]},
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 3},
    {ok, {SupFlags, [Client]}}.

%%====================================================================
%% Internal functions
%%====================================================================
