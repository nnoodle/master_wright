-module(master_wright_listing_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Listing = #{id => master_wright_listing,
               start => {master_wright_listing, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [master_wright_listing]},
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    {ok, {SupFlags, [Listing]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
