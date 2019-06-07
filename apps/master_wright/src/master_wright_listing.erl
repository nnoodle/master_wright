-module(master_wright_listing).

-behaviour(gen_server).

%% API
-export([start_link/0, first/0, add/1, nth/1, all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {counter = 0}).
-record(advert, {port, name, description, ip, agent}). % copied from client

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Ad) ->
    gen_server:cast(master_wright_listing, {add, Ad}).

first() ->
    gen_server:call(master_wright_listing, first).

nth(I) ->
    gen_server:call(master_wright_listing, {nth, I}).

all() ->
    gen_server:call(master_wright_listing, all).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% put this into state?
    ets:new(master_wright_listing, [ordered_set, named_table]),
    {ok, #state{}, infinity}.

handle_call(first, _From, State) ->
    Reply = case ets:lookup(master_wright_listing, ets:first(master_wright_listing)) of
                [{_, Ad} | _] -> {ok, Ad};
                _ -> {error, no_servers}
            end,
    {reply, Reply, State};
handle_call({nth, N}, _From, State) ->
    %% negative values are OK
    Reply = case string:to_integer(N) of
                {I, _} ->
                    case ets:lookup(master_wright_listing, ets:next(master_wright_listing, I)) of
                        [{_, Ad} | _] -> {ok, {I, Ad}};
                        [] -> {error, out_of_bounds}
                    end;
                _ -> {error, invalid_integer}
            end,
    {reply, Reply, State};
handle_call(all, _From, State) ->
    Reply = ets:foldr(fun ({_,{Port,Name,Desc,Ip,_Agent}}, Acc) ->
                              [{Name,Desc,Ip,Port}|Acc]
                      end, [], master_wright_listing),
    {reply, Reply, State}.

handle_cast(reset, State) ->
    {noreply, State#state{counter=0}};
handle_cast({add, #advert{ip=Ip}=Adv}, #state{counter=Count}=State) ->
    Ad = dropname(Adv),
    Insert = case ets:match(master_wright_listing, {'$1', {'_','_','_', Ip,'_'}}) of
        [] -> {State#state.counter, Ad};
        [[N] | _] -> {N, Ad}
    end,
    %% Insert = {State#state.counter, Ad},
    ets:insert(master_wright_listing, Insert),
    {noreply, State#state{counter=Count+1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
dropname({_, Port, Name, Description, Ip, Agent}) ->
    {Port, Name, Description, Ip, Agent}.
