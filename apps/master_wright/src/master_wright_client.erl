-module(master_wright_client).

-behaviour(gen_server).

%% API
-export([start_link/2, spawn/2, recv/2, send_ct/3, ban/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(STATE, #state{socket=Socket, transport=Transport}=State).
-define(AOMS, "AOMS").

-record(state, {socket, transport, advert, hdid = "undefined"}).
-record(advert, {port, name, description, ip, agent}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Socket, Transport) ->
    gen_server:start_link(?MODULE, [Socket, Transport], []).

spawn(Socket, Transport) ->
    master_wright_client_sup:start_child(Socket, Transport).

recv(Pid, [Header | Rest]) ->
    gen_server:cast(Pid, {client_header_to_atom(Header), Rest});
recv(_Pid, Data) ->
    io:format("DATA: ~p~n", [Data]),
    ok.

ban(Pid) ->
    gen_server:cast(Pid, doom).

list() ->
    lists:map(fun ({_,Pid,_,_}) ->
                      {ok, Ip, Hdid} = gen_server:call(Pid, ids),
                      {Pid, Ip, Hdid}
              end,
              supervisor:which_children(master_wright_client_sup)).

send_ct(Pid, Username, Message) ->
    gen_server:cast(Pid, {send_ct, Username, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, Transport]) ->
    {ok, {Ip, _}}= Transport:peername(Socket),
    io:format("CONNECTION: ~p (pid: ~p)~n", [Ip, self()]),
    case master_wright_ban:is_banned(Ip) of
        {ok, true} -> {stop, banned};
        _ ->
            Transport:send(Socket, master_wright_netcode:encode([servercheok, "2.6.1"])),
            Transport:send(Socket, master_wright_netcode:encode(['AO2CHECK', "0.0.0"])),
            %% timeout is handled by master_wright_protocol
            {ok, #state{socket = Socket, transport = Transport}, infinity}
        end.

handle_call(id, _From, ?STATE) ->
    {ok, {Ip, _}} = Transport:peername(Socket),
    {reply, {ok, Ip}, State};
handle_call(advert, _From, #state{advert=Advert}=State) ->
    {reply, {ok, Advert}, State};
handle_call(Request, _From, State) ->
    io:format("CALL: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast({ping, _}, ?STATE) ->
    Transport:send(Socket, master_wright_netcode:encode(
                             case State#state.advert of
                                 undefined -> 'NOSERV';
                                 _ -> 'PONG'
                             end)),
    {noreply, State};
handle_cast({id, [_Agent, _Version | _]}, State) ->
    send_ct(self(), ?AOMS,
            application:get_env(master_wright, motd, "Welcome to Attorney Online!")),
    {noreply, State};
handle_cast({vc, _}, ?STATE) ->
    Transport:send(Socket, master_wright_netcode:encode(['SV', "master_wright"])),
    {noreply, State};
handle_cast({ct, [Username, Message | _]}, State) ->
    Cmd = process_ct(Message, State),
    Enabled = application:get_env(master_wright, master_chat, true),
    if
        is_list(Cmd) -> send_ct(self(), ?AOMS, Cmd);
        Enabled ->
            lists:foreach(
              fun ({_,Pid,_,_}) ->
                      send_ct(Pid, Username, Message)
              end,
              supervisor:which_children(master_wright_client_sup));
       true ->
            send_ct(self(), ?AOMS,
                    "Master chat is disabled. You need to switch to the server chat")
    end,
    {noreply, State};
handle_cast({scc, [Port, Name, Description, Agent | _]}, ?STATE) ->
    {ok, {Ip, _}} = Transport:peername(Socket),
    Advert = #advert{port=Port,
                     name=Name,
                     description=Description,
                     ip=inet:ntoa(Ip),
                     agent=Agent},
    master_wright_listing:add(Advert),
    Transport:send(Socket, master_wright_netcode:encode(['PSDD', 0])),
    {noreply, State#state{advert=Advert}};
handle_cast({hi, [HDID | _]}, State) ->
    {noreply, State#state{hdid=binary_to_list(HDID)}};
handle_cast({all, _}, ?STATE) ->
    Servers = master_wright_listing:all(),
    Transport:send(Socket, master_wright_netcode:encode(
                             case length(Servers) of
                                 0 -> 'ALL';
                                 _ -> ['ALL' | Servers]
                             end)),
    {noreply, State};
handle_cast({askforservers, _}, ?STATE) ->
    case master_wright_listing:first() of
        {ok, {Port, Name, Desc, Ip, Agent}} ->
            Transport:send(Socket, master_wright_netcode:encode(
                                     ['SN', 0, Ip, Agent, Port, Name, Desc]));
        _ -> noop
    end,
    {noreply, State};
handle_cast({sr, [I | _]}, ?STATE) ->
    case master_wright_listing:nth(I) of
        {ok, {Index, {Port, Name, Desc, Ip, Agent}}} ->
            Transport:send(Socket, master_wright_netcode:encode(
                                     ['SN', Index, Ip, Agent, Port, Name, Desc]));
        _ -> noop
    end,
    {noreply, State};
handle_cast({unknown, _}, State) ->
    {noreply, State};
% internal
handle_cast({send_ct, Username, Message}, ?STATE) ->
    Transport:send(Socket, master_wright_netcode:encode(['CT', Username, Message])),
    {noreply, State};
handle_cast(check, ?STATE) ->
    Transport:send(Socket, master_wright_netcode:encode('CHECK')),
    {noreply, State};
handle_cast(doom, ?STATE) ->
    {ok, {Ip, _}} = Transport:peername(Socket),
    {atomic, ok} = master_wright_ban:add(Ip),
    Transport:send(Socket, master_wright_netcode:encode('DOOM')),
    {stop, banned, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    io:format("INFO: timeout~n"),
    {stop, normal, State};
handle_info(Info, State) ->
    io:format("INFO: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, #state{socket=Socket, transport=Transport}) ->
    io:format("TERM REASON: ~p~n", [Reason]),
    ok = Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
client_header_to_atom(Header) ->
    case Header of
        <<"PING">> -> ping;
        <<"ID">> -> id;
        <<"VC">> -> vc;
        <<"CT">> -> ct;
        <<"HI">> -> hi;
        <<"SCC">> -> scc;
        <<"ALL">> -> all;
        %% AO1 deprecated(?) serverlist calls
        <<"askforservers">> -> askforservers;
        <<"SR">> -> sr;
        _ -> unknown
    end.

%% @private
process_ct(Message, State) ->
    HDID = string:prefix(Message, "/hdid"),
    if HDID =/= nomatch -> "Your HDID is " ++ State#state.hdid;
       true -> false
    end.
