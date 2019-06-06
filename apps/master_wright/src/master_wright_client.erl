-module(master_wright_client).

-behaviour(gen_server).

%% API
-export([start_link/2, spawn/2, recv/2, send_ct/3, doom/1,
        motd/0]).

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

doom(Pid) ->
    gen_server:cast(Pid, doom).

send_ct(Pid, Username, Message) ->
    gen_server:cast(Pid, {send_ct, Username, Message}).

motd() ->
    "Welcome to Attorney Online!
This is the master server.".

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket, Transport]) ->
    io:format("CONNECTION: ~p (pid: ~p)~n", [Transport:peername(Socket), self()]),
    %% timeout is handled by master_wright_protocol
    {ok, #state{socket = Socket, transport = Transport}, infinity}.

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
    send_ct(self(), ?AOMS, motd()),
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
                      if
                          Pid =/= self() -> send_ct(Pid, Username, Message);
                          true -> ok
                      end
              end,
              supervisor:which_children(master_wright_client_sup));
       true ->
            send_ct(self(), ?AOMS,
                    "Master chat is disabled. You need to switch to the server chat")
    end,
    {noreply, State};
handle_cast({scc, [Port, Name, Description, Agent | _]}, ?STATE) ->
    {ok, {Ip, _}} = Transport:peername(Socket),
    Transport:send(Socket, master_wright_netcode:encode(['PSDD', 0])),
    {noreply, State#state{advert=#advert{port=Port,
                                         name=Name,
                                         description=Description,
                                         ip=inet:ntoa(Ip),
                                         agent=Agent}}};
handle_cast({hi, [HDID | _]}, State) ->
    {noreply, State#state{hdid=binary_to_list(HDID)}};
handle_cast({all, _}, ?STATE) ->
    Servers = lists:foldl(
                fun ({_,Pid,_,_}, Acc) ->
                        if self() =/= Pid ->
                                case gen_server:call(Pid, advert, 3000) of
                                    {ok, {_, Port,Name,Desc,Ip,_}} -> [{Name, Desc, Ip, Port} | Acc];
                                    _ -> Acc
                                end;
                           %% when self() == Pid
                           State#state.advert =/= undefined ->
                                {_, Port,Name,Desc,Ip,_} = State#state.advert,
                                [{Name,Desc,Ip,Port} | Acc];
                           true -> Acc
                        end
                end, [], supervisor:which_children(master_wright_client_sup)),
    Transport:send(Socket, master_wright_netcode:encode(
                             case length(Servers) of
                                 0 -> 'ALL';
                                 _ -> ['ALL' | Servers]
                             end)),
    {noreply, State};
handle_cast({unknown, _}, State) ->
    {noreply, State};
% internal
handle_cast({send_ct, Username, Message}, ?STATE) ->
    Transport:send(Socket, master_wright_netcode:encode(['CT', Username, Message])),
    {noreply, State};
handle_cast(doom, ?STATE) ->
    Transport:send(Socket, master_wright_netcode:encode('DOOM')),
    {noreply, State};
handle_cast(Request, State) ->
    io:format("CAST: ~p~n", [Request]),
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
%% @doc we need this to drop any malicious headers, if any.
client_header_to_atom(Header) ->
    case Header of
        <<"PING">> -> ping;
        <<"ID">> -> id;
        <<"VC">> -> vc;
        <<"CT">> -> ct;
        <<"HI">> -> hi;
        <<"SCC">> -> scc;
        <<"ALL">> -> all;
        %% TODO
        %% <<"SR">> -> sr;
        %% <<"SN">> -> sn;
        _ -> unknown
    end.

%% @private
process_ct(Message, State) ->
    HDID = string:prefix(Message, "/hdid"),
    if HDID =/= nomatch -> "Your HDID is " ++ State#state.hdid;
       true -> false
    end.