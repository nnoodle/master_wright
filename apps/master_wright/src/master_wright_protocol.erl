-module(master_wright_protocol).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

-record(state, {pid, timeout}). % it's called state, but it's not suppose to change.

start_link(Ref, _Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    {ok, Pid} = master_wright_client:spawn(Socket, Transport),
    loop(Socket, Transport,
         #state{pid=Pid, timeout=application:get_env(master_wright, timeout, 30*1000)}).

%% @private
loop(Socket, Transport, #state{pid=Pid, timeout=_Timeout}=State) ->
    case Transport:recv(Socket, 0, infinity) of
        {ok, Data} ->
            master_wright_client:recv(Pid, master_wright_netcode:decode(Data)),
            loop(Socket, Transport, State);
        _ ->
            ok = Transport:close(Socket),
            Pid ! timeout
    end.
