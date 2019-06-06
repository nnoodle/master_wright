-module(master_wright_netcode).
-export([encode/1, decode/1]).

%% decode("CT#foo#bar#%") → [ "CT", "foo", "bar" ]
%% decode("ALL#foo&bar&1.1.1.1&1234#%") → [ "ALL", [ "foo", "bar", "1.1.1.1", "1234" ] ]
%% encode(["ALL",["foo","bar","1.1.1.1","1234"]]) → "ALL#foo&bar&1.1.1.1&1234#%"

encode(Datum) when is_atom(Datum) ->
    iolist_to_binary(atom_to_list(Datum) ++ "#%");
encode(Datum) ->
    Func = fun(E) ->
                case E of
                    {Name, Desc, Ip, Port} ->
                        [Name, "&", Desc, "&", Ip, "&", Port, "#"];
                    A when is_atom(A) -> [atom_to_list(A), "#"];
                    N when is_integer(N) -> [integer_to_list(N), "#"];
                    _ -> [E, "#"]
                end
        end,
    iolist_to_binary(lists:flatten(lists:map(Func, Datum)) ++ "%").

decode(Data) ->
    lists:droplast(binary:split(Data, <<"#">>, [global])).
