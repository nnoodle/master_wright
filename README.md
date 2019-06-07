# master_wright

an attorney online 2 master server implementation.

## build

```shell
$ rebar3 compile
```

## config

configuration is done through editing the `config/sys.config` file.

```erlang
%% what a sys.config might look like
[
 {master_wright,
  [{num_acceptors, 10},                    % number of process that accept connections
   {max_connections, 1024},                % maximum number of active connections, use infinity to disable limit
   {port, 27016},                          % port to listen on
   {motd, "Welcome to Attorney Online!"},  % Message Of The Day (seen after clients connect)
   {master_chat, true}                     % whether or not master chat is enabled
  ]},
 %% options for Mnesia, an Erlang/OTP library master_wright uses
 {mnesia,
  [{dir, "~/.cache/master_wright/Mnesia"}  % where to store database
                                           % (defaults to the current directory)
  ]}
].
```
