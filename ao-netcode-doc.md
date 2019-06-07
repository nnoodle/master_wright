# Master server

(this was yanked from [here](https://github.com/AttorneyOnline/AO2Protocol) for quick reference)

Upon connecting, the master server will send the `servercheok` and `AO2CHECK` messages.

| Function                                | Message                                                           | Direction     | Response        |
|-----------------------------------------|-------------------------------------------------------------------|---------------|-----------------|
| Check                                   | `servercheok#2.6.0#%`                                             | Client        | -               |
| Check (AO2)                             | `AO2CHECK#0.0.0#%`                                                | Client        | `ID`, `HI`      |
| Chat                                    | `CT#[username]#[message]#%`                                       | Client/Server | `CT`            |
| Version check                           | `VC#%`                                                            | Server        | `SV`            |
| Client version (AO2)                    | `ID#[client software]#[version]#%`                                | Server        | `CT` (MOTD)     |
| Hard drive ID (or anything in practice) | `HI#[hdid]#%`                                                     | Server        | -               |
| Get first server (AO1)                  | `askforservers#%`                                                 | Server        | `SN`            |
| Get server entry (AO1)                  | `SR#[id]#%`                                                       | Server        | `SN`            |
| Server entry (paginated) (AO1)          | `SN#[entry number]#[ip]#[server software]#[port]#[name]#[desc]#%` | Client        | -               |
| Get all servers (AO2)                   | `ALL#%`                                                           | Server        | `ALL`           |
| Server entry (all) (AO2)                | `ALL#[[name]&[desc]&[ip]&[port]#]%`                               | Client        | -               |
| Server version                          | `SV#[version]#%`                                                  | Client        | -               |
| Ping                                    | `PING#%`                                                          | Server        | `NOSERV`/`PONG` |
| Pong (when server advertised)           | `PONG#%`                                                          | Client        | -               |
| Server not advertised                   | `NOSERV#%`                                                        | Advertiser    | -               |
| Heartbeat                               | `SCC#[port]#[name]#[description]#[server software]#%`             | Server        | `PSDD`          |
| Heartbeat success                       | `PSDD#0#%`                                                        | Advertiser    | -               |
| Keepalive                               | `CHECK#%`                                                         | Advertiser    | -               |
| Global ban                              | `DOOM#%`                                                          | Client        | -               |
