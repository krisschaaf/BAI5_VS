-module(app).

-export([init/0, start/1, stop/1]).

init() ->
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    {ok,[{botname,commbot},
     {CbCast1Name,CbCast1Node},
     {CbCast2Name,CbCast2Node},
     {CbCast3Name,CbCast3Node},
     {servercbcname,TowerCBCName},
     {servercbcnode,TowerCBCNode},
     {serverclockname,TowerClockName},
     {serverclocknode,TowerClockNode}]} = file:consult("app.cfg"),

    case net_adm:ping(CbCast1Node) of
        pong -> 
            util:logging(Datei, "CbCast1 "++util:to_String(CbCast1Name)++" integrated via ping by Server "++util:to_String(CbCast1Node)++"\n");
        pang -> 
            util:logging(Datei, "CbCast1 "++util:to_String(CbCast1Name)++" not integrated via ping by Server "++util:to_String(CbCast1Name)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(CbCast2Node) of
        pong -> 
            util:logging(Datei, "CbCast2 "++util:to_String(CbCast2Name)++" integrated via ping by Server "++util:to_String(CbCast2Node)++"\n");
        pang -> 
            util:logging(Datei, "CbCast2 "++util:to_String(CbCast2Name)++" not integrated via ping by Server "++util:to_String(CbCast2Name)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(CbCast3Node) of
        pong -> 
            util:logging(Datei, "CbCast3 "++util:to_String(CbCast3Name)++" integrated via ping by Server "++util:to_String(CbCast3Node)++"\n");
        pang -> 
            util:logging(Datei, "CbCast3 "++util:to_String(CbCast3Name)++" not integrated via ping by Server "++util:to_String(CbCast3Name)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(TowerCBCNode) of
        pong -> 
            util:logging(Datei, "TowerCBC "++util:to_String(TowerCBCName)++" integrated via ping by Server "++util:to_String(TowerCBCNode)++"\n");
        pang -> 
            util:logging(Datei, "TowerCBC "++util:to_String(TowerCBCName)++" not integrated via ping by Server "++util:to_String(TowerCBCName)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(TowerClockNode) of
        pong -> 
            util:logging(Datei, "TowerClock "++util:to_String(TowerClockName)++" integrated via ping by Server "++util:to_String(TowerClockNode)++"\n");
        pang -> 
            util:logging(Datei, "TowerClock "++util:to_String(TowerClockName)++" not integrated via ping by Server "++util:to_String(TowerClockName)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    util:logging(Datei, "All servers integrated, initialize comms...\n"),

    PidCbCast1 = rpc:call(CbCast1Node, cbCast, init, []),
    PidCbCast2 = rpc:call(CbCast2Node, cbCast, init, []),
    PidCbCast3 = rpc:call(CbCast3Node, cbCast, init, []),

    global:register_name(CbCast1Name, PidCbCast1),
    global:register_name(CbCast2Name, PidCbCast2),
    global:register_name(CbCast3Name, PidCbCast3),

    util:logging(Datei, "All comms initialized, initialize Tower...\n"),

    PidTowerClock = rpc:call(TowerClockNode, towerClock, init, []),
    PidTowerCBC = rpc:call(TowerCBCNode, towerCBC, init, [manu]),

    global:register_name(TowerClockName, PidTowerClock),
    global:register_name(TowerCBCName, PidTowerCBC),

    util:logging(Datei, "Tower initialized, initialize app...\n"),

    Tower = {{TowerClockName, TowerClockNode}, {TowerCBCName, TowerClockNode}},
    Comms = {{CbCast1Name, CbCast1Node}, {CbCast2Name, CbCast2Node}, {CbCast3Name, CbCast3Node}},

    PID = spawn(fun() -> loop(Datei, Tower, Comms) end),
    register(app, PID),

    util:logging(Datei, "App initialized on Node "++util:to_String(erlang:node())++" with PID: "++util:to_String(PID)++"\n\n\n").

start(PID) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    PID ! {self(), {start}},
    receive
        {replyapp, ok_start} -> 
            true
        after 5000 ->
            util:logging(Datei, "Timeout: App not started\n")
    end.

stop(PID) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    PID ! {self(), {stop}},
    receive
        {replyapp, ok_stop} -> 
            true
        after 5000 ->
            util:logging(Datei, "Timeout: App not stopped, killing now...\n"),
            exit(whereis(PID), ok)
    end.

loop(Datei, Tower, Comms) ->
    receive
        {From, {start}} when is_pid(From) ->
            util:logging(Datei, "Starting app...\n"),
            run(Datei, Tower, Comms),
            From ! {replyapp, ok_start},
            loop(Datei, Tower, Comms);
        {From, {stop}} when is_pid(From) ->
            util:logging(Datei, "Stopping app...\n"),
            From ! {replyapp, ok_stop},
            loop(Datei, Tower, Comms);
        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, Tower, Comms)
    end.

run(Datei, Tower, Comms) -> 
    {{TowerClockName, TowerClockNode}, {TowerCBCName, TowerClockNode}} = Tower,
    {{CbCast1Name, CbCast1Node}, {CbCast2Name, CbCast2Node}, {CbCast3Name, CbCast3Node}} = Comms,

    R1 = rpc:call(CbCast1Node, cbCast, send, [CbCast1Name, "1"]),
    util:logging(Datei, "Sent 1 to CbCast1: "++util:to_String(R1)++"\n"),
    R2 = rpc:call(CbCast2Node, cbCast, send, [CbCast2Name, "2"]),
    util:logging(Datei, "Sent 2 to CbCast2: "++util:to_String(R2)++"\n"),
    R3 = rpc:call(CbCast1Node, cbCast, send, [CbCast1Name, "3"]),
    util:logging(Datei, "Sent 3 to CbCast1: "++util:to_String(R3)++"\n").

