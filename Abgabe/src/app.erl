-module(app).

-export([init/0, start/1, stop/1]).

startC(Name) ->
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    CommCBC = cbCast:init(),
    erlang:register(Name,CommCBC),
    util:logging(Datei, util:to_String(Name)++" locally registered with PID: "++util:to_String(CommCBC)++"\n"),
    CommCBC.

init() ->
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    {ok,[{botname,CbCastName},
     {botnodea,CbCast1Node},
     {botnodeb,CbCast2Node},
     {botnodec,CbCast3Node},
     {servercbcname,TowerCBCName},
     {servercbcnode,TowerCBCNode},
     {serverclockname,TowerClockName},
     {serverclocknode,TowerClockNode}]} = file:consult("app.cfg"),

     case net_adm:ping(TowerClockNode) of
        pong -> 
            util:logging(Datei, "TowerClock "++util:to_String(TowerClockName)++" integrated via ping by Server "++util:to_String(TowerClockNode)++"\n");
        pang -> 
            util:logging(Datei, "TowerClock "++util:to_String(TowerClockName)++" not integrated via ping by Server "++util:to_String(TowerClockName)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(TowerCBCNode) of
        pong -> 
            util:logging(Datei, "TowerCBC "++util:to_String(TowerCBCName)++" integrated via ping by Server "++util:to_String(TowerCBCNode)++"\n");
        pang -> 
            util:logging(Datei, "TowerCBC "++util:to_String(TowerCBCName)++" not integrated via ping by Server "++util:to_String(TowerCBCName)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(CbCast1Node) of
        pong -> 
            util:logging(Datei, "CbCast1 "++util:to_String(CbCastName)++" integrated via ping by Server "++util:to_String(CbCast1Node)++"\n");
        pang -> 
            util:logging(Datei, "CbCast1 "++util:to_String(CbCastName)++" not integrated via ping by Server "++util:to_String(CbCast1Node)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(CbCast2Node) of
        pong -> 
            util:logging(Datei, "CbCast2 "++util:to_String(CbCastName)++" integrated via ping by Server "++util:to_String(CbCast2Node)++"\n");
        pang -> 
            util:logging(Datei, "CbCast2 "++util:to_String(CbCastName)++" not integrated via ping by Server "++util:to_String(CbCast2Node)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    case net_adm:ping(CbCast3Node) of
        pong -> 
            util:logging(Datei, "CbCast3 "++util:to_String(CbCastName)++" integrated via ping by Server "++util:to_String(CbCast3Node)++"\n");
        pang -> 
            util:logging(Datei, "CbCast3 "++util:to_String(CbCastName)++" not integrated via ping by Server "++util:to_String(CbCast3Node)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    util:logging(Datei, "All servers integrated, initialize Tower...\n"),

    rpc:call(TowerClockNode, towerClock, init, []),
    rpc:call(TowerCBCNode, towerCBC, init, [manu]),
    
    util:logging(Datei, "Tower initialized, initialize Comms...\n"),
    
    spawn(CbCast1Node, fun() -> startC(CbCastName) end),
    spawn(CbCast2Node, fun() -> startC(CbCastName) end),
    spawn(CbCast3Node, fun() -> startC(CbCastName) end),

    Tower = {{TowerClockName, TowerClockNode}, {TowerCBCName, TowerCBCNode}},
    Comms = {{CbCastName, CbCast1Node}, {CbCastName, CbCast2Node}, {CbCastName, CbCast3Node}},

    util:logging(Datei, "Comms initialized, initialize App...\n"),

    PID = spawn(fun() -> loop(Datei, Tower, Comms) end),
    register(app, PID),

    util:logging(Datei, "App initialized on Node "++util:to_String(erlang:node())++" with PID: "++util:to_String(PID)++"\n\n").

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
            unregister(app),
            From ! {replyapp, ok_stop};
        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, Tower, Comms)
    end.

run(Datei, Tower, Comms) -> 
    {_, TowerCBC} = Tower,
    {CbCast1, CbCast2, CbCast3} = Comms,

    sendMessages(Datei, CbCast1, CbCast2),

    shuffleMessages(Datei, TowerCBC),

    forwardMessages(Datei, TowerCBC),

    readMessages(Datei, CbCast3),
    
    util:logging(Datei, "\nTest ended\n").
    

sendMessages(Datei, CbCast1, CbCast2) ->
    util:logging(Datei, "\nSending messages...\n"),

    R1 = cbCast:send(CbCast1, "1.1"),
    util:logging(Datei, "Sent 1.1 from CbCast1: " ++ util:to_String(R1) ++ "\n"),

    R2 = cbCast:send(CbCast2, "2.1"),
    util:logging(Datei, "Sent 2.1 from CbCast2: " ++ util:to_String(R2) ++ "\n"),

    R3 = cbCast:send(CbCast1, "1.2"),
    util:logging(Datei, "Sent 1.2 from CbCast1: " ++ util:to_String(R3) ++ "\n"),

    R4 = cbCast:send(CbCast2, "2.2"),
    util:logging(Datei, "Sent 2.2 from CbCast2: " ++ util:to_String(R4) ++ "\n"),

    R5 = cbCast:send(CbCast2, "2.3"),
    util:logging(Datei, "Sent 2.3 from CbCast2: " ++ util:to_String(R5) ++ "\n"),

    R6 = cbCast:send(CbCast1, "1.3"),
    util:logging(Datei, "Sent 1.3 from CbCast1: " ++ util:to_String(R6) ++ "\n"),

    R7 = cbCast:send(CbCast1, "1.4"),
    util:logging(Datei, "Sent 1.4 from CbCast1: " ++ util:to_String(R7) ++ "\n"),

    R8 = cbCast:send(CbCast1, "1.5"),
    util:logging(Datei, "Sent 1.5 from CbCast1: " ++ util:to_String(R8) ++ "\n"),

    R9 = cbCast:send(CbCast2, "2.4"),
    util:logging(Datei, "Sent 2.4 from CbCast2: " ++ util:to_String(R9) ++ "\n"),

    R10 = cbCast:send(CbCast2, "2.5"),
    util:logging(Datei, "Sent 2.5 from CbCast2: " ++ util:to_String(R10) ++ "\n").


shuffleMessages(Datei, Tower) ->
    util:logging(Datei, "\nShuffle messages...\n"),

    Tower ! {{app, erlang:node()}, {shuffleMessages}},
    receive
        {replycbc, ok_shuffleMessages, {OldBuffer, NewBuffer}} -> 
            util:logging(Datei, "\nOldBuffer: \n" ++ util:to_String(OldBuffer) ++ "\n"),
            util:logging(Datei, "\nNewBuffer: \n" ++ util:to_String(NewBuffer) ++ "\n")
    after 1000 -> 
        util:logging(Datei, "Timeout: ShuffleMessages not completed\n")
    end.

forwardMessages(Datei,  {_, TowerCBCNode}) ->
    util:logging(Datei, "\nForwarding messages...\n"),

    C1 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 1]),
    util:logging(Datei, "Forwarded 1 to CbCast3: " ++ util:to_String(C1) ++ "\n"),

    C2 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 2]),
    util:logging(Datei, "Forwarded 2 to CbCast3: " ++ util:to_String(C2) ++ "\n"),

    C3 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 3]),
    util:logging(Datei, "Forwarded 3 to CbCast3: " ++ util:to_String(C3) ++ "\n"),

    C4 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 4]),
    util:logging(Datei, "Forwarded 4 to CbCast3: " ++ util:to_String(C4) ++ "\n"),

    C5 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 5]),
    util:logging(Datei, "Forwarded 5 to CbCast3: " ++ util:to_String(C5) ++ "\n"),

    C6 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 6]),
    util:logging(Datei, "Forwarded 6 to CbCast3: " ++ util:to_String(C6) ++ "\n"),

    C7 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 7]),
    util:logging(Datei, "Forwarded 7 to CbCast3: " ++ util:to_String(C7) ++ "\n"),

    C8 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 8]),
    util:logging(Datei, "Forwarded 8 to CbCast3: " ++ util:to_String(C8) ++ "\n"),

    C9 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 9]),
    util:logging(Datei, "Forwarded 9 to CbCast3: " ++ util:to_String(C9) ++ "\n"),

    C10 = rpc:call(TowerCBCNode, towerCBC, cbcast, [3, 10]),
    util:logging(Datei, "Forwarded 10 to CbCast3: " ++ util:to_String(C10) ++ "\n").


readMessages(Datei, CbCast3) ->
    util:logging(Datei, "\nRead messages...\n"),
    
    N1 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N1) ++ "\n"),
    
    N2 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N2) ++ "\n"),

    N3 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N3) ++ "\n"),

    N4 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N4) ++ "\n"),

    N5 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N5) ++ "\n"),

    N6 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N6) ++ "\n"),

    N7 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N7) ++ "\n"),

    N8 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N8) ++ "\n"),

    N9 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N9) ++ "\n"),

    N10 = cbCast:read(CbCast3),
    util:logging(Datei, "Read from CbCast3: " ++ util:to_String(N10) ++ "\n").