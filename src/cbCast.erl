-module(cbCast).

-export([init/0, stop/1, send/2, received/1, read/1]).

% init(): erstellt einen Prozess für die Kommunikationseinheit. Rückgabe ist ihre PID.
init() ->
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerCBC.cfg"),
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    case net_adm:ping(Servernode) of
        pong -> 
            util:logging(Datei, "TowerCBC "++util:to_String(Servername)++" von Server "++util:to_String(Servernode)++" per ping eingebunden\n");
        pang -> 
            util:logging(Datei, "TowerCBC "++util:to_String(Servername)++" von Server "++util:to_String(Servernode)++" per ping nicht eingebunden\n"),
            throw({error, "Server nicht erreichbar"})            
    end,

    CommCBC = spawn(fun() -> loop(Datei) end),
    register(cbCast,CommCBC),
    
    {Servername, Servernode} ! {self(), {register, CommCBC}},
    receive
        {replycbc, ok_registered} ->
            util:logging(Datei, "cbCast registered at "++util:to_String(Servername)++" with PID: "++util:to_String(CommCBC)++" on Node "++util:to_String(erlang:node())++"\n");
        {replycbc, ok_existing} ->
            util:logging(Datei, "cbCast already registered at "++util:to_String(Servername)++" with PID: "++util:to_String(CommCBC)++" on Node "++util:to_String(erlang:node())++"\n")
        after 1000 ->
            util:logging(Datei, "Timeout: cbCast not registered\n")
    end,
    CommCBC.

% stop(Comm): terminiert die Kommunikationseinheit mit PID Comm. Rückgabe ist done.
stop(Comm) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {stop}},
    receive
        {ok_stop} -> 
            unregister(cbCast), %TODO: get Comm pid
            done
        after 5000 ->
            util:logging(Datei, "Timeout: CbCast not stopped, killing now...\n"),
            exit(whereis(Comm), ok),
            unregister(cbCast)
    end.

% send(Comm,Message): sendet eine Nachricht Message über die Kommunikationseinheit Comm als kausaler Multicast an alle anderen in der Gruppe.
send(Comm, Message) -> 
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerCBC.cfg"),
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    cbCast ! {self(), {tickVT}}, % VT um 1 erhöhen
    receive 
        {ok_tickVT, VT} -> 
            {Servername, Servernode} ! {whereis(Comm), {multicastNB, {Message, VT}}}, % TODO: ist Comm und MulticastNB hier richtig?
            util:logging(Datei, "Message '"++util:to_String(Servername)++" with VT "++util:to_String(VT)++"' sent to "++util:to_String(Comm)++"\n"),
            pushDLQ(Message, VT)
        after 1000 -> 
            util:logging(Datei, "Timeout: Message '"++util:to_String(Servername)+"' could not be sent\n")
    end.

% received(Comm): empfängt blockierend eine Nachricht von der Kommunikationseinheit Comm. Die Rückgabe ist eine Zeichenkette.
received(Comm) -> 
    readMessage(Comm).

% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> spawn(fun() -> readMessage(Comm) end).

readMessage(Comm) ->
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), readMessage},
    receive 
        {ok_readMessage, Message} -> 
            case Message of
                "" -> null;
                _ -> Message
            end
        after 1000 -> util:logging(Datei, "Timeout: Could not receive Message\n")
    end.

castMessage(Message, VT, Datei) -> 
    util:logging(Datei, "Nachricht: "++util:to_String(Message)++" mit Vektorzeitstempel: "++util:to_String(VT)++" empfangen.\n"),
    pushHBQ(Message, VT),
    {}.

% Die initialisierung der VT muss aus dem richtigten Prozess heraus gestartet werden.
loop(Datei) ->
    VT = vectorC:initVT(),
    loop(Datei, VT, [], {[], VT}).

% {<PID>,{castMessage,{<Message>,<VT>}}}: als Nachricht. Empfängt die Nachricht <Message> mit Vektorzeitstempel <VT>. <PID> wird nicht benötigt.
% z.B. cbCast ! {self(), {castMessage, {"msg", 12}}}.
loop(Datei, VT, HBQ, DLQ) ->
	receive
        {_From, {castMessage, {Message, NewVT}}} ->
            castMessage(Message, NewVT, Datei),
            loop(Datei, VT, HBQ, DLQ);
        {From, {readMessage}} when is_pid(From)->
            From ! {ok_readMessage, popDLQ(DLQ)},
            loop(Datei, VT, HBQ, DLQ);
        {From, {tickVT}} when is_pid(From) ->
            NewVT = vectorC:tickVT(VT),
            From ! {ok_tickVT, NewVT},
            loop(Datei, NewVT, HBQ, DLQ);
        {From, {stop}} when is_pid(From)->
            From ! {ok_stop};
        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, VT, HBQ, DLQ)
	end.

%TODO: implement
pushHBQ(Message, VT) ->
    {}.

%TODO: implement
pushDLQ(Message, VT) -> 
    %TODO sync DLQ VT here
    {}.
popDLQ(DLQ) -> {}.

