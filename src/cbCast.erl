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
    exit(whereis(Comm), ok).

% send(Comm,Message): sendet eine Nachricht Message über die Kommunikationseinheit Comm als kausaler Multicast an alle anderen in der Gruppe.
send(Comm, Message) -> 
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerCBC.cfg"),
    {Servername, Servernode} ! {Comm, {multicastNB, {Message, 0}}}. % TODO: ist Comm und MulticastNB hier richtig?

% received(Comm): empfängt blockierend eine Nachricht von der Kommunikationseinheit Comm. Die Rückgabe ist eine Zeichenkette.
received(Comm) -> readMessage(Comm).

% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> spawn(fun() -> readMessage(Comm) end).

readMessage(Comm) ->
    receive
        {From, {castMessage, {Message, _VT}}} when From == Comm ->
            Message
    after 1000 -> null
    end.

castMessage(Message, VT, Datei) -> 
    util:logging(Datei, "Nachricht: "++util:to_String(Message)++" mit Vektorzeitstempel: "++util:to_String(VT)++" empfangen."),
    {}.

% {<PID>,{castMessage,{<Message>,<VT>}}}: als Nachricht. Empfängt die Nachricht <Message> mit Vektorzeitstempel <VT>. <PID> wird nicht benötigt.
% z.B. cbCast ! {self(), {castMessage, {"msg", 12}}}.
loop(Datei) ->
	receive
        {_From, {castMessage, {Message, VT}}} ->
            castMessage(Message, VT, Datei),
            loop(Datei)
	end.