-module(cbCast).

-export([init/0, stop/1, send/2, received/1, read/1]).

% init(): erstellt einen Prozess für die Kommunikationseinheit. Rückgabe ist ihre PID.
init() ->
	{ok, HostName} = inet:gethostname(),
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerCBC.cfg"),
    Datei = "logs/cbCast@"++HostName++".log",
    CommCBC = spawn(fun() -> loop(Datei, register, Servername, Servernode) end),
    register(cbCast,CommCBC),
    CommCBC.

% stop(Comm): terminiert die Kommunikationseinheit mit PID Comm. Rückgabe ist done.
stop(Comm) -> 
    exit(whereis(Comm), ok).

% send(Comm,Message): sendet eine Nachricht Message über die Kommunikationseinheit Comm als kausaler Multicast an alle anderen in der Gruppe.
send(Comm, Message) -> {}.

% received(Comm): empfängt blockierend eine Nachricht von der Kommunikationseinheit Comm. Die Rückgabe ist eine Zeichenkette.
received(Comm) -> "".

% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> "".

castMessage(Message, VT, Datei) -> 
    util:logging(Datei, "Nachricht: "++util:to_String(Message)++" mit Vektorzeitstempel: "++util:to_String(VT)++" empfangen."),
    {}.

loop(Datei, register, Servername, Servernode) ->
    {Servername, Servernode} ! {self(), {register, self()}},
    loop(Datei).

% {<PID>,{castMessage,{<Message>,<VT>}}}: als Nachricht. Empfängt die Nachricht <Message> mit Vektorzeitstempel <VT>. <PID> wird nicht benötigt.
% z.B. cbCast ! {self(), {castMessage, {"msg", 12}}}.
loop(Datei) ->
	receive
        {_From, {castMessage, {Message, VT}}} ->
            castMessage(Message, VT, Datei),
            loop(Datei)
	end.