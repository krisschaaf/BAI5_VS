-module(cbCast).

-export([init/0, stop/1, send/2, received/1, read/1]).

% init(): erstellt einen Prozess für die Kommunikationseinheit. Rückgabe ist ihre PID.
init() -> {}.

% stop(Comm): terminiert die Kommunikationseinheit mit PID Comm. Rückgabe ist done.
stop(Comm) -> done. 

% send(Comm,Message): sendet eine Nachricht Message über die Kommunikationseinheit Comm als kausaler Multicast an alle anderen in der Gruppe.
send(Comm, Message) -> {}.

% received(Comm): empfängt blockierend eine Nachricht von der Kommunikationseinheit Comm. Die Rückgabe ist eine Zeichenkette.
received(Comm) -> "".

% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> "".

% {<PID>,{castMessage,{<Message>,<VT>}}}: als Nachricht. Empfängt die Nachricht <Message> mit Vektorzeitstempel <VT>. <PID> wird nicht benötigt.