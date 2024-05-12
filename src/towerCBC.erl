-module(towerCBC).

-export([init/0, init/1, stop/1, reset/1, listall/0, cbcast/2]).

% init()|init(auto|manu): startet den Multicast im automatischen Modus (init()|init(auto)) oder dem manuellen Modus (init(manu)). 
% Rückgabewert ist die PID des Multicast.
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
init() -> {}.
init(manu) -> {}.

% stop(<PID>): wobei <PID> die Kontaktadresse des Multicast ist. Diese Funktion beendet den Multicast. 
% Rückgabewert ist bei Erfolg true.
stop(PID) -> {}.

% reset(<PID>): wobei <PID> die Kontaktadresse des Multicast ist. Diese Funktion setzt den Multicast wieder in den initialen Zustand. 
% Rückgabewert ist bei Erfolg true.
reset(PID) -> {}.

% listall(): muss auf der Node des Multicast ausgeführt werden und listet alle registrierten Kommunikationseinheiten in der zugehörigen Log-Datei auf. 
% Rückgabewert ist bei Erfolg true, sonst false.
listall() -> {}.

% cbcast(<Receiver>,<MessageNumber>): muss auf der Node des Multicast ausgeführt werden und sendet die als <MessageNumber>-te beim Multicast eingetroffene Nachricht an den Empfänger <Receiver>. 
% Dient im manuellen Zustand der manuellen Durchführuzng des Multicast. 
% Rückgabewert ist bei Erfolg true, sonst false. 
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
cbcast(Receiver, MessageNumber) -> {}.

% {<PID>,{register,<RPID>}}: als Nachricht. Registriert die Kommunikationseinheit <RPID> beim Multicast. 
% <PID> erhält als Antwort {replycbc,ok_existing} wenn er bereits registriert wurde oder {replycbc,ok_registered} wenn er neu registriert wurde. 
% Der Multicast wird an <RPID> gesendet.


% {<PID>,{multicastB,{<Message>,<VT>}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, blockierenden Multicast an alle Gruppenmitglieder. 
% Blockierend bedeutet, dass in der Zeit kein anderer Multicast versendet wird. <PID> ist eine PID und wird lediglich im Log vermerkt.


% {<PID>,{multicastNB,{<Message>,<VT>}}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, nicht blockierenden Multicast an alle Gruppenmitglieder. 
% Nicht blockierend bedeutet, dass in der Zeit andere Multicast versendet werden können. <PID> ist eine PID und wird lediglich im Log vermerkt.