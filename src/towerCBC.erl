-module(towerCBC).

-export([init/1, stop/1, reset/1, listall/0, cbcast/2]).

% init()|init(auto|manu): startet den Multicast im automatischen Modus (init()|init(auto)) oder dem manuellen Modus (init(manu)). 
% Rückgabewert ist die PID des Multicast.
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
init(Config) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    case Config of
        auto -> TowerCBC = spawn(fun() -> loop(Datei, [], true) end);
        manu -> TowerCBC = spawn(fun() -> loop(Datei, [], false) end)
    end,
    register(towerKLCcbc, TowerCBC),
    util:logging(Datei, "TowerCBC initialized with PID: "++util:to_String(TowerCBC)++" on Node "++util:to_String(erlang:node())++"\n"),
    TowerCBC.

% stop(<PID>): wobei <PID> die Kontaktadresse des Multicast ist. Diese Funktion beendet den Multicast. 
% Rückgabewert ist bei Erfolg true.
stop(PID) -> 
    exit(whereis(PID), ok).

% reset(<PID>): wobei <PID> die Kontaktadresse des Multicast ist. Diese Funktion setzt den Multicast wieder in den initialen Zustand. 
% Rückgabewert ist bei Erfolg true.
reset(PID) -> 
    PID ! {self(), {reset}},
    true.

% listall(): muss auf der Node des Multicast ausgeführt werden und listet alle registrierten Kommunikationseinheiten in der zugehörigen Log-Datei auf. 
% Rückgabewert ist bei Erfolg true, sonst false.
listall() -> 
    towerKLCcbc ! {self(), {listall}},
    true.

% cbcast(<Receiver>,<MessageNumber>): muss auf der Node des Multicast ausgeführt werden und sendet die als <MessageNumber>-te beim Multicast eingetroffene Nachricht an den Empfänger <Receiver>. 
% Dient im manuellen Zustand der manuellen Durchführuzng des Multicast. 
% Rückgabewert ist bei Erfolg true, sonst false. 
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
cbcast(Receiver, MessageNumber) -> {}.

loop(Datei, Registered, Auto) ->
    receive
        % {<PID>,{register,<RPID>}}: als Nachricht. Registriert die Kommunikationseinheit <RPID> beim Multicast. 
        % <PID> erhält als Antwort {replycbc,ok_existing} wenn er bereits registriert wurde oder {replycbc,ok_registered} wenn er neu registriert wurde. 
        % Der Multicast wird an <RPID> gesendet.
        {From, {register, RPID}} when is_pid(From) and is_pid(RPID) ->
            case lists:member(RPID, Registered) of
                true ->
                    util:logging(Datei, "existing "++util:to_String(RPID)++"\n"),
                    From ! {replycbc, ok_existing},
                    NewRegistered = Registered;
                false ->
                    util:logging(Datei, "registered "++util:to_String(RPID)++"\n"),
                    NewRegistered = Registered++[RPID],
                    From ! {replycbc, ok_registered}
            end,
            loop(Datei, NewRegistered, Auto);

        % {<PID>,{multicastB,{<Message>,<VT>}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, blockierenden Multicast an alle Gruppenmitglieder. 
        % Blockierend bedeutet, dass in der Zeit kein anderer Multicast versendet wird. <PID> ist eine PID und wird lediglich im Log vermerkt.
        {From, {multicastB, {Message, VT}}} when is_pid(From) and is_tuple(Message) and Auto ->      
            util:logging(Datei, "multicastB: "++util:to_String(Message)++" with VT: "++util:to_String(VT)++" by: "++util:to_String(From)++"\n"),
            sendToAll(Datei, Registered, Message, VT),
            loop(Datei, Registered, Auto);

        % {<PID>,{multicastNB,{<Message>,<VT>}}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, nicht blockierenden Multicast an alle Gruppenmitglieder. 
        % Nicht blockierend bedeutet, dass in der Zeit andere Multicast versendet werden können. <PID> ist eine PID und wird lediglich im Log vermerkt.
        {From, {multicastNB, {Message, VT}}} when is_pid(From) and is_tuple(Message) and not(Auto) ->
            util:logging(Datei, "multicastNB: "++util:to_String(Message)++" with VT: "++util:to_String(VT)++" by: "++util:to_String(From)++"\n"),
            spawn(fun() -> sendToAll(Datei, Registered, Message, VT) end),
            loop(Datei, Registered, Auto);

        % {<PID>,{multicastM,<CommNR>,<MessageNR>}}: als Nachricht. Sendet die vorhandene Nachricht <MessageNR> an die registrierte Kommunikationseinheit  <CommNR>. 
        % Dies geht nur im Zustand manu.
        {From, {multicastM, CommNR, MessageNR}} when is_pid(From) and not(Auto) -> 
            CommNR ! {self(), {castMessage, {MessageNR, 0}}}, % TODO: get correct VT
            loop(Datei, Registered, Auto);

        % {<PID>,{reset}}: als Nachricht. Setzt den Multicast wieder in den initialen Zustand.
        % <PID> erhält als Antwort {replycbc,ok_reset}.
        {From, {reset}} ->
            From ! {replycbc, ok_reset},
            loop(Datei, [], Auto);

        % {<PID>,{listall}}: als Nachricht. Listet alle registrierten Kommunikationseinheiten in der zugehörigen Log-Datei auf.
        % <PID> erhält als Antwort {replycbc,ok_listall}.
        {From, {listall}} ->
            util:logging(Datei, "Registered Comm Modules:\n"),
            logAll(Datei, Registered),
            From ! {replycbc, ok_listall},
            loop(Datei, Registered, Auto)
    end.


sendToAll(_Datei, [], _Message, _VT) ->
    ok;
sendToAll(Datei, [Head | Tail], Message, VT) ->
    Head ! {self(), {castMessage, {Message, VT}}},
    util:logging(Datei, "send to "++util:to_String(Head)++"\n"),
    sendToAll(Datei, Tail, Message, VT).

logAll(_Datei, []) ->
    ok;
logAll(Datei, [Head | Tail]) ->
    util:logging(Datei, util:to_String(Head)++"\n"),
    logAll(Datei, Tail).