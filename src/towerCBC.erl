-module(towerCBC).

-export([init/1, stop/1, reset/1, listall/0, cbcast/2]).

% init()|init(auto|manu): startet den Multicast im automatischen Modus (init()|init(auto)) oder dem manuellen Modus (init(manu)). 
% Rückgabewert ist die PID des Multicast.
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
init(Configuration) -> 
    {ok, Hostname} = inet:gethostname(),
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerCBC.cfg"),
    Datei = "logs/"++util:to_String(Servername)++"@"++Hostname++".log",

    case Configuration of
        manu -> manu(Servername, Servernode, Datei);
        auto -> auto(Servername, Servernode, Datei)
    end.

manu(Servername, Servernode, Datei) ->     
    TowerCBC = spawn(Servernode, fun() -> loop(Datei, [], manu, Servernode) end),
    register(Servername,TowerCBC),
    TowerCBC.

auto(Servername, Servernode, Datei) -> {}.

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
    towerCBC ! {self(), {listall}},
    true.

% cbcast(<Receiver>,<MessageNumber>): muss auf der Node des Multicast ausgeführt werden und sendet die als <MessageNumber>-te beim Multicast eingetroffene Nachricht an den Empfänger <Receiver>. 
% Dient im manuellen Zustand der manuellen Durchführuzng des Multicast. 
% Rückgabewert ist bei Erfolg true, sonst false. 
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
cbcast(Receiver, MessageNumber) -> {}.

loop(Datei, Registered, Configuration, Servernode) ->
    receive
        % {<PID>,{register,<RPID>}}: als Nachricht. Registriert die Kommunikationseinheit <RPID> beim Multicast. 
        % <PID> erhält als Antwort {replycbc,ok_existing} wenn er bereits registriert wurde oder {replycbc,ok_registered} wenn er neu registriert wurde. 
        % Der Multicast wird an <RPID> gesendet.
        {_From, {register, RPID}} ->
            case lists:member(RPID, Registered) of
                true ->
                    util:logging(Datei, "existing "++util:to_String(RPID)++"\n"),
                    RPID ! {replycbc, ok_existing},
                    NewRegistered = Registered;
                false ->
                    util:logging(Datei, "registered "++util:to_String(RPID)++"\n"),
                    NewRegistered = Registered++[RPID],
                    RPID ! {replycbc, ok_registered}
            end,
            loop(Datei, NewRegistered, Configuration, Servernode);

        % {<PID>,{multicastB,{<Message>,<VT>}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, blockierenden Multicast an alle Gruppenmitglieder. 
        % Blockierend bedeutet, dass in der Zeit kein anderer Multicast versendet wird. <PID> ist eine PID und wird lediglich im Log vermerkt.
        {_From, {multicastB, {Message, VT}}} ->      
            util:logging(Datei, "multicastB: "++util:to_String(Message)++" with VT: "++util:to_String(VT)++"\n"),
            sendToAll(Datei, Registered, Message, VT),
            loop(Datei, Registered, Configuration, Servernode);

        % {<PID>,{multicastNB,{<Message>,<VT>}}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, nicht blockierenden Multicast an alle Gruppenmitglieder. 
        % Nicht blockierend bedeutet, dass in der Zeit andere Multicast versendet werden können. <PID> ist eine PID und wird lediglich im Log vermerkt.
        {_From, {multicastNB, {Message, VT}}} ->
            util:logging(Datei, "multicastNB: "++util:to_String(Message)++" with VT: "++util:to_String(VT)++"\n"),
            spawn(Servernode, fun() -> sendToAll(Datei, Registered, Message, VT) end),
            loop(Datei, Registered, Configuration, Servernode);

        % {<PID>,{reset}}: als Nachricht. Setzt den Multicast wieder in den initialen Zustand.
        % <PID> erhält als Antwort {replycbc,ok_reset}.
        {From, {reset}} ->
            From ! {replycbc, ok_reset},
            loop(Datei, [], Configuration, Servernode);

        % {<PID>,{listall}}: als Nachricht. Listet alle registrierten Kommunikationseinheiten in der zugehörigen Log-Datei auf.
        % <PID> erhält als Antwort {replycbc,ok_listall}.
        {From, {listall}} ->
            util:logging(Datei, "Registered Comm Modules:\n"),
            logAll(Datei, Registered),
            From ! {replycbc, ok_listall},
            loop(Datei, Registered, Configuration, Servernode)
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