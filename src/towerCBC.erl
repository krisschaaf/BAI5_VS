-module(towerCBC).

-export([init/1, stop/1, reset/1, listall/0, cbcast/2]).

% init()|init(auto|manu): startet den Multicast im automatischen Modus (init()|init(auto)) oder dem manuellen Modus (init(manu)). 
% Rückgabewert ist die PID des Multicast.
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
init(Config) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    case Config of
        auto -> TowerCBC = spawn(fun() -> loop(Datei, [], true, []) end);
        manu -> TowerCBC = spawn(fun() -> loop(Datei, [], false, []) end);
        _ -> 
            throw({error, "Invalid mode"}),
            TowerCBC = null
    end,
    register(towerKLCcbc, TowerCBC),
    util:logging(Datei, "TowerCBC initialized with Mode: "++util:to_String(Config)++" and PID: "++util:to_String(TowerCBC)++" on Node "++util:to_String(erlang:node())++"\n"),
    TowerCBC.

% stop(<PID>): wobei <PID> die Kontaktadresse des Multicast ist. Diese Funktion beendet den Multicast. 
% Rückgabewert ist bei Erfolg true.
stop(PID) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    PID ! {self(), {stop}},
    receive
        {ok_stop} -> 
            true
        after 5000 ->
            util:logging(Datei, "Timeout: TowerCBC not stopped, killing now...\n"),
            exit(whereis(PID), ok)
    end.

% reset(<PID>): wobei <PID> die Kontaktadresse des Multicast ist. Diese Funktion setzt den Multicast wieder in den initialen Zustand. 
% Rückgabewert ist bei Erfolg true.
reset(PID) -> 
    PID ! {self(), {reset}},
    receive
        {replycbc, ok_reset} -> true 
    after 1000 -> false 
    end.

% listall(): muss auf der Node des Multicast ausgeführt werden und listet alle registrierten Kommunikationseinheiten in der zugehörigen Log-Datei auf. 
% Rückgabewert ist bei Erfolg true, sonst false.
listall() -> 
    towerKLCcbc ! {self(), {listall}},
    receive
        {replycbc, ok_listall} -> true
    after 1000 -> false
    end.

% cbcast(<Receiver>,<MessageNumber>): muss auf der Node des Multicast ausgeführt werden und sendet die als <MessageNumber>-te beim Multicast eingetroffene Nachricht an den Empfänger <Receiver>. 
% Dient im manuellen Zustand der manuellen Durchführuzng des Multicast. 
% Rückgabewert ist bei Erfolg true, sonst false. 
% Achtung: um den manuellen Modus nutzen zu können, muss die Kommunikationseinheit cbCast.erl mit multicastNB senden!
cbcast(Receiver, MessageNumber) when is_integer(Receiver), is_integer(MessageNumber), Receiver > 0, MessageNumber > 0 -> 
    towerKLCcbc ! {self(), {multicastM, Receiver, MessageNumber}},
    receive
        {replycbc, ok_send} -> true;
        {replycbc, error_send} -> false
    after 1000 -> false
    end.

loop(Datei, Registered, Auto, Buffer) ->
    receive
        % {<PID>,{register,<RPID>}}: als Nachricht. Registriert die Kommunikationseinheit <RPID> beim Multicast. 
        % <PID> erhält als Antwort {replycbc,ok_existing} wenn er bereits registriert wurde oder {replycbc,ok_registered} wenn er neu registriert wurde. 
        % Der Multicast wird an <RPID> gesendet.
        {From, {register, RPID}} when is_pid(From) and is_pid(RPID) ->
            case isListMember(RPID, Registered) of
                true ->
                    util:logging(Datei, "existing "++util:to_String(RPID)++"\n"),
                    From ! {replycbc, ok_existing},
                    NewRegistered = Registered;
                false ->
                    util:logging(Datei, "registered "++util:to_String(RPID)++"\n"),
                    NewRegistered = Registered++[RPID],
                    From ! {replycbc, ok_registered}
            end,
            loop(Datei, NewRegistered, Auto, Buffer);

        % {<PID>,{multicastB,{<Message>,<VT>}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, blockierenden Multicast an alle Gruppenmitglieder. 
        % Blockierend bedeutet, dass in der Zeit kein anderer Multicast versendet wird. <PID> ist eine PID und wird lediglich im Log vermerkt.
        {From, {multicastB, {Message, VT}}} when is_pid(From) ->
            case isListMember(From, Registered) of
                false ->
                    util:logging(Datei, "Not registered " ++ util:to_String(From) ++ "\n"),
                    loop(Datei, Registered, Auto, Buffer);
                true ->
                    case vectorC:isVT(VT) of
                        false ->
                            util:logging(Datei, "Invalid VT in multicastB: "++util:to_String(VT)++"\n"),
                            loop(Datei, Registered, Auto, Buffer);
                        true ->
                            util:logging(Datei, "multicastB: "++util:to_String(Message)++" with VT: "++util:to_String(VT)++" by: "++util:to_String(From)++"\n"),
                            sendToAll(Datei, Registered, {Message, VT}),
                            loop(Datei, Registered, Auto, Buffer ++ [{Message, VT}])
                    end
            end;

        % {<PID>,{multicastNB,{<Message>,<VT>}}}: als Nachricht. Sendet die Nachricht <Message> mit Vektorzeitstempel <VT> als ungeordneten, nicht blockierenden Multicast an alle Gruppenmitglieder. 
        % Nicht blockierend bedeutet, dass in der Zeit andere Multicast versendet werden können. <PID> ist eine PID und wird lediglich im Log vermerkt.
        {From, {multicastNB, {_, _}}} when is_pid(From) and Auto->
            util:logging(Datei, "MulticastNB not allowed in auto mode\n"),
            loop(Datei, Registered, Auto, Buffer);
        {From, {multicastNB, {Message, VT}}} when is_pid(From) and not(Auto) ->
            case isListMember(From, Registered) of
                false ->
                    util:logging(Datei, "Not registered " ++ util:to_String(From) ++ "\n"),
                    loop(Datei, Registered, Auto, Buffer);
                true ->
                    case vectorC:isVT(VT) of
                        false ->
                            util:logging(Datei, "Invalid VT in multicastNB: " ++ util:to_String(VT) ++ "\n"),
                            loop(Datei, Registered, Auto, Buffer);
                        true ->
                            util:logging(Datei, "multicastNB: " ++ util:to_String(Message)++ " with VT: " ++ util:to_String(VT)++" by: " ++ util:to_String(From) ++ "\n"),
                            loop(Datei, Registered, Auto, Buffer ++ [{Message, VT}])
                    end
            end;

        % {<PID>,{multicastM,<CommNR>,<MessageNR>}}: als Nachricht. Sendet die vorhandene Nachricht <MessageNR> an die registrierte Kommunikationseinheit  <CommNR>. 
        % Dies geht nur im Zustand manu.
        {From, {multicastM, _, _}} when Auto-> 
            util:logging(Datei, "MulticastM not allowed in auto mode\n"),
            From ! {replycbc, error_send},
            loop(Datei, Registered, Auto, Buffer);
        {From, {multicastM, CommNR, MessageNR}} when is_pid(From), not(Auto), is_integer(CommNR), is_integer(MessageNR), CommNR>0, MessageNR>0, CommNR =< length(Registered), MessageNR =< length(Buffer) -> 
            Comm = getElementByIndex(Registered, CommNR - 1), % Receiver Index beginnt bei 0
            Result = getElementByIndex(Buffer, MessageNR - 1), % Buffer Index beginnt bei 0
            util:logging(Datei, util:to_String(Result)++"\n"),
            case Result of
                {Message, VT} when is_pid(Comm) -> 
                    Comm ! {self(), {castMessage, {Message, VT}}},
                    util:logging(Datei, "Send "++util:to_String(Message)++" to "++util:to_String(CommNR)++"\n"),
                    From ! {replycbc, ok_send};
                _ -> 
                    From ! {replycbc, error_send}
            end,
            loop(Datei, Registered, Auto, Buffer);
        {From, {multicastM, CommNR, MessageNR}} ->
            util:logging(Datei, "MulticastM: "++util:to_String(CommNR)++" with MessageNR: "++util:to_String(MessageNR)++" by: "++util:to_String(From)++"...not possible\n"),
            From ! {replycbc, error_send},
            loop(Datei, Registered, Auto, Buffer);

        % {<PID>,{reset}}: als Nachricht. Setzt den Multicast wieder in den initialen Zustand.
        % <PID> erhält als Antwort {replycbc,ok_reset}.
        {From, {reset}} ->
            From ! {replycbc, ok_reset},
            loop(Datei, [], Auto, []);

        % {<PID>,{listall}}: als Nachricht. Listet alle registrierten Kommunikationseinheiten in der zugehörigen Log-Datei auf.
        % <PID> erhält als Antwort {replycbc,ok_listall}.
        {From, {listall}} ->
            util:logging(Datei, "Registered Comm Modules:\n"),
            logAll(Datei, Registered),
            From ! {replycbc, ok_listall},
            loop(Datei, Registered, Auto, Buffer);
        
        {From, {stop}} when is_pid(From)->
            unregister(towerKLCcbc),
            From ! {ok_stop};

        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, Registered, Auto, Buffer)
    end.


isListMember(_, []) ->
    false;
isListMember(Element, [Element | _]) ->
    true;
isListMember(Element, [_ | Tail]) ->
    isListMember(Element, Tail).

sendToAll(_Datei, [], _Message) ->
    ok;
sendToAll(Datei, [Head | Tail], Message) ->
    Head ! {self(), {castMessage, Message}},
    util:logging(Datei, "send to "++util:to_String(Head)++"\n"),
    sendToAll(Datei, Tail, Message).

logAll(_Datei, []) ->
    ok;
logAll(Datei, [Head | Tail]) ->
    util:logging(Datei, util:to_String(Head)++"\n"),
    logAll(Datei, Tail).

getElementByIndex(List, Index) when Index >= 0 ->
    getElementByIndex(List, Index, 0).

getElementByIndex([], _Index, _CurrentIndex) ->
    undefined;
getElementByIndex([H | _], Index, Index) ->
    H;
getElementByIndex([_ | T], Index, CurrentIndex) ->
    getElementByIndex(T, Index, CurrentIndex + 1).