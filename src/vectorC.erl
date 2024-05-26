-module(vectorC).

-export([initVT/0, myVTid/1, myVTvc/1, myCount/1, foCount/2, isVT/1, syncVT/2, tickVT/1, compVT/2, aftereqVTJ/2]).

% initVT(): erstellt einen initialen Vektorzeitstempel. Dazu nimmt sie gemäß Spezifikation in towerClock.cfg Kontakt mit der Zentrale towerClock auf, um eine Identität zu erhalten. 
% Rückgabe ist ein initialer Vektorzeitstempel. Dieser stellt die Vektoruhr dar. Achtung: die towerClock.cfg dient nur der ADT vectorC.erl. Der Tower selbst liest diese Datei nicht ein.
initVT() -> 
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerClock.cfg"),
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    case net_adm:ping(Servernode) of
        pong -> 
            util:logging(Datei, "TowerClock "++util:to_String(Servername)++" von Server "++util:to_String(Servernode)++" per ping eingebunden\n");
        pang -> 
            util:logging(Datei, "TowerClock "++util:to_String(Servername)++" von Server "++util:to_String(Servernode)++" per ping nicht eingebunden\n"),
            throw({error, "Server nicht erreichbar"})          
    end,

    {Servername, Servernode} ! {getVecID, self()},
    receive
        {vt, VecID} -> 
            util:logging(Datei, "VecID received: "++util:to_String(VecID)++"\n"),
            VecID
        after 1000 -> 
            util:logging(Datei, "Timeout: no VecID received.\n"),
            false
    end.

% myVTid(VT): gibt die ProzessID zurück, also <Pnum> des Vektorzeitstempels, als ganze Zahl.
myVTid({VTID, _VT}) -> VTID.
    
% myVTvc(VT): gibt den Vektor zurück, also den Vektorzeitstempel, als Liste aus ganzen Zahlen inklusive 0.
myVTvc({_VTID, VT}) -> VT.

% myCount(VT): gibt den eigenen Zeitstempel als ganze Zahl zurück bzw. den zugehörigen Ereigniszähler, .
myCount({VTID, VT}) -> getElementByIndex(VT, VTID).

% foCount(J,VT): gibt den Zeitstempel der Position J als ganze Zahl zurück bzw. den zugehörigen Ereigniszähler.
foCount(J, {_VTID, VT}) -> getElementByIndex(VT, J).

getElementByIndex(List, Index) when is_list(List), is_integer(Index), Index >= 0 ->
    getElementByIndex(List, Index, 0).

getElementByIndex([], _Index, _CurrentIndex) ->
    undefined;
getElementByIndex([H | _], Index, Index) ->
    H;
getElementByIndex([_ | T], Index, CurrentIndex) ->
    getElementByIndex(T, Index, CurrentIndex + 1).

% isVT(VT): prüft, ob VT ein Vektorzeitstempel ist. Rückgabe ist true oder false.
isVT(VT) -> 
    case VT of
        {VTID, VTList} -> 
            case is_integer(VTID) and is_list(VTList) and isVTListElements(VTList, 0) of
                true -> true;
                false -> false
            end;
        _ -> false
    end.

isVTListElements([], _Index) ->
    true;
isVTListElements([H | T], Index) when is_integer(H) ->
    isVTListElements(T, Index + 1);
isVTListElements(_, _) ->
    false.

% syncVT (VT1,VT2): synchronisiert zwei Vektorzeitzstempel (jeweils das Maximum, VT1 wird als eigener Zeitstempel angesehen). 
% Rückgabe ist der neue Vektorzeitstempel.
syncVT({_, VT1}, {_, VT2}) -> syncVT(VT1, VT2, []).

syncVT([], [], SyncedVT) ->
    SyncedVT;
syncVT([H1 | T1], [H2 | T2], SyncedVT) ->
    syncVT(T1, T2, SyncedVT ++ [max(H1, H2)]).

% tickVT(VT): zählt den Ereigniszähler des zugehörigen Prozesses um 1 nach oben. 
% Rückgabe ist der neue Vektorzeitstempel.
tickVT({VTID, VT}) -> {VTID, incrementElementAtIndex(VT, VTID, 0)}.

incrementElementAtIndex([], _TargetIndex, _CurrentIndex) ->
    []; 
incrementElementAtIndex([H | T], TargetIndex, CurrentIndex) when TargetIndex == CurrentIndex ->
    [H + 1 | T]; 
incrementElementAtIndex([H | T], TargetIndex, CurrentIndex) ->
    [H | incrementElementAtIndex(T, TargetIndex, CurrentIndex + 1)].


% compVT(VT1,VT2): vergleicht Vektorzeitstempel. Rückgabe: afterVT, beforeVT, equalVT oder concurrentVT.
compVT({_, VT1}, {_, VT2}) -> 
    case listsHaveSameElements(VT1, VT2) of
        true -> equalVT;
        false -> concurrentVT % TODO: fix
    end.

listsHaveSameElements([], []) ->
    true;
listsHaveSameElements([H1 | T1], [H2 | T2]) when H1 == H2 ->
    listsHaveSameElements(T1, T2);
listsHaveSameElements(_, _) ->
    false.

% aftereqVTJ(VT,VTR): vergleicht im Sinne des kausalen Multicast die Vektorzeitstempel, ob VT aftereq VTR ist, ohne Beachtung von Position J. 
% Die Distanz wird berechnet für die Identität J des Vektorzeitstempels VTR, dem Zeitstempel der Nachricht.  
% Rückgabe: {aftereqVTJ, <Distanz an der Stelle J>} oder false. Dabei wird die Distanz berechnet, durch VT[j] - VTR[j], wobei j die Identität von VTR ist. 
% Wenn die Distanz -1 ist, dann kann die Nachricht mit Zeitstempel VTR ausgeliefert werden.
aftereqVTJ(VT, VTR) -> {}.