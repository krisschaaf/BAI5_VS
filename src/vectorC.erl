-module(vectorC).

-export([initVT/0, myVTid/1, myVTvc/1, myCount/1, foCount/2, isVT/1, syncVT/2, tickVT/1, compVT/2, aftereqVTJ/2, remove_at/2]).

% initVT(): erstellt einen initialen Vektorzeitstempel. Dazu nimmt sie gemäß Spezifikation in towerClock.cfg Kontakt mit der Zentrale towerClock auf, um eine Identität zu erhalten. 
% Rückgabe ist ein initialer Vektorzeitstempel. Dieser stellt die Vektoruhr dar. Achtung: die towerClock.cfg dient nur der ADT vectorC.erl. Der Tower selbst liest diese Datei nicht ein.
initVT() -> 
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("towerClock.cfg"),
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    case net_adm:ping(Servernode) of
        pong -> 
            util:logging(Datei, "TowerClock "++util:to_String(Servername)++" integrated via ping by Server "++util:to_String(Servernode)++"\n");
        pang -> 
            util:logging(Datei, "TowerClock "++util:to_String(Servername)++" not integrated via ping by Server "++util:to_String(Servernode)++" per ping nicht eingebunden\n"),
            throw({error, "Server not reachable"})          
    end,

    {Servername, Servernode} ! {getVecID, self()},
    receive
        {vt, VecID} -> 
            util:logging(Datei, "VecID received: "++util:to_String(VecID)++"\n"),
            {VecID, zeros(VecID)}
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
foCount(J, {_VTID, VT}) when J > 0 -> getElementByIndex(VT, J);
foCount(_, _) -> throw({error, "Invalid index. Index must be greater than 0."}).

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

% syncVT (VT1,VT2): synchronisiert zwei Vektorzeitzstempel (jeweils das Maximum, VT1 wird als eigener Zeitstempel angesehen). 
% Rückgabe ist der neue Vektorzeitstempel.
syncVT({VTID1, VT1}, {_, VT2}) -> 
    NormalizedVT1 = padWithZeros(VT1, length(VT2)),
    NormalizedVT2 = padWithZeros(VT2, length(VT1)),
    syncVT(NormalizedVT1, NormalizedVT2, [], VTID1). 

syncVT([], [], SyncedVT, VTID1) ->
    {VTID1, SyncedVT};
syncVT([H1 | T1], [H2 | T2], SyncedVT, VTID1) ->
    syncVT(T1, T2, SyncedVT ++ [max(H1, H2)], VTID1).

% tickVT(VT): zählt den Ereigniszähler des zugehörigen Prozesses um 1 nach oben. 
% Rückgabe ist der neue Vektorzeitstempel.
tickVT({VTID, VT}) -> {VTID, incrementElementAtIndex(VT, VTID, 1)}.

% compVT(VT1,VT2): vergleicht Vektorzeitstempel. Rückgabe: afterVT, beforeVT, equalVT oder concurrentVT.
compVT({_, VT1}, {_, VT2}) ->
    NormalizedVT1 = padWithZeros(VT1, erlang:length(VT2)),
    NormalizedVT2 = padWithZeros(VT2, erlang:length(VT1)),
    compareLists(NormalizedVT1, NormalizedVT2).

% aftereqVTJ(VT,VTR): vergleicht im Sinne des kausalen Multicast die Vektorzeitstempel, ob VT aftereq VTR ist, ohne Beachtung von Position J. 
% Die Distanz wird berechnet für die Identität J des Vektorzeitstempels VTR, dem Zeitstempel der Nachricht.  
% Rückgabe: {aftereqVTJ, <Distanz an der Stelle J>} oder false. Dabei wird die Distanz berechnet, durch VT[j] - VTR[j], wobei j die Identität von VTR ist. 
% Wenn die Distanz -1 ist, dann kann die Nachricht mit Zeitstempel VTR ausgeliefert werden.
aftereqVTJ(VT, VTR) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    {{Id1, NewVClock1}, {Id2, NewVClock2}, {RemovedElement1, RemovedElement2}} = removeJ(VT, VTR),
    Distance = compVT({Id1, NewVClock1}, {Id2, NewVClock2}),
    case Distance of
        afterVT -> 
            util:logging(Datei, "VT is after VTR with Distance "++util:to_String(RemovedElement1 - RemovedElement2)++".\n"),
            {aftereqVTJ, RemovedElement1 - RemovedElement2};
        equalVT -> 
            util:logging(Datei, "VT is equal VTR with Distance "++util:to_String(RemovedElement1 - RemovedElement2)++".\n"),
            {aftereqVTJ, RemovedElement1 - RemovedElement2};
        _ -> 
            util:logging(Datei, "aftereqVTJ false.\n"),
            false
    end.


% Hilfsfunktionen
zeros(0) ->
    [];
zeros(N) when N < 0 ->
    [];
zeros(N) when N > 0 ->
    [0 | zeros(N - 1)].


getElementByIndex(List, Index) when Index > 0 ->
    getElementByIndex(List, Index, 1).

getElementByIndex([], _Index, _CurrentIndex) ->
    undefined;
getElementByIndex([H | _], Index, Index) ->
    H;
getElementByIndex([_ | T], Index, CurrentIndex) ->
    getElementByIndex(T, Index, CurrentIndex + 1).


isVTListElements([], _Index) ->
    true;
isVTListElements([H | T], Index) when is_integer(H) ->
    isVTListElements(T, Index + 1);
isVTListElements(_, _) ->
    false.


padWithZeros(VT, TargetLength) ->
    VT ++ zeros(TargetLength - length(VT)).


incrementElementAtIndex([], _TargetIndex, _CurrentIndex) ->
    []; 
incrementElementAtIndex([H | T], TargetIndex, CurrentIndex) when TargetIndex == CurrentIndex ->
    [H + 1 | T]; 
incrementElementAtIndex([H | T], TargetIndex, CurrentIndex) ->
    [H | incrementElementAtIndex(T, TargetIndex, CurrentIndex + 1)].


compareLists(VT1, VT2) ->
    compareLists(VT1, VT2, equalVT).

compareLists([], [], Result) ->
    Result;
compareLists([H1 | T1], [H2 | T2], CurrentResult) when H1 > H2 ->
    NewResult = case CurrentResult of
        beforeVT -> concurrentVT;
        _ -> afterVT
    end,
    compareLists(T1, T2, NewResult);
compareLists([H1 | T1], [H2 | T2], CurrentResult) when H1 < H2 ->
    NewResult = case CurrentResult of
        afterVT -> concurrentVT;
        _ -> beforeVT
    end,
    compareLists(T1, T2, NewResult);
compareLists([H1 | T1], [H2 | T2], CurrentResult) when H1 == H2 ->
    compareLists(T1, T2, CurrentResult).


removeJ({VTID1, VT1}, {VTID2, VT2}) ->
    MaxLength = max(length(VT1), length(VT2)),
    NormalizedVT1 = padWithZeros(VT1, MaxLength),
    NormalizedVT2 = padWithZeros(VT2, MaxLength),

    {NewVT1, RemovedElement1} = remove_at(NormalizedVT1, VTID2),
    {NewVT2, RemovedElement2} = remove_at(NormalizedVT2, VTID2),

    {{VTID1, NewVT1}, {VTID2, NewVT2}, {RemovedElement1, RemovedElement2}}.
    

remove_at(List, Index) -> remove_at(List, Index, 1, []).

remove_at([H | T], Index, CurrentIndex, Acc) when CurrentIndex == Index ->
    {Acc ++ T, H};
remove_at([H | T], Index, CurrentIndex, Acc) ->
    remove_at(T, Index, CurrentIndex + 1, Acc ++ [H]).