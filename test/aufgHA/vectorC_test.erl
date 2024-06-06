-module(vectorC).
-export([initVT/0, myVTid/1, myVTvc/1, myCount/1, foCount/2, isVT/1, syncVT/2, tickVT/1, compVT/2, aftereqVTJ/2]).
-author("Akif").

%% Funktion initialisiert eine neue Vektorzeituhr, indem beim Tower eine Prozess-Id angefordert
%% und daraus ein neuer Vektorzeitstempel generiert wird.
initVT() ->
    {ok, ConfigList} = file:consult("towerClock.cfg"),
    {ok, Servername} = vsutil:get_config_value(servername, ConfigList),
    {ok, Servernode} = vsutil:get_config_value(servernode, ConfigList),
    case net_adm:ping(Servernode) of
        pang ->
            io:format("Tower auf Node ~p konnte nicht eingebunden werden.\n", [Servernode]),
            exit(not_found);
        pong ->
            io:format("Tower auf Node ~p eingebunden.\n", [Servernode])
    end,
    %---1.3---
    {Servername, Servernode} ! {getVecID, self()},
    receive
        {vt, ProcessId} -> {ProcessId, generateVT(ProcessId)}
    end.

%% Funktion gibt die Prozess-Id des Vektorzeitstempels zurück
myVTid({ProcessId, _Vector}) -> ProcessId.

%% Funktion gibt den Vektor des Vektorzeitstempels zurück
myVTvc({_ProcessId, Vector}) -> Vector.

%% Funktion, um den eigenen Zähler (ProcessId) des Vektorzeitstempels zurückzugeben.
myCount({_ProcessId, []}) -> not_found;
myCount({1, [Head|_Tail]}) -> Head;
myCount({ProcessId, [_Head|Tail]}) ->
    NewProcessId = ProcessId-1,
    myCount({NewProcessId, Tail}).

%% Funktion, um den Zähler für die Position J des Vektorzeitstempels zurückzugeben
foCount(_J, {_ProcessId, []}) -> not_found;
foCount(1, {_ProcessId, [Head|_Tail]}) -> Head;
foCount(J, {ProcessId, [_Head|Tail]}) -> foCount(J-1, {ProcessId, Tail}).

%% Funktion gibt zurück, ob VT ein Vektorzeitstempel der Form {ProcessId, Vector} ist.
isVT(VT) ->
    case VT of
        {_ProcessId, [_Head|_Tail]} -> true;
        _X -> false
    end.

%% Funktion synchronisiert zwei Zeitstempel VT1 und VT2, wobei VT1 der eigene Zeitstempel ist.
syncVT(VT1, VT2) -> syncVT(VT1, VT2, []).
syncVT({ProcessId1, []}, {_ProcessId2, []}, NewVector) -> {ProcessId1, NewVector};
syncVT({ProcessId1, [Head1|Tail1]}, {ProcessId2, [Head2|Tail2]}, NewVector) ->
    % Beide Vektoren haben Elemente, also Vergleich Head1 < Head2 um Maximum zu bilden.
    if
        Head1 < Head2 -> syncVT({ProcessId1, Tail1}, {ProcessId2, Tail2}, NewVector ++ [Head2]);
        true -> syncVT({ProcessId1, Tail1}, {ProcessId2, Tail2}, NewVector ++ [Head1])
    end;
syncVT({ProcessId1, [Head1|Tail1]}, {ProcessId2, []}, NewVector) ->
    % Der zweite Vektor ist kürzer, also ist Head1 automatisch Maximum an dieser Stelle.
    syncVT({ProcessId1, Tail1}, {ProcessId2, []}, NewVector ++ [Head1]);
syncVT({ProcessId1, []}, {ProcessId2, [Head2|Tail2]}, NewVector) ->
    % Der erste Vektor ist kürzer, also ist Head1 automatisch Maximum an dieser Stelle.
    syncVT({ProcessId1, []}, {ProcessId2, Tail2}, NewVector ++ [Head2]).

%% Funktion inkrementiert eigenen Ereigniszähler (ProcessId) um 1.
tickVT({ProcessId, List}) ->
    %---2.4---
    %Neue Liste erstellen mit der ID Stelle + 1
    %Neues Tuple Ausgeben
    NewList = incElemInList(ProcessId, List, []),
    {ProcessId, NewList}.

%% Funktion um zwei Vektoren gemäß ihrer zeitlichen Beziehung zu vergleichen.
compVT({_Id1, Vector1}, {_Id2, Vector2}) ->
    compare_vectors(Vector1, Vector2).

compare_vectors(Vector1, Vector2) -> compare_vectors(Vector1, Vector2, equalVT).
compare_vectors([], [], Result) -> Result;
compare_vectors([H1|T1], [], Result) -> compare_vectors([H1|T1], [0], Result);
compare_vectors([], [H2|T2], Result) -> compare_vectors([0], [H2|T2], Result);
compare_vectors([H1|T1], [H2|T2], Result) ->
    NewResult = compare_elements(H1,H2),
    if
        NewResult == equalVT -> compare_vectors(T1,T2, Result);
        NewResult == Result ; Result == equalVT -> compare_vectors(T1, T2, NewResult);
        true -> concurrentVT
    end.

compare_elements(X, X) -> equalVT;
compare_elements(X, Y) when X > Y -> afterVT;
compare_elements(X, Y) when X < Y -> beforeVT.

%% Funktion um zwei Vektoren gemäß den Kriterien des kausalen Multicast zu vergleichen
%---3.5---
aftereqVTJ(VT, VTR) -> aftereqVTJ(VT, VTR, 0).
aftereqVTJ({_VTId, []}, {_VTRId, []}, Distance) -> {aftereqVTJ, Distance};
aftereqVTJ({VTId, [Head1|Tail1]}, {VTRId, []}, Distance) -> aftereqVTJ({VTId, [Head1|Tail1]}, {VTRId, [0]}, Distance);
aftereqVTJ({VTId, []}, {VTRId, [Head2|Tail2]}, Distance) -> aftereqVTJ({VTId, [0]}, {VTRId, [Head2|Tail2]}, Distance);
aftereqVTJ({VTid, [Head1|Tail1]}, {1, [Head2|Tail2]}, _Distance) ->
    aftereqVTJ({VTid, Tail1}, {0, Tail2}, Head1-Head2);
aftereqVTJ({VTId, [Head1|Tail1]}, {VTRId, [Head2|Tail2]}, Distance) ->
    if
        Head1 >= Head2 -> aftereqVTJ({VTId, Tail1}, {VTRId-1, Tail2}, Distance);
        true -> false
    end.

%% Hilfsfunktion, um Zähler an der gewünschten Stelle (Id) zu inkrementieren
incElemInList(1, [Head|Tail], NewList) -> NewList++[Head+1]++Tail;
incElemInList(Id, [Head|Tail], NewList) ->
    incElemInList(Id-1, Tail, NewList++[Head]).

%% Hilfsfunktion, um einen neuen Zeitstempel gemäß der gegebenen ProcessId zu erstellen.
generateVT(0) -> [];
generateVT(ProcessId) -> [0 | generateVT(ProcessId - 1)].


% Verworfene compVT-Funktion
% compVT({_Id1, List1}, {_Id2, List2}) ->
%     compare_vectors(List1, List2).

% %L1=[1, 0, 2], L2=[1, 1]
% compare_vectors(List1, List2) -> compare_vectors(List1, List2, equalVT).
% compare_vectors([], [], Result) -> Result;
% compare_vectors([H1|T1], [H2|T2], _Result) ->
%     NewResult = compare_elements(H1,H2),
%     if
%         NewResult == equalVT -> checkAllEq(T1, T2, NewResult); %Call compare_vectors
%         true ->
%             afterOrBefore(T1, T2, NewResult)
%     end.

% %L1=[2], L2=[] afterVT
% afterOrBefore([], [], Result) -> Result;
% afterOrBefore([], [Head|Tail], Result) ->
%     overSizedList([],[Head|Tail], Result, aob);
% afterOrBefore([Head|Tail], [], Result) ->
%     overSizedList([Head|Tail],[], Result, aob);
% afterOrBefore([H1|T1], [H2|T2], Result) ->
%     NewResult = compare_elements(H1,H2),
%     if
%         NewResult == Result -> afterOrBefore(T1,T2, NewResult);
%         NewResult == equalVT -> afterOrBefore(T1,T2,Result);
%         true ->
%             compare_vectors([], [], concurrentVT)
%     end.

% %L1=[0, 2], L2=[1]
% checkAllEq([], [], Result) -> Result;
% checkAllEq([], [Head|Tail], Result) ->
%     overSizedList([],[Head|Tail], Result, equ);
% checkAllEq([Head|Tail], [], Result) ->
%     overSizedList([Head|Tail],[], Result, equ);
% checkAllEq([H1|T1], [H2|T2], Result) ->
%     NewResult = compare_elements(H1,H2),
%     if
%         NewResult == equalVT -> checkAllEq(T1,T2,Result);
%         true ->
%             afterOrBefore(T1,T2, NewResult)
%     end.

% %L1=[2], L2=[] beforeVT
% overSizedList([Head|Tail], [], Result, Flag) ->
%     if
%         Head == 0 ->
%             if
%                 Flag == aob ->
%                     afterOrBefore(Tail, [], Result);
%                 true ->
%                      checkAllEq(Tail, [], Result)
%             end;
%         true ->
%             NewResult = compare_elements(Head, 0),
%                 if
%                     Flag == aob ->
%                         if
%                             Result == beforeVT -> concurrentVT;
%                             true ->
%                                 afterOrBefore(Tail, [], NewResult)
%                         end;
%                     true ->
%                         checkAllEq(Tail, [], NewResult)
%                 end
%     end;
% overSizedList([],[Head|Tail], Result, Flag) ->
%     if
%         Head == 0 ->
%             if
%                 Flag == aob ->
%                     afterOrBefore([], Tail, Result);
%                 true ->
%                     checkAllEq([], Tail, Result)
%             end;
%         true ->
%             NewResult = compare_elements(Head,0),
%             if
%                 Flag == aob ->
%                     if
%                             Result == afterVT -> concurrentVT;
%                             true ->
%                                 afterOrBefore([],Tail, NewResult)
%                     end;
%                 true ->
%                     checkAllEq([], Tail, NewResult)
%             end
%     end.

