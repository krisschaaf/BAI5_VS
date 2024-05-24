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
myVTid(VT) -> {}.

% myVTvc(VT): gibt den Vektor zurück, also den Vektorzeitstempel, als Liste aus ganzen Zahlen inklusive 0.
myVTvc(VT) -> {}.

% myCount(VT): gibt den eigenen Zeitstempel als ganze Zahl zurück bzw. den zugehörigen Ereigniszähler, .
myCount(VT) -> {}.

% foCount(J,VT): gibt den Zeitstempel der Position J als ganze Zahl zurück bzw. den zugehörigen Ereigniszähler.
foCount(J, VT) -> {}.

% isVT(VT): prüft, ob VT ein Vektorzeitstempel ist. Rückgabe ist true oder false.
isVT(VT) -> {}.

% syncVT (VT1,VT2): synchronisiert zwei Vektorzeitzstempel (jeweils das Maximum, VT1 wird als eigener Zeitstempel angesehen). 
% Rückgabe ist der neue Vektorzeitstempel.
syncVT(VT1, VT2) -> {}.

% tickVT(VT): zählt den Ereigniszähler des zugehörigen Prozesses um 1 nach oben. 
% Rückgabe ist der neue Vektorzeitstempel.
tickVT(VT) -> {}.

% compVT(VT1,VT2): vergleicht Vektorzeitstempel. Rückgabe: afterVT, beforeVT, equalVT oder concurrentVT.
compVT(VT1, VT2) -> {}.

% aftereqVTJ(VT,VTR): vergleicht im Sinne des kausalen Multicast die Vektorzeitstempel, ob VT aftereq VTR ist, ohne Beachtung von Position J. 
% Die Distanz wird berechnet für die Identität J des Vektorzeitstempels VTR, dem Zeitstempel der Nachricht.  
% Rückgabe: {aftereqVTJ, <Distanz an der Stelle J>} oder false. Dabei wird die Distanz berechnet, durch VT[j] - VTR[j], wobei j die Identität von VTR ist. 
% Wenn die Distanz -1 ist, dann kann die Nachricht mit Zeitstempel VTR ausgeliefert werden.
aftereqVTJ(VT, VTR) -> {}.