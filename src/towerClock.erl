-module(towerClock).

-export([init/0, stop/1]).

% init(): startet die Vektoruhr Zentrale gemäß Spezifikation in towerClock.cfg. In dieser Datei stehen die Node und der lokale Name der Vektoruhr Zentrale. 
% Rückgabewert ist die PID des Tower.
init() -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    TowerClock = spawn(fun() -> loop(Datei) end),
    register(vtKLCclockC,TowerClock),
    util:logging(Datei, "TowerClock initialized with PID: "++util:to_String(TowerClock)++" on Node "++util:to_String(erlang:node())++"\n"),
    TowerClock.

% stop(<PID>): wobei <PID> die Kontaktadresse des Tower oder seine PID ist. Diese Funktion beendet den Tower. 
% Rückgabewert bei Erfolg ist true.
stop(PID) -> 
    exit(whereis(PID), ok).

loop(Datei) ->
    receive
        % {getVecID,<PID>}: als Nachricht, wobei <PID> die Antwortadresse ist. 
        % Der Tower sendet an <PID> mittels der Nachricht {vt,<Prozess-ID>} die eindeutige ID <Prozess-ID> (positive ganze Zahl) dieser Vektoruhr.
        {getVecID, PID} ->
            PID ! {vt, PID}, % TODO: get correct PID
            util:logging(Datei, "vectorID sent to "++util:to_String(PID)++"\n"),
            loop(Datei)
    end.