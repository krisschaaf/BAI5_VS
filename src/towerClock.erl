-module(towerClock).

-export([init/0, stop/1]).

% {getVecID,<PID>}: als Nachricht, wobei <PID> die Antwortadresse ist. Der Tower sendet an <PID> mittels der Nachricht {vt,<Prozess-ID>} die eindeutige ID <Prozess-ID> (positive ganze Zahl) dieser Vektoruhr.


% init(): startet die Vektoruhr Zentrale gemäß Spezifikation in towerClock.cfg. In dieser Datei stehen die Node und der lokale Name der Vektoruhr Zentrale. 
% Rückgabewert ist die PID des Tower.
init() -> {}.

% stop(<PID>): wobei <PID> die Kontaktadresse des Tower oder seine PID ist. Diese Funktion beendet den Tower. 
% Rückgabewert bei Erfolg ist true.
stop(PID) -> {}.