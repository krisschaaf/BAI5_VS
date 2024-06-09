-module(towerClock).

-export([init/0, stop/1]).

% init(): startet die Vektoruhr Zentrale gemäß Spezifikation in towerClock.cfg. In dieser Datei stehen die Node und der lokale Name der Vektoruhr Zentrale. 
% Rückgabewert ist die PID des Tower.
init() -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    TowerClock = spawn(fun() -> loop(Datei, []) end),
    register(vtKLCclockC,TowerClock),
    util:logging(Datei, "TowerClock initialized with PID: "++util:to_String(TowerClock)++" on Node "++util:to_String(erlang:node())++"\n"),
    TowerClock.

% stop(<PID>): wobei <PID> die Kontaktadresse des Tower oder seine PID ist. Diese Funktion beendet den Tower. 
% Rückgabewert bei Erfolg ist true.
stop(PID) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    PID ! {self(), {stop}},
    receive
        {ok_stop} -> 
            unregister(vtKLCclockC), %TODO: get tower pid
            true
        after 5000 ->
            util:logging(Datei, "Timeout: TowerClock not stopped, killing now...\n"),
            exit(whereis(PID), ok),
            unregister(vtKLCclockC)
    end.

loop(Datei, Map) ->
    receive
        % {getVecID,<PID>}: als Nachricht, wobei <PID> die Antwortadresse ist. 
        % Der Tower sendet an <PID> mittels der Nachricht {vt,<Prozess-ID>} die eindeutige ID <Prozess-ID> (positive ganze Zahl) dieser Vektoruhr.
        {getVecID, PID} ->
            case map(Map, PID) of
                undefined ->
                    ID = erlang:length(Map)+1,
                    util:logging(Datei, "VecID "++util:to_String(ID)++" registered and sent to "++util:to_String(PID)++"\n"),
                    PID ! {vt, ID},
                    loop(Datei, Map++[{PID, ID}]);
                ID -> 
                    util:logging(Datei, "VecID "++util:to_String(ID)++" sent to "++util:to_String(PID)++"\n"),
                    PID ! {vt, ID},
                    loop(Datei, Map)
            end;
        {From, {stop}} when is_pid(From)->
            From ! {ok_stop};
        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, Map)
    end.

map([], _PID) ->
    undefined;
map([{PID, ID} | _], PID) ->
    ID;
map([_ | T], PID) ->
    map(T, PID).