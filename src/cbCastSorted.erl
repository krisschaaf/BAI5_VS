-module(cbCast).

-export([init/0, stop/1, send/2, received/1, read/1, pushHBQ/2, pushDLQ/2, checkQueues/3, listQueues/1, popDLQ/1]).

% init(): erstellt einen Prozess für die Kommunikationseinheit. Rückgabe ist ihre PID.
init() ->
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("towerCBC.cfg"),
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    case net_adm:ping(Servernode) of
        pong -> 
            util:logging(Datei, "TowerCBC "++util:to_String(Servername)++" integrated via ping by Server "++util:to_String(Servernode)++"\n");
        pang -> 
            util:logging(Datei, "TowerCBC "++util:to_String(Servername)++" not integrated via ping by Server "++util:to_String(Servernode)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    CommCBC = spawn(fun() -> loop(Datei, {Servername, Servernode}) end),
    % register(cbCast,CommCBC),
    
    {Servername, Servernode} ! {self(), {register, CommCBC}},
    receive
        {replycbc, ok_registered} ->
            util:logging(Datei, "cbCast registered at "++util:to_String(Servername)++" with PID: "++util:to_String(CommCBC)++" on Node "++util:to_String(erlang:node())++"\n");
        {replycbc, ok_existing} ->
            util:logging(Datei, "cbCast already registered at "++util:to_String(Servername)++" with PID: "++util:to_String(CommCBC)++" on Node "++util:to_String(erlang:node())++"\n")
        after 1000 ->
            util:logging(Datei, "Timeout: cbCast not registered\n")
    end,
    CommCBC.

% stop(Comm): terminiert die Kommunikationseinheit mit PID Comm. Rückgabe ist done.
stop(Comm) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {stop}},
    receive
        {ok_stop} -> 
            % unregister(cbCast), %TODO: get Comm pid
            done
        after 5000 ->
            util:logging(Datei, "Timeout: CbCast not stopped, killing now...\n"),
            exit(whereis(Comm), ok)
            % unregister(cbCast)
    end.

% send(Comm,Message): sendet eine Nachricht Message über die Kommunikationseinheit Comm als kausaler Multicast an alle anderen in der Gruppe.
send(Comm, Message) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {send, {Comm, Message}}},
    receive 
        {ok_send} -> 
            done
        after 1000 -> 
            util:logging(Datei, "Timeout: Message '"++util:to_String(Message)+"' could not be sent\n")
    end.

% received(Comm): empfängt blockierend eine Nachricht von der Kommunikationseinheit Comm. Die Rückgabe ist eine Zeichenkette.
received(Comm) -> 
    Comm ! {self(), {getMessage, true}},
    receive 
        {ok_getMessage, {true, Message}} -> Message
    end.

listQueues(Comm) ->
    Comm ! {self(), {listQueues}},
    receive
        {replycbc, ok_listQueues} -> ok %TODO: replycbc überall anpassen
    after 1000 -> false
    end.


% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {getMessage, false}},
    receive 
        {ok_getMessage, {false, Message}} -> Message
        after 1000 -> util:logging(Datei, "Timeout: Could not read Message\n")
    end.

% Die initialisierung der VT muss aus dem richtigten Prozess heraus gestartet werden.
loop(Datei, TowerCBC) ->
    VT = vectorC:initVT(),
    Queues = spawn(fun() -> loopQueues(Datei, VT, [], []) end),
    loop(Datei, TowerCBC, Queues).


loop(Datei, TowerCBC, Queues) ->
	receive
        % {<PID>,{castMessage,{<Message>,<VT>}}}: als Nachricht. Empfängt die Nachricht <Message> mit Vektorzeitstempel <VT>. <PID> wird nicht benötigt.
        {_From, {castMessage, {Message, NewVT}}} ->
            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" casted.\n"),
            Queues ! {self(), {pushHBQ, {Message, NewVT}}},
            receive
                {ok_pushHBQ} -> ok
                after 1000 -> util:logging(Datei, "Timeout: Could not push Message to HBQ\n")
            end,
            Queues ! {self(), {checkQueues}},
            receive
                {ok_checkQueues} -> ok
                after 1000 -> util:logging(Datei, "Timeout: Could not check Queues\n")
            end,
            loop(Datei, TowerCBC, Queues);
        
        {From, {getMessage, Blocking}} when is_pid(From)->
            Queues ! {self(), {popDLQ}},
            receive 
                {ok_popDLQ, Msg} ->
                    case Msg of
                        {Message, MsgVT} -> 
                            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(MsgVT)++" read.\n"),
                            From ! {ok_getMessage, {Blocking, Message}},
                            Queues ! {self(), {syncVT, {MsgVT}}},
                            receive
                                {ok_syncVT, _} -> loop(Datei, TowerCBC, Queues)
                                after 1000 -> util:logging(Datei, "Timeout: Could not sync VT\n")
                            end;
                        {} -> 
                            case Blocking of
                                true -> 
                                    util:logging(Datei, "Waiting on Message\n"),
                                    receive
                                        {NewFrom, {stop}} when is_pid(NewFrom)->
                                            NewFrom ! {ok_stop};
                                        {_From, {castMessage, {Message, NewVT}}} ->
                                            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" received.\n"),
                                            Queues ! {self(), {pushHBQ, {Message, NewVT}}},
                                            receive
                                                {ok_pushHBQ} -> self() ! {From, {getMessage, true}}  
                                                after 1000 -> util:logging(Datei, "Timeout: Could not push Message to HBQ\n")
                                            end,
                                            Queues ! {self(), {checkQueues}},
                                            receive
                                                {ok_checkQueues} -> ok
                                                after 1000 -> util:logging(Datei, "Timeout: Could not check Queues\n")
                                            end
                                    end;
                                false -> 
                                    From ! {ok_getMessage, {false, null}},
                                    loop(Datei, TowerCBC, Queues)
                            end;
                        _ -> error
                    end
                after 1000 -> util:logging(Datei, "Timeout: Could not pop Message from DLQ\n")
            end,
            loop(Datei, TowerCBC, Queues);
       
        {From, {send, {Comm, Message}}} when is_pid(From)->
            Queues ! {self(), {tickVT}},
            receive
                {ok_tickVT, NewVT} -> 
                    TowerCBC ! {self(), {multicastNB, {Message, NewVT}}},
                    util:logging(Datei, "Message "++util:to_String(Message)++" with VT "++util:to_String(NewVT)++" sent from "++util:to_String(Comm)++"\n"),
                    Queues ! {self(), {pushDLQ, {Message, NewVT}}}, % TODO: remove!
                    receive
                        {ok_pushDLQ} -> From ! {ok_send}
                        after 1000 -> util:logging(Datei, "Timeout: Could not push Message to DLQ\n")
                    end,
                    loop(Datei, TowerCBC, Queues)
                after 1000 -> util:logging(Datei, "Timeout: Could not tick VT\n")
            end;
        
        {From, {stop}} when is_pid(From)->
            From ! {ok_stop};
        
        {From, {listQueues}} when is_pid(From)->
            Queues ! {self(), {listQueues}},
            receive
                {replycbc, ok_listQueues} -> 
                    util:logging(Datei, "Queues listed.\n"),
                    From ! {replycbc, ok_listQueues}
            after 1000 -> util:logging(Datei, "Timeout: Could not list Queues\n")
            end,
            loop(Datei, TowerCBC, Queues);
        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, TowerCBC, Queues)
	end.

loopQueues(Datei, VT, HBQ, DLQ) ->
    receive
        {From, {pushHBQ, {Message, NewVT}}} when is_pid(From) ->
            NewHBQ = pushHBQ({Message, NewVT}, HBQ),
            From ! {ok_pushHBQ},
            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" pushed to HBQ.\n"),
            loopQueues(Datei, VT, NewHBQ, DLQ);

        {From, {pushDLQ, {Message, NewVT}}} when is_pid(From) ->
            NewDLQ = pushDLQ({Message, NewVT}, DLQ),
            From ! {ok_pushDLQ},
            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" pushed to DLQ.\n"),
            loopQueues(Datei, VT, HBQ, NewDLQ);

        {From, {popDLQ}} when is_pid(From) ->
            case popDLQ(DLQ) of
                {Message, NewDLQ} -> 
                    From ! {ok_popDLQ, Message},
                    util:logging(Datei, "Message: "++util:to_String(Message)++" popped from DLQ.\n"),
                    loopQueues(Datei, VT, HBQ, NewDLQ);
                _ -> error
            end;

        {From, {checkQueues}} when is_pid(From) ->
            {NewHBQ, NewDLQ} = checkQueues(Datei, HBQ, DLQ),
            From ! {ok_checkQueues},
            util:logging(Datei, "Queues checked.\n"),
            loopQueues(Datei, VT, NewHBQ, NewDLQ);

        {From, {syncVT, {AsyncVT}}} when is_pid(From) ->
            NewVT = vectorC:syncVT(VT, AsyncVT),
            From ! {ok_syncVT, NewVT},
            util:logging(Datei, "Comm VT "++util:to_String(VT)++" synchronized with "++util:to_String(AsyncVT)++"\n"),
            loopQueues(Datei, NewVT, HBQ, DLQ);

        {From, {tickVT}} -> 
            NewVT = vectorC:tickVT(VT),
            From ! {ok_tickVT, NewVT},
            util:logging(Datei, "VT "++util:to_String(VT)++" ticked to "++util:to_String(NewVT)++"\n"),
            loopQueues(Datei, NewVT, HBQ, DLQ);

        {From, {listQueues}} when is_pid(From) ->
            util:logging(Datei, "HBQ: "++util:to_String(HBQ)++"\nDLQ: "++util:to_String(DLQ)++"\n"),
            From ! {replycbc, ok_listQueues},
            loopQueues(Datei, VT, HBQ, DLQ);

        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loopQueues(Datei, VT, HBQ, DLQ)
    end.

% returns new HBQ
% new msg are added to the list in the right position, starting at the head (head is largest VT)
pushHBQ(Message, []) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",
    util:logging(Datei, "empty "++util:to_String(Message)++"\n"),%TODO: remove
    [Message];
pushHBQ(Message, [Head|Tail]) ->
    {_, VT1} = Message,
    {_, VT2} = Head,
    Position = vectorC:compVT(VT1, VT2),
    case Position of
        afterVT -> 
            Datei = "logs/"++util:to_String(erlang:node())++".log",
            util:logging(Datei, "after "++util:to_String([Message|[Head|Tail]]++"\n")),%TODO: remove
            [Message|[Head|Tail]]; %TODO: Listenaufbau debuggen
        beforeVT -> 
            Datei = "logs/"++util:to_String(erlang:node())++".log",
            util:logging(Datei, "before "++util:to_String([Head|pushHBQ(Message, Tail)])++"\n"),%TODO: remove
            [Head|pushHBQ(Message, Tail)];
        equalVT -> 
            Datei = "logs/"++util:to_String(erlang:node())++".log",
            util:logging(Datei, "equal "++util:to_String([Head|Tail])++"\n"),%TODO: remove
            [Head|Tail]; %TODO: testen
        concurrentVT -> 
            Datei = "logs/"++util:to_String(erlang:node())++".log",
            util:logging(Datei, "concurrent "++util:to_String([Message|[Head | Tail]])++"\n"),%TODO: remove
            [Message|[Head|Tail]]
    end.

% returns new DLQ
% new msg are added to the head of the list
pushDLQ(Message, DLQ) -> 
    [Message|DLQ].

% returns popped Element and new DLQ without popped Element
popDLQ(DLQ) -> getLastElement(DLQ).

% returns 
checkQueues(Datei, [], []) -> 
    util:logging(Datei, "HBQ>>> HBQ is empty.\n"),
    {[],[]};
checkQueues(Datei, [], DLQ) ->
    util:logging(Datei, "HBQ>>> HBQ is empty.\n"), 
    {[],DLQ};
checkQueues(Datei, HBQ, []) -> 
    {HBQLastElement, NewHBQ} = getLastElement(HBQ),
    NewDLQ = pushDLQ(HBQLastElement, []),
    util:logging(Datei, "HBQ>>> removed "++util:to_String(HBQLastElement)++" from HBQ and added to DLQ (DLQ empty).\n"),
    checkQueues(Datei, NewHBQ, NewDLQ);
checkQueues(Datei, HBQ, DLQ) ->
    {HBQLastElement, NewHBQ} = getLastElement(HBQ),
    {_, VTHBQ} = HBQLastElement,
     [{_, VTDLQ}|_] = DLQ,
    case vectorC:aftereqVTJ(VTDLQ, VTHBQ) of
        {aftereqVTJ, -1} -> 
            NewDLQ = pushDLQ(HBQLastElement, DLQ),
            util:logging(Datei, "HBQ>>> removed "++util:to_String(HBQLastElement)++" from HBQ and added to DLQ (aftereqVTJ).\n"),
            checkQueues(Datei, NewHBQ, NewDLQ);
        %TODO: aftereqVTJ, 0 -> ist das equalVT und kann verworfen werden?
        {aftereqVTJ, _} -> 
            util:logging(Datei, "HBQ>>> up to date.\n"),
            {HBQ, DLQ};
        false -> 
            util:logging(Datei, "HBQ>>> up to date.\n"),
            {HBQ, DLQ}
    end.

% returns last Element and new List without last Element
getLastElement([]) -> {[], []};
getLastElement(List) -> getLastElement(List, []).
getLastElement([Head|[]], NewList) -> {Head, NewList};
getLastElement([Head|Tail], NewList) -> getLastElement(Tail, NewList++[Head]).

