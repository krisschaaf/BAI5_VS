-module(cbCast).

-export([init/0, stop/1, send/2, received/1, read/1, listQueues/1]).

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
            done
        after 5000 ->
            util:logging(Datei, "Timeout: CbCast not stopped, killing now...\n"),
            exit(whereis(Comm), ok)
    end.

% send(Comm,Message): sendet eine Nachricht Message über die Kommunikationseinheit Comm als kausaler Multicast an alle anderen in der Gruppe.
send(Comm, Message) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {send, {Comm, Message}}},
    receive 
        {replycbcast, ok_send} -> 
            done
        after 1000 -> 
            util:logging(Datei, "Timeout: Message '"++util:to_String(Message)+"' could not be sent\n")
    end.

% received(Comm): empfängt blockierend eine Nachricht von der Kommunikationseinheit Comm. Die Rückgabe ist eine Zeichenkette.
received(Comm) -> 
    Comm ! {self(), {getMessage, true}},
    receive 
        {replycbcast, ok_getMessage, {true, Message}} -> Message
    end.

% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {getMessage, false}},
    receive 
        {replycbcast, ok_getMessage, {false, Message}} -> Message
        after 1000 -> util:logging(Datei, "Timeout: Could not read Message\n")
    end.


listQueues(Comm) ->
    Comm ! {self(), {listQueues}},
    receive
        {replycbcast, ok_listQueues} -> ok
    after 1000 -> false
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
            case vectorC:isVT(NewVT) of 
                false ->
                    util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" discarded.\n"),
                    loop(Datei, TowerCBC, Queues)
            end,
            Queues ! {self(), {getVTid}},
            receive
                {replyqueues, ok_getVTid, VTid} -> 
                    case VTid == vectorC:myVTid(NewVT) of
                        false -> 
                            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" received.\n"),
                            Queues ! {self(), {pushHBQ, {Message, NewVT}}},
                            receive
                                {replyqueues, ok_pushHBQ} -> 
                                    Queues ! {self(), {checkQueues}},
                                    receive
                                        {replyqueues, ok_checkQueues} -> ok
                                        after 1000 -> util:logging(Datei, "Timeout: Could not check Queues\n")
                                    end
                            end;
                        true -> 
                            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" discarded.\n")
                    end
                after 1000 -> util:logging(Datei, "Timeout: Could not push Message to HBQ\n")
            end,
            loop(Datei, TowerCBC, Queues);
        
        {From, {getMessage, Blocking}} when is_pid(From)->
            Queues ! {self(), {popDLQ}},
            receive 
                {replyqueues, ok_popDLQ, Msg} ->
                    case Msg of
                        {Message, MsgVT} -> 
                            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(MsgVT)++" read.\n"),
                            From ! {replycbcast, ok_getMessage, {Blocking, Message}},
                            Queues ! {self(), {syncVT, {MsgVT}}},
                            receive
                                {replyqueues, ok_syncVT} -> 
                                    Queues ! {self(), {checkQueues}},
                                    receive
                                        {replyqueues, ok_checkQueues} -> ok
                                        after 1000 -> util:logging(Datei, "Timeout: Could not check Queues\n")
                                    end
                                after 1000 -> util:logging(Datei, "Timeout: Could not sync VT\n")
                                end,
                            loop(Datei, TowerCBC, Queues);
                        {} -> 
                            case Blocking of
                                true -> 
                                    util:logging(Datei, "Waiting on Message\n"),
                                    receive
                                        {NewFrom, {stop}} when is_pid(NewFrom)->
                                            NewFrom ! {replycbcast, ok_stop};
                                        {_From, {castMessage, {Message, NewVT}}} ->
                                            case vectorC:isVT(NewVT) of 
                                                true ->
                                                    util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" received.\n"),
                                                    Queues ! {self(), {pushHBQ, {Message, NewVT}}},
                                                    receive
                                                        {replyqueues, ok_pushHBQ} -> self() ! {From, {getMessage, true}}  
                                                        after 1000 -> util:logging(Datei, "Timeout: Could not push Message to HBQ\n")
                                                    end,
                                                    Queues ! {self(), {checkQueues}},
                                                    receive
                                                        {replyqueues, ok_checkQueues} -> ok
                                                        after 1000 -> util:logging(Datei, "Timeout: Could not check Queues\n")
                                                    end;
                                                false ->
                                                    util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" discarded.\n")
                                            end
                                    end;
                                false -> 
                                    From ! {replycbcast, ok_getMessage, {false, null}},
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
                {replyqueues, ok_tickVT, NewVT} -> 
                    TowerCBC ! {self(), {multicastNB, {Message, NewVT}}},
                    util:logging(Datei, "Message "++util:to_String(Message)++" with VT "++util:to_String(NewVT)++" sent from "++util:to_String(Comm)++"\n"),
                    
                    Queues ! {self(), {pushDLQ, {Message, NewVT}}},
                    receive
                        {replyqueues, ok_pushDLQ} -> 
                            Queues ! {self(), {checkQueues}},
                            receive
                                {replyqueues, ok_checkQueues} -> ok
                                after 1000 -> util:logging(Datei, "Timeout: Could not check Queues\n")
                            end
                    end

                after 1000 -> util:logging(Datei, "Timeout: Could not tick VT\n")
            end,
            From ! {replycbcast, ok_send},
            loop(Datei, TowerCBC, Queues);
        
        {From, {stop}} when is_pid(From)->
            From ! {replycbcast, ok_stop};
        
        {From, {listQueues}} when is_pid(From)->
            Queues ! {self(), {listQueues}},
            receive
                {replyqueues, ok_listQueues} -> 
                    util:logging(Datei, "Queues listed.\n"),
                    From ! {replycbcast, ok_listQueues}
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
            From ! {replyqueues, ok_pushHBQ},
            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" pushed to HBQ.\n"),
            loopQueues(Datei, VT, NewHBQ, DLQ);

        {From, {pushDLQ, {Message, NewVT}}} when is_pid(From) ->
            NewDLQ = pushDLQ({Message, NewVT}, DLQ),
            From ! {replyqueues, ok_pushDLQ},
            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" pushed to DLQ.\n"),
            loopQueues(Datei, VT, HBQ, NewDLQ);

        {From, {popDLQ}} when is_pid(From) ->
            case popDLQ(DLQ) of
                {Message, NewDLQ} -> 
                    From ! {replyqueues, ok_popDLQ, Message},
                    util:logging(Datei, "Message: "++util:to_String(Message)++" popped from DLQ.\n"),
                    loopQueues(Datei, VT, HBQ, NewDLQ);
                _ -> error
            end;

        {From, {checkQueues}} when is_pid(From) ->
            {NewHBQ, NewDLQ} = checkQueues(Datei, HBQ, DLQ, VT),
            From ! {replyqueues, ok_checkQueues},
            util:logging(Datei, "Queues checked.\n"),
            loopQueues(Datei, VT, NewHBQ, NewDLQ);

        {From, {syncVT, {AsyncVT}}} when is_pid(From) ->
            NewVT = vectorC:syncVT(VT, AsyncVT),
            From ! {replyqueues, ok_syncVT},
            util:logging(Datei, "Comm VT "++util:to_String(VT)++" synchronized with "++util:to_String(AsyncVT)++" is now "++util:to_String(NewVT)++".\n"),
            loopQueues(Datei, NewVT, HBQ, DLQ);

        {From, {tickVT}} -> 
            NewVT = vectorC:tickVT(VT),
            From ! {replyqueues, ok_tickVT, NewVT},
            util:logging(Datei, "VT "++util:to_String(VT)++" ticked to "++util:to_String(NewVT)++"\n"),
            loopQueues(Datei, NewVT, HBQ, DLQ);

        {From, {getVTid}} when is_pid(From) ->
            VTid = vectorC:myVTid(VT),
            From ! {replyqueues, ok_getVTid, VTid},
            loopQueues(Datei, VT, HBQ, DLQ);

        {From, {listQueues}} when is_pid(From) ->
            util:logging(Datei, "HBQ: "++util:to_String(HBQ)++"\nDLQ: "++util:to_String(DLQ)++"\n"),
            From ! {replyqueues, replycbc, ok_listQueues},
            loopQueues(Datei, VT, HBQ, DLQ);

        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loopQueues(Datei, VT, HBQ, DLQ)
    end.

% returns new DLQ
% new msg are added to the head of the list
pushHBQ(Message, HBQ) -> 
    [Message|HBQ].

% returns new DLQ
% new msg are added to the head of the list
pushDLQ(Message, DLQ) -> 
    [Message|DLQ].

% returns popped Element and new DLQ without popped Element
popDLQ(DLQ) -> getLastElement(DLQ).

% returns 
checkQueues(Datei, HBQ, DLQ, VT) -> checkQueues(Datei, HBQ, HBQ, DLQ, VT).

checkQueues(Datei, _, [], DLQ, VT) ->
    util:logging(Datei, "Queues>>> Queues checked with VT "++util:to_String(VT)++" (HBQ empty).\n"), 
    util:logging(Datei, "DLQ>>> Current Size "++util:to_String(length(DLQ))++".\n"), 
    {[],DLQ};
checkQueues(Datei, [], HBQ, DLQ, VT) ->
    util:logging(Datei, "Queues>>> Queues checked with VT "++util:to_String(VT)++".\n"),
    util:logging(Datei, "HBQ>>> Current Size "++util:to_String(length(HBQ))++".\n"), 
    util:logging(Datei, "DLQ>>> Current Size "++util:to_String(length(DLQ))++".\n"), 
    {HBQ, DLQ};
checkQueues(Datei, [HBQHead|HBQTail], HBQ, DLQ, VT) ->
    VTvcHBQ = vectorC:myVTvc(HBQHead),
    case vectorC:aftereqVTJ(VT, VTvcHBQ) of
        {aftereqVTJ, -1} -> 
            NewDLQ = pushDLQ(HBQHead, DLQ),
            NewHBQ = removeFromList(HBQ, HBQHead),
            util:logging(Datei, "Queues>>> removed "++util:to_String(HBQHead)++" from HBQ and added to DLQ (aftereqVTJ, -1).\n"),
            checkQueues(Datei, HBQTail, NewHBQ, NewDLQ, VT);
        {aftereqVTJ, 0} -> %TODO: check if this is correct
            NewDLQ = pushDLQ(HBQHead, DLQ),
            NewHBQ = removeFromList(HBQ, HBQHead),
            util:logging(Datei, "Queues>>> removed "++util:to_String(HBQHead)++" from HBQ and added to DLQ (aftereqVTJ, 0).\n"),
            checkQueues(Datei, HBQTail, NewHBQ, NewDLQ, VT);
        {aftereqVTJ, _} -> 
            checkQueues(Datei, HBQTail, HBQ, DLQ, VT);
        false -> 
            checkQueues(Datei, HBQTail, HBQ, DLQ, VT)
    end.

removeFromList(List, Element) -> 
    removeFromList(List, Element, []).

removeFromList([], _Element, NewList) -> NewList;
removeFromList([Head|Tail], Element, NewList) when Head == Element -> 
    removeFromList(Tail, Element, NewList);
removeFromList([Head|Tail], Element, NewList) ->
    removeFromList(Tail, Element, NewList++[Head]).

% returns last Element and new List without last Element
getLastElement([]) -> {[], []};
getLastElement(List) -> getLastElement(List, []).
getLastElement([Head|[]], NewList) -> {Head, NewList};
getLastElement([Head|Tail], NewList) -> getLastElement(Tail, NewList++[Head]).

