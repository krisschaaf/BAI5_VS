-module(cbCast).

-export([init/0, stop/1, send/2, received/1, read/1]).

% init(): erstellt einen Prozess für die Kommunikationseinheit. Rückgabe ist ihre PID.
init() ->
    {ok, [{servername,Servername}, {servernode,Servernode}]} = file:consult("configs/towerCBC.cfg"),
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    case net_adm:ping(Servernode) of
        pong -> 
            util:logging(Datei, "TowerCBC "++util:to_String(Servername)++" integrated via ping by Server "++util:to_String(Servernode)++"\n");
        pang -> 
            util:logging(Datei, "TowerCBC "++util:to_String(Servername)++" not integrated via ping by Server "++util:to_String(Servernode)++"\n"),
            throw({error, "Server not reachable"})            
    end,

    CommCBC = spawn(fun() -> loop(Datei, {Servername, Servernode}) end),
    register(cbCast,CommCBC),
    
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

% read(Comm): empfängt nicht blockierend eine Nachricht von der Kommunikationseinheit Comm. 
% Wenn keine Nachricht vorhanden ist, wird null zurück gegeben, sonst eine Zeichenkette.
read(Comm) -> 
    Datei = "logs/"++util:to_String(erlang:node())++".log",

    Comm ! {self(), {getMessage, false}}, %TODO: muss hierfür ein Prozess gestartet werden? Soll die Kommunikationseinheit nicht blockieren oder soll der Prozess nicht blockieren?
    receive 
        {ok_getMessage, {false, Message}} -> Message
        after 1000 -> util:logging(Datei, "Timeout: Could not read Message\n")
    end.

% Die initialisierung der VT muss aus dem richtigten Prozess heraus gestartet werden.
loop(Datei, TowerCBC) ->
    VT = vectorC:initVT(),
    Queues = spawn(fun() -> loopQueues(Datei, VT, [], {[], VT}) end),
    loop(Datei, TowerCBC, Queues).


loop(Datei, TowerCBC, Queues) ->
	receive
        % {<PID>,{castMessage,{<Message>,<VT>}}}: als Nachricht. Empfängt die Nachricht <Message> mit Vektorzeitstempel <VT>. <PID> wird nicht benötigt.
        {_From, {castMessage, {Message, NewVT}}} ->
            util:logging(Datei, "Message: "++util:to_String(Message)++" with Timestamp: "++util:to_String(NewVT)++" casted.\n"),
            Queues ! {self(), {pushHBQ, {Message, NewVT}}}, % TODO: check HBQ and DLQ
            receive
                {ok_pushHBQ} -> ok
                after 1000 -> util:logging(Datei, "Timeout: Could not push Message to HBQ\n")
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
                                            Queues ! {self(), {pushHBQ, {Message, NewVT}}}, % TODO: check HBQ and DLQ
                                            receive
                                                {ok_pushHBQ} -> self() ! {From, {getMessage, true}}  
                                                after 1000 -> util:logging(Datei, "Timeout: Could not push Message to HBQ\n")
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
                    TowerCBC ! {self(), {multicastNB, {Message, NewVT}}}, % TODO: ist Comm und MulticastNB hier richtig? nur manuell?
                    util:logging(Datei, "Message "++util:to_String(Message)++" with VT "++util:to_String(NewVT)++" sent from "++util:to_String(Comm)++"\n"),
                    Queues ! {self(), {pushDLQ, {Message, NewVT}}},
                    receive
                        {ok_pushDLQ} -> From ! {ok_send}
                        after 1000 -> util:logging(Datei, "Timeout: Could not push Message to DLQ\n")
                    end,
                    loop(Datei, TowerCBC, Queues)
                after 1000 -> util:logging(Datei, "Timeout: Could not tick VT\n")
            end;
        
        {From, {stop}} when is_pid(From)->
            From ! {ok_stop};
        
        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loop(Datei, TowerCBC, Queues)
	end.

loopQueues(Datei, VT, HBQ, DLQ) ->
    receive
        {From, {pushHBQ, {Message, NewVT}}} when is_pid(From) ->
            NewHBQ = pushHBQ(Message, NewVT, HBQ),
            From ! {ok_pushHBQ},
            loopQueues(Datei, VT, NewHBQ, DLQ); %TODO: VT ?

        {From, {pushDLQ, {Message, NewVT}}} when is_pid(From) ->
            NewDLQ = pushDLQ(Message, NewVT, DLQ),
            From ! {ok_pushDLQ},
            loopQueues(Datei, VT, HBQ, NewDLQ); % TODO: VT ?

        {From, {popDLQ}} when is_pid(From) ->
            case popDLQ(DLQ) of
                {NewDLQ, {Message, NewVT}} -> 
                    From ! {ok_popDLQ, {Message, NewVT}},
                    loopQueues(Datei, VT, HBQ, NewDLQ); %TODO: VT ?
                {NewDLQ, {}} -> 
                    From ! {ok_popDLQ, {}},
                    loopQueues(Datei, VT, HBQ, NewDLQ);
                _ -> error
            end;

        {From, {checkQueues}} when is_pid(From) ->
            {NewHBQ, NewDLQ} = checkQueues(HBQ, DLQ),
            From ! {ok_checkQueues},
            loopQueues(Datei, VT, NewHBQ, NewDLQ);

        {From, {syncVT, {AsyncVT}}} when is_pid(From) ->
            NewVT = vectorC:syncVT(VT, AsyncVT),
            From ! {ok_syncVT, NewVT},
            loopQueues(Datei, NewVT, HBQ, DLQ);

        {From, {tickVT}} -> 
            NewVT = vectorC:tickVT(VT),
            From ! {ok_tickVT, NewVT},
            loopQueues(Datei, NewVT, HBQ, DLQ);

        Any -> 
            util:logging(Datei, "Unknown message: "++util:to_String(Any)++"\n"),
            loopQueues(Datei, VT, HBQ, DLQ)
    end.

%TODO: implement
pushHBQ(Message, VT, HBQ) ->
    HBQ ++ [{Message, VT}].

%TODO: implement
pushDLQ(Message, VT, DLQ) -> 
    %TODO: sync DLQ VT here
    {Queue, OldVT} = DLQ,
    NewQueue = Queue ++ [{Message, VT}],
    {NewQueue, OldVT}. %TODO: VT?

popDLQ({[], VT}) -> {{[], VT}, {}};
popDLQ({[HeadMsg|TailMsgList], VT}) -> {{TailMsgList, VT}, HeadMsg}.

checkQueues(HBQ, DLQ) -> {}.

