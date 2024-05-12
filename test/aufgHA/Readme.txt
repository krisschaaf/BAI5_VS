--------------------
Compilieren der Dateien:
--------------------
Zu dem Paket gehören die Dateien
towerClock.beam; towerCBC.beam; vectorC.beam; cbCast.beam
util.beam; vsutil.beam; 

sowie:
Readme.txt; towerCBC.cfg; towerClock.cfg

ACHTUNG: tower*.cfg wird nur von vectorC und cbCast genutzt! 

--------------------
Starten der Nodes:
--------------------
(w)erl -(s)name <towerCBCNode> -setcookie zummsel
(w)erl -(s)name <towerClockNode> -setcookie zummsel
(w)erl -(s)name <cbCast*Node> -setcookie zummsel

Starten der tower* (auf unterschiedlichen Nodes):
--------------------------
1>towerClock:init( ).
towerClock:cfg:
{servername, <Name auf der Node>}.
{servernode, <Node des Tower>}.

1>towerCBC:init(<manu|auto>).
towerCBC:cfg:
{servername, <Name auf der Node>}.
{servernode, <Node des Tower>}.


Starten der Kommunikationseinheiten (auf unterschiedlichen Nodes):
--------------------------
3>cbCast:init().
greift auf towerCBC.cfg zu. Über die ADT vectorC wird auf towerClock.cfg zugegriffen.


Runterfahren:
-------------
2> Ctrl/Strg Shift G
-->q

Durchführung der Tests:
-------------
auf der towerCBC-Node
Alle anderen Nodes (keine Programme!!) müssen gemäss test.cfg gestartet sein.
1> testCBC:testADT().
evtl. die Nodes neu starten
2> testCBC:test().