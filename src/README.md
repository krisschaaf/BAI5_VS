# Bitte lesen

## testCBC:test()

- ein Ordner logs muss im Projektverzeichnis erstellt werden (auf gleicher Ebene wie die *.erl Dateien)

## app.erl

### Vorbereitungen

- ein Ordner logs muss im Projektverzeichnis erstellt werden (auf gleicher Ebene wie die *.erl Dateien)
- die app.cfg muss angepasst werden
- es müssen 5 Nodes gestartet werden. Default config: towerCBC@... , towerClock@..., botA@..., botB@..., botC@...

### Ausführung

Die Anwendung läuft auf einem eigenen Node.

Initialisierung der App über app:init() -> Bei Erfolg "ok" als Rückgabe.

Dann app:start(app) um die eigentliche App zu starten. 

Aufruf von app:stop(app) beendet die App.

```bash
krisschaaf...:~/.../src|⇒ erl -sname 'app@MacBook-Air-von-Kristoffer' -setcookie zumsl
Erlang/OTP 26 [erts-14.2.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit] [dtrace]

Eshell V14.2.2 (press Ctrl+G to abort, type help(). for help)
(app@MacBook-Air-von-Kristoffer)2> app:init().
ok
(app@MacBook-Air-von-Kristoffer)3> app:start(app).
true
```

Die Ergebnisse sind in der Datei <b>logs/'app@ * .log'</b> zu finden.