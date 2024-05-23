    erl -sname 'towerClock@MacBook-Air-von-Kristoffer' -setcookie zumsl

    erl -sname 'towerCBC@MacBook-Air-von-Kristoffer' -setcookie zumsl

    net_adm:ping('towerClock@MacBook-Air-von-Kristoffer').

## Fragen:

- Muss der Tower sowohl die PID als auch den Node bei der Registrierung speichern?
- Wie genau werden die Comm Module initialisiert?