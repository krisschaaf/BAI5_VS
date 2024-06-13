    erl -sname 'towerClock@MacBook-Air-von-Kristoffer' -setcookie zumsl

    erl -sname 'towerCBC@MacBook-Air-von-Kristoffer' -setcookie zumsl

    erl -sname 'cbCast1@MacBook-Air-von-Kristoffer' -setcookie zumsl
    erl -sname 'cbCast2@MacBook-Air-von-Kristoffer' -setcookie zumsl
    erl -sname 'cbCast3@MacBook-Air-von-Kristoffer' -setcookie zumsl

    net_adm:ping('towerClock@MacBook-Air-von-Kristoffer').