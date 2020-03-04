#!/bin/sh

print_date(){
    date "+%a %m-%d %T"
}
print_battery(){
    apm -l
}

while true
do
    xsetroot -name "🔋$(print_battery)% | $(print_date)"
    sleep 1

done
