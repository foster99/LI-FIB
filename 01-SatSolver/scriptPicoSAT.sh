#!/bin/bash

start_measuring_time() {
  read start < <(date +'%s')
}

stop_measuring_time() {
  read end < <(date +'%s')
}

show_elapsed_time() {
  echo "$((end-start)) s"
}

for f in random3SAT/vars-2*.cnf
#for f in random3SAT/vars*.cnf
do
    echo
    echo "------------------"
    echo $f
    echo "picoSAT:"
    start_measuring_time
    picosat -v $f > salPicoSAT
    stop_measuring_time
    egrep -o "UNSATISFIABLE|SATISFIABLE" salPicoSAT
    egrep "decisions" salPicoSAT
    show_elapsed_time
    echo "misat:"
    start_measuring_time
    ./misat < $f
    stop_measuring_time
    show_elapsed_time
done
