
for i in {1,2,3,4,5}
do
    mkdir horarios$i
    mv salida$i.out horarios_$i* horarios$i/
done

mkdir entradas
mv entradaHoraris*.pl entradas/

rm *.cnf model header clauses