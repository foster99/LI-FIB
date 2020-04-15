file = horarios

$(file): $(file).pl
	swipl -q -O -g main --stand_alone=true -o $(file) -c $(file).pl

test: $(file)_1.pl $(file)_2.pl $(file)_3.pl $(file)_4.pl $(file)_5.pl
	swipl -q -O -g main --stand_alone=true -o $(file)_1 -c $(file)_1.pl
	swipl -q -O -g main --stand_alone=true -o $(file)_2 -c $(file)_2.pl
	swipl -q -O -g main --stand_alone=true -o $(file)_3 -c $(file)_3.pl
	swipl -q -O -g main --stand_alone=true -o $(file)_4 -c $(file)_4.pl
	swipl -q -O -g main --stand_alone=true -o $(file)_5 -c $(file)_5.pl



clean:
	rm -f ligas horarios flow miSudoku model header clauses *.cnf

