file = voley

$(file): $(file).pl
	swipl -O -q -g main --stand_alone=true -o $(file) -c $(file).pl

run: $(file)
	./$(file)

clean:
	rm -f voley model header clauses *.cnf

