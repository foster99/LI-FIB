file = minColoring

$(file): $(file).pl
	swipl -q -O -g main --stand_alone=true -o $(file) -c $(file).pl

run: $(file)
	./$(file)

clean:
	rm -f $(file) model header clauses *.cnf

