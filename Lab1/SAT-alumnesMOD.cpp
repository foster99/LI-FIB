#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <map>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

// Lista Ocurrencia
vector< vector<int> > pos_ocurr;
vector< vector<int> > neg_ocurr;

// Heurisitco
vector<double> usageCounter;
uint conflictCount = 0;

    ////////// VALORES A MODIFICAR PARA TUNEAR EL HEURISTICO //////////
    const uint NUM_DEC      = 50;
    const double PRIZE      = 500.0;
    const double DIVISOR    = 10.0;
    ///////////////////////////////////////////////////////////////////

// Medicion
double elapsed_time;
uint decisionsCount = 0;
uint propagationsCount = 0;

void readClauses( ) {
	// Skip comments
	char c = cin.get();
	while (c == 'c') {
		while (c != '\n') c = cin.get();
		c = cin.get();
	}  
	// Read "cnf numVars numClauses"
	string aux;
	cin >> aux >> numVars >> numClauses;
	
	clauses.resize(numClauses);
	/* -------------------------------------------------------------------*/
	pos_ocurr.resize(numVars + 1);
	neg_ocurr.resize(numVars + 1);
	/* -------------------------------------------------------------------*/
	// Read clauses
	for (uint i = 0; i < numClauses; ++i) {
		int lit;
		while (cin >> lit and lit != 0) {
			clauses[i].push_back(lit);
	/* -------------------------------------------------------------------*/
			if (lit > 0) 	pos_ocurr[lit].push_back(i);
			else 			neg_ocurr[-lit].push_back(i);
	/* -------------------------------------------------------------------*/
		}
	}
	
	/* -------------------------------------------------------------------*/
	usageCounter.resize(numVars + 1);
	for (uint lit = 1; lit < numVars + 1; ++lit)
        usageCounter[lit] = 0;
		//usageCounter[lit] = pos_ocurr[lit].size() + neg_ocurr[lit].size();
    /* -------------------------------------------------------------------*/
}

void changeCounter(int clauseId) {
    
    ++conflictCount;
    if (conflictCount > NUM_DEC) {
        conflictCount = 0;
        for (auto& counter : usageCounter)
            counter /= DIVISOR;
    }
    
    for (const auto lit : clauses[clauseId])
        usageCounter[abs(lit)] += PRIZE;
        
}

int currentValueInModel(int lit){
	if (lit >= 0) return model[lit];
	else {
		if (model[-lit] == UNDEF) return UNDEF;
		else return 1 - model[-lit];
	}
}


void setLiteralToTrue(int lit){
	modelStack.push_back(lit);
	if (lit > 0) model[lit] = TRUE;
	else model[-lit] = FALSE;		
}


bool propagateGivesConflict ( ) {
    
    int mod_lit;
    vector<int>* mod_clauses;
	
	while ( indexOfNextLitToPropagate < modelStack.size() ) {
		
		/* Hay que cambiar este bucle para no ser greedy */
        mod_lit = modelStack[indexOfNextLitToPropagate++];

        if (mod_lit < 0) mod_clauses = &pos_ocurr[-mod_lit];
        else mod_clauses = &neg_ocurr[mod_lit];
        
		for (uint i = 0; i < (*mod_clauses).size(); ++i) {
			bool someLitTrue = false;	// Si algun literal es cierto, esa clausula no puede llevar a un conflico
			int numUndefs = 0;
			int lastLitUndef = 0;
            
            int clause = (*mod_clauses)[i];
            
			for (uint k = 0; not someLitTrue and k < clauses[clause].size(); ++k) { 
				int val = currentValueInModel(clauses[clause][k]);
				if (val == TRUE) someLitTrue = true;
				else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[clause][k]; }
			}
			
			if (not someLitTrue and numUndefs == 0) {
                changeCounter(clause);
                return true;                            // CONFLICTO!! (contradiccion)
			}
                
			else if (not someLitTrue and numUndefs == 1) {
                setLiteralToTrue(lastLitUndef);	// Hemos deducido algo!! (y seguimos adelante)
				++propagationsCount;
			}
		}    
	}
	return false;
}

void backtrack() {
	uint i = modelStack.size() -1;
	int lit = 0;
	while (modelStack[i] != 0){ // 0 is the DL mark
		lit = modelStack[i];
		model[abs(lit)] = UNDEF;
		modelStack.pop_back();
		--i;
	}
	// at this point, lit is the last decision
	modelStack.pop_back(); // remove the DL mark
	--decisionLevel;
	indexOfNextLitToPropagate = modelStack.size();
	setLiteralToTrue(-lit);  // reverse last decision
}

// Heuristic for finding the next decision literal:
int getNextDecisionLiteral() {
    
    double aux, max = -1;
    int lit = 0;
    
    for (uint i = 1; i < usageCounter.size(); ++i)  {
		if (model[i] == UNDEF && (aux = usageCounter[i]) > max) {
            lit = i;
            max = aux;
		}
	}
	
    decisionsCount++;
	return lit;
}

void checkmodel(){
	for (uint i = 0; i < numClauses; ++i){
		bool someTrue = false;
		for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
			someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
		if (not someTrue) {
			cout << "Error in model, clause is not satisfied:";
			for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
			cout << endl;
			exit(1);
		}
	}  
}

void print_info() {
	elapsed_time = double((double(clock()) - elapsed_time) / CLOCKS_PER_SEC);
    cout << "   TIME:\t" << elapsed_time << " s" << endl;
    cout << "   #DECISIONS:\t" << decisionsCount << " decisions" << endl;
    //cout << "PROPAGATIONS/SECOND: " << double(propagationsCount)/elapsed_time << endl;
}

int main() { 

	readClauses(); // reads numVars, numClauses and clauses
	model.resize(numVars+1,UNDEF);
	indexOfNextLitToPropagate = 0;  
	decisionLevel = 0;
	
	// Take care of initial unit clauses, if any
	for (uint i = 0; i < numClauses; ++i) {
		if (clauses[i].size() == 1) {
			int lit = clauses[i][0];
			int val = currentValueInModel(lit);
            
            // Si hay alguna clausula que sea un solo literal y:
            //      - es falso => la formula es insat.
            //      - es undef => lo ponemos como cierto
            
			if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
			else if (val == UNDEF) setLiteralToTrue(lit);
		}
	}

	elapsed_time = clock();

	// DPLL algorithm
	while (true) {
		while ( propagateGivesConflict() ) {
			if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; print_info(); return 10;}
			backtrack();
		}
		int decisionLit = getNextDecisionLiteral();
		if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; print_info(); return 20; }
		// start new decision level:
		modelStack.push_back(0);  // push mark indicating new DL
		++indexOfNextLitToPropagate;
		++decisionLevel;
		setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
	}
}  
