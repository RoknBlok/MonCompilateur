//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"


#include <string>
#include <iostream>
#include <cstdlib>
#include <set>
#include <map>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>
#include <vector>
#include <algorithm>

using namespace std;

enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE, WTFR};
enum OPADD {ADD, SUB, OR, WTFA};
enum OPMUL {MUL, DIV, MOD, AND ,WTFM};
enum TYPES {INTEGER, BOOLEAN, CHAR, DOUBLE, STRING, WTFT};

string typeString[] = {
	"INTEGER", "BOOLEAN", "DOUBLE", "CHAR", "STRING"
};

struct Variable {
	string name;
	TYPES type;
};


set<Variable> DeclaredVariables;

bool IsDeclared(string name) {
	return DeclaredVariables.find({name}) != DeclaredVariables.end();
}


bool is_subroutine;


struct Subroutine {
	bool defined;
	TYPES returnType;
	vector<Variable> arguments;
	vector<Variable> local;
};

map<string, Subroutine> DeclaredSubroutines;

// DÉCLARATION
bool IsSubroutineDeclared(string name) {
	return DeclaredSubroutines.find(name) != DeclaredSubroutines.end();
}
// DÉFINITION
bool IsSubroutineDefined(string name) {
	return IsSubroutineDeclared(name) && DeclaredSubroutines[name].defined;
}

bool IsArgument(string name, string functionName) {
	vector<Variable>& v = DeclaredSubroutines[functionName].arguments;
	Variable vv = {name};
	return find(v.begin(), v.end(), vv) != v.end();
}

bool is_subroutine;
string currentSubroutine;

TOKEN current;				// Current token


FlexLexer* lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

	
map<string, enum TYPES> DeclaredVariables;	// Store declared variables and their types
unsigned long long TagNumber=0;


void Error(string s){
	cerr << "Ligne n°"<<lexer->lineno()<<", lu : '"<<lexer->YYText()<<"'("<<current<<"), mais ";
	cerr<< s << endl;
	exit(-1);
}

void ReadKeyword(const string& keyword) {
    if (current != KEYWORD) {
        Error("(ReadKeyword) Erreur: Mot clé attendu!");
    }

    if (keyword != lexer->YYText()) {
        Error("(ReadKeyword) Erreur: Mot clé '" + keyword + "' attendu, trouvé '" + lexer->YYText() + "'!");
    }

    current = (TOKEN)lexer->yylex();
}

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
// Statement := AssignementStatement
// AssignementStatement := Letter "=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"
	
// To avoid cross references problems :
enum TYPES Expression(void);			// Called by Term() and calls Term()
void Statement(void);
void StatementPart(void);
		
enum TYPES Identifier() {
	if (current != ID) {
		Error("(Identifier) Erreur: Identifiant attendu!");
	}
	string id = lexer->YYText();
	current = (TOKEN)lexer->yylex();

	if (is_subroutine) {
		vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
		for (int i = 0; i < localVector.size(); i++) {
			if (localVector[i].name == id) {
				cout << "\tpushq\t" << -8 - 8 * i << "(%rbp)" << endl;
				return localVector[i].type;
			}
		}

		vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
		for (int i = 0; i < argumentsVector.size(); i++) {
			if (argumentsVector[i].name == id) {
				cout << "\tpushq\t" << 16 + 8 * i << "(%rbp)" << endl;
				return argumentsVector[i].type;
			}
		}
	}

	if (current == LPARENT) {
		if (!IsSubroutineDeclared(id)) {
			Error("(Identifier) Erreur: Routine non définie!");
		}
		if (DeclaredSubroutines[id].returnType == WTFT) {
			Error("(Identifier) Erreur: Procédure n'a pas de valeur de retour!");
		}
		TYPES returnType = FunctionCall(id);
		cout << "\tpushq\t%rax" << endl;
		return returnType;
	}

	if (!IsDeclared(id)) {
		Error("(Identifier) Erreur: Variable non déclarée!");
	}

	cout << "\tpushq\t" << id << endl;
	return DeclaredVariables.find({id})->type;
}


enum TYPES Number(void){
	bool is_a_decimal=false;
	double d;					// 64-bit float
	unsigned int *i;			// pointer to a 32 bit unsigned int 
	string	number=lexer->YYText();
	if(number.find(".")!=string::npos){
		// Floating point constant number
		d=atof(lexer->YYText());
		i=(unsigned int *) &d; // i points to the const double
		//cout <<"\tpush $"<<*i<<"\t# Conversion of "<<d<<endl;
		// Is equivalent to : 
		cout <<"\tsubq $8,%rsp\t\t\t# allocate 8 bytes on stack's top"<<endl;
		cout <<"\tmovl	$"<<*i<<", (%rsp)\t# Conversion of "<<d<<" (32 bit high part)"<<endl;
		cout <<"\tmovl	$"<<*(i+1)<<", 4(%rsp)\t# Conversion of "<<d<<" (32 bit low part)"<<endl;
		current=(TOKEN) lexer->yylex();
		return DOUBLE;
	}
	else{ // Integer Constant
		cout <<"\tpush $"<<atoi(lexer->YYText())<<endl;
		current=(TOKEN) lexer->yylex();
		return INTEGER;
	}
	
}

enum TYPES CharConst(void){
	cout<<"\tmovq $0, %rax"<<endl;
	cout<<"\tmovb $"<<lexer->YYText()<<",%al"<<endl;
	cout<<"\tpush %rax\t# push a 64-bit version of "<<lexer->YYText()<<endl;
	current=(TOKEN) lexer->yylex();
	return CHAR;
}

enum TYPES Factor(void){
	enum TYPES type;
	switch(current){
		case RPARENT:
			current=(TOKEN) lexer->yylex();
			type=Expression();
			if(current!=LPARENT)
				Error("')' était attendu");		// ")" expected
			else
				current=(TOKEN) lexer->yylex();
			break;
		case NUMBER:
			type=Number();
			break;
		case ID:
			type=Identifier();
			break;
		case CHARCONST:
			type=CharConst();
			break;
		default:
			Error("'(', ou constante ou variable attendue.");
	};
	return type;
}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void){
	OPMUL opmul;
	if(strcmp(lexer->YYText(),"*")==0)
		opmul=MUL;
	else if(strcmp(lexer->YYText(),"/")==0)
		opmul=DIV;
	else if(strcmp(lexer->YYText(),"%")==0)
		opmul=MOD;
	else if(strcmp(lexer->YYText(),"&&")==0)
		opmul=AND;
	else opmul=WTFM;
	current=(TOKEN) lexer->yylex();
	return opmul;
}


// Term := Factor {MultiplicativeOperator Factor}
enum TYPES Term(void){
	TYPES type1, type2;
	OPMUL mulop;
	type1=Factor();
	while(current==MULOP){
		mulop=MultiplicativeOperator();		// Save operator in local variable
		type2=Factor();
		if(type2!=type1)
			Error("types incompatibles dans l'expression");
		switch(mulop){
			case AND:
				if(type2!=BOOLEAN)
					Error("type non booléen pour l'opérateur AND");
				cout << "\tpop %rbx"<<endl;	// get first operand
				cout << "\tpop %rax"<<endl;	// get second operand
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# AND"<<endl;	// store result
				break;
			case MUL:
				if(type2!=INTEGER&&type2!=DOUBLE)
					Error("type non numérique pour la multiplication");
				if(type2==INTEGER){
					cout << "\tpop %rbx"<<endl;	// get first operand
					cout << "\tpop %rax"<<endl;	// get second operand
					cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
					cout << "\tpush %rax\t# MUL"<<endl;	// store result
				}
				else{
					cout<<"\tfldl	8(%rsp)\t"<<endl;
					cout<<"\tfldl	(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)"<<endl;
					cout<<"\tfmulp	%st(0),%st(1)\t# %st(0) <- op1 + op2 ; %st(1)=null"<<endl;
					cout<<"\tfstpl 8(%rsp)"<<endl;
					cout<<"\taddq	$8, %rsp\t# result on stack's top"<<endl; 
				}
				break;
			case DIV:
				if(type2!=INTEGER&&type2!=DOUBLE)
					Error("type non numérique pour la division");
				if(type2==INTEGER){
					cout << "\tpop %rbx"<<endl;	// get first operand
					cout << "\tpop %rax"<<endl;	// get second operand
					cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
					cout << "\tdiv %rbx"<<endl;			// quotient goes to %rax
					cout << "\tpush %rax\t# DIV"<<endl;		// store result
				}
				else{
					cout<<"\tfldl	(%rsp)\t"<<endl;
					cout<<"\tfldl	8(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)"<<endl;
					cout<<"\tfdivp	%st(0),%st(1)\t# %st(0) <- op1 + op2 ; %st(1)=null"<<endl;
					cout<<"\tfstpl 8(%rsp)"<<endl;
					cout<<"\taddq	$8, %rsp\t# result on stack's top"<<endl; 
				}
				break;
			case MOD:
				if(type2!=INTEGER)
					Error("type non entier pour le modulo");
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// remainder goes to %rdx
				cout << "\tpush %rdx\t# MOD"<<endl;		// store result
				break;
			default:
				Error("opérateur multiplicatif attendu");
		}
	}
	return type1;
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void){
	OPADD opadd;
	if(strcmp(lexer->YYText(),"+")==0)
		opadd=ADD;
	else if(strcmp(lexer->YYText(),"-")==0)
		opadd=SUB;
	else if(strcmp(lexer->YYText(),"||")==0)
		opadd=OR;
	else opadd=WTFA;
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
enum TYPES SimpleExpression(void){
	enum TYPES type1, type2;
	OPADD adop;
	type1=Term();
	while(current==ADDOP){
		adop=AdditiveOperator();		// Save operator in local variable
		type2=Term();
		if(type2!=type1)
			Error("types incompatibles dans l'expression");
		switch(adop){
			case OR:
				if(type2!=BOOLEAN)
					Error("opérande non booléenne pour l'opérateur OR");
				cout << "\tpop %rbx"<<endl;	// get first operand
				cout << "\tpop %rax"<<endl;	// get second operand
				cout << "\torq	%rbx, %rax\t# OR"<<endl;// operand1 OR operand2
				cout << "\tpush %rax"<<endl;			// store result
				break;			
			case ADD:
				if(type2!=INTEGER&&type2!=DOUBLE)
					Error("opérande non numérique pour l'addition");
				if(type2==INTEGER){
					cout << "\tpop %rbx"<<endl;	// get first operand
					cout << "\tpop %rax"<<endl;	// get second operand
					cout << "\taddq	%rbx, %rax\t# ADD"<<endl;	// add both operands
					cout << "\tpush %rax"<<endl;			// store result
				}
				else{
					cout<<"\tfldl	8(%rsp)\t"<<endl;
					cout<<"\tfldl	(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)"<<endl;
					cout<<"\tfaddp	%st(0),%st(1)\t# %st(0) <- op1 + op2 ; %st(1)=null"<<endl;
					cout<<"\tfstpl 8(%rsp)"<<endl;
					cout<<"\taddq	$8, %rsp\t# result on stack's top"<<endl; 
				}
				break;			
			case SUB:	
				if(type2!=INTEGER&&type2!=DOUBLE)
					Error("opérande non numérique pour la soustraction");
				if(type2==INTEGER){
					cout << "\tpop %rbx"<<endl;	// get first operand
					cout << "\tpop %rax"<<endl;	// get second operand
					cout << "\tsubq	%rbx, %rax\t# ADD"<<endl;	// add both operands
					cout << "\tpush %rax"<<endl;			// store result
				}
				else{
					cout<<"\tfldl	(%rsp)\t"<<endl;
					cout<<"\tfldl	8(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)"<<endl;
					cout<<"\tfsubp	%st(0),%st(1)\t# %st(0) <- op1 - op2 ; %st(1)=null"<<endl;
					cout<<"\tfstpl 8(%rsp)"<<endl;
					cout<<"\taddq	$8, %rsp\t# result on stack's top"<<endl; 
				}
				break;	
			default:
				Error("opérateur additif inconnu");
		}
	}
	return type1;
}

enum TYPES Type(void){
	if(current!=KEYWORD)
		Error("type attendu");
	if(strcmp(lexer->YYText(),"BOOLEAN")==0){
		current=(TOKEN) lexer->yylex();
		return BOOLEAN;
	}	
	else if(strcmp(lexer->YYText(),"INTEGER")==0){
		current=(TOKEN) lexer->yylex();
		return INTEGER;
	}
	else if(strcmp(lexer->YYText(),"DOUBLE")==0){
		current=(TOKEN) lexer->yylex();
		return DOUBLE;
	}
	else if(strcmp(lexer->YYText(),"CHAR")==0){
		current=(TOKEN) lexer->yylex();
		return CHAR;
	}
	else if(strcmp(lexer->YYText(),"STRING")==0){
		current=(TOKEN) lexer->yylex();
		return STRING;
	}
	else {
		current=(TOKEN) lexer->yylex();
		return WTFT;
	}
}

// Declaration := Ident {"," Ident} ":" Type
void VarDeclaration(void) {
	if (current != ID) {
		Error("(VarDeclaration) Erreur: Identifiant attendu!");
	}

	// Ensemble de noms des variables déclarées ici.
	set<string> variables;

	// Vérification
	if (IsDeclared(lexer->YYText())) {
		Error("(VarDeclaration) Erreur: Variable `" + string(lexer->YYText()) + "` déjà déclarée!");
	}
	variables.insert(lexer->YYText());
	current = (TOKEN)lexer->yylex();  // reconnaître Identifier

	while(current == COMMA) {
		current = (TOKEN)lexer->yylex();  // reconnaître ","

		// Vérification
		if (IsDeclared(lexer->YYText())) {
			Error("(VarDeclaration) Erreur: Variable `" + string(lexer->YYText()) + "` déjà déclarée!");
		}
		variables.insert(lexer->YYText());
		current = (TOKEN)lexer->yylex();  // reconnaître Identifier
	}

	if (current != COLON) {
		Error("(VarDeclaration) Erreur: Symbole `:` attendu!");
	}

	current = (TOKEN)lexer->yylex();  // reconnaître ":"

	TYPES varType = Type();  // reconnaître Type

	// 1. Déclarer et 2. Créer les variables lues.
	// Toutes les variables sont initialisées avec la valeur 0 (0.0, false).
	switch (varType) {
		case INTEGER:
			// .quad.
			for (string variableName : variables) {
				DeclaredVariables.insert({variableName, INTEGER});  // déclarer
				cout << variableName << ":\n\t.quad 0" << endl;  // créer
			}
			break;
		case BOOLEAN:
			// .byte OU .quad également, on n'est pas à ça près.
			for (string variableName : variables) {
				DeclaredVariables.insert({variableName, BOOLEAN});
				cout << variableName << ":\n\t.quad 0" << endl;
			}
			break;
		case DOUBLE:
			// .double
			for (string variableName : variables) {
				DeclaredVariables.insert({variableName, DOUBLE});
				cout << variableName << ":\n\t.double 0.0" << endl;
			}
			break;
		case CHAR:
			// .byte
			for (string variableName : variables) {
				DeclaredVariables.insert({variableName, CHAR});
				cout << variableName << ":\n\t.byte 0" << endl;
			}
			break;
		case STRING:
			// .quad adresse
			for (string variableName : variables) {
				DeclaredVariables.insert({variableName, STRING});
				cout << variableName << ":\n\t.quad 0" << endl;
			}
			break;
		default:
			Error("(VarDeclaration) Erreur: Type inconnu!");
	}
}

// VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void VarDeclarationPart(void){
	current=(TOKEN) lexer->yylex();
	VarDeclaration();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		VarDeclaration();
	}
	if(current!=DOT)
		Error("'.' attendu");
	current=(TOKEN) lexer->yylex();
}

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(void){
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==")==0)
		oprel=EQU;
	else if(strcmp(lexer->YYText(),"!=")==0)
		oprel=DIFF;
	else if(strcmp(lexer->YYText(),"<")==0)
		oprel=INF;
	else if(strcmp(lexer->YYText(),">")==0)
		oprel=SUP;
	else if(strcmp(lexer->YYText(),"<=")==0)
		oprel=INFE;
	else if(strcmp(lexer->YYText(),">=")==0)
		oprel=SUPE;
	else oprel=WTFR;
	current=(TOKEN) lexer->yylex();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
enum TYPES Expression(void){
	enum TYPES type1, type2;
	unsigned long long tag;
	OPREL oprel;
	type1=SimpleExpression();
	if(current==RELOP){
		tag=++TagNumber;
		oprel=RelationalOperator();
		type2=SimpleExpression();
		if(type2!=type1)
			Error("types incompatibles pour la comparaison");
		if(type1!=DOUBLE){
			cout << "\tpop %rax"<<endl;
			cout << "\tpop %rbx"<<endl;
			cout << "\tcmpq %rax, %rbx"<<endl;
		}
		else{
			cout<<"\tfldl	(%rsp)\t"<<endl;
			cout<<"\tfldl	8(%rsp)\t# first operand -> %st(0) ; second operand -> %st(1)"<<endl;
			cout<<"\t addq $16, %rsp\t# 2x pop nothing"<<endl;
			cout<<"\tfcomip %st(1)\t\t# compare op1 and op2 -> %RFLAGS and pop"<<endl;
			cout<<"\tfaddp %st(1)\t# pop nothing"<<endl;
		}
		switch(oprel){
			case EQU:
				cout << "\tje Vrai"<<tag<<"\t# If equal"<<endl;
				break;
			case DIFF:
				cout << "\tjne Vrai"<<tag<<"\t# If different"<<endl;
				break;
			case SUPE:
				cout << "\tjae Vrai"<<tag<<"\t# If above or equal"<<endl;
				break;
			case INFE:
				cout << "\tjbe Vrai"<<tag<<"\t# If below or equal"<<endl;
				break;
			case INF:
				cout << "\tjb Vrai"<<tag<<"\t# If below"<<endl;
				break;
			case SUP:
				cout << "\tja Vrai"<<tag<<"\t# If above"<<endl;
				break;
			default:
				Error("Opérateur de comparaison inconnu");
		}
		cout << "\tpush $0\t\t# False"<<endl;
		cout << "\tjmp Suite"<<tag<<endl;
		cout << "Vrai"<<tag<<":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True"<<endl;	
		cout << "Suite"<<tag<<":"<<endl;
		return BOOLEAN;
	}
	return type1;
}

// AssignementStatement := Identifier ":=" Expression
void AssignementStatement(void){
	enum TYPES type1, type2;
	string variable;
	if(current!=ID)
		Error("Identificateur attendu");
	if(!IsDeclared(lexer->YYText())){
		cerr << "Erreur : Variable '"<<lexer->YYText()<<"' non déclarée"<<endl;
		exit(-1);
	}
	variable=lexer->YYText();
	type1=DeclaredVariables[variable];
	current=(TOKEN) lexer->yylex();
	if(current!=ASSIGN)
		Error("caractères ':=' attendus");
	current=(TOKEN) lexer->yylex();
	type2=Expression();
	if(type2!=type1){
		cerr<<"Type variable "<<type1<<endl;
		cerr<<"Type Expression "<<type2<<endl;
		Error("types incompatibles dans l'affectation");
	}
	if(type1==CHAR){
		cout << "\tpop %rax"<<endl;
		cout << "\tmovb %al,"<<variable<<endl;
	}
	else
		cout << "\tpop "<<variable<<endl;
}

// DisplayStatement := "DISPLAY" Expression
void DisplayStatement(void){
	enum TYPES type;
	unsigned long long tag=++TagNumber;
	current=(TOKEN) lexer->yylex();
	type=Expression();
	switch(type){
	case INTEGER:
		cout << "\tpop %rsi\t# The value to be displayed"<<endl;
		cout << "\tmovq $FormatString1, %rdi\t# \"%llu\\n\""<<endl;
		cout << "\tmovl	$0, %eax"<<endl;
		cout << "\tcall	printf@PLT"<<endl;
		break;
	case BOOLEAN:
			cout << "\tpop %rdx\t# Zero : False, non-zero : true"<<endl;
			cout << "\tcmpq $0, %rdx"<<endl;
			cout << "\tje False"<<tag<<endl;
			cout << "\tmovq $TrueString, %rdi\t# \"TRUE\\n\""<<endl;
			cout << "\tjmp Next"<<tag<<endl;
			cout << "False"<<tag<<":"<<endl;
			cout << "\tmovq $FalseString, %rdi\t# \"FALSE\\n\""<<endl;
			cout << "Next"<<tag<<":"<<endl;
			cout << "\tcall	puts@PLT"<<endl;
			break;
	case DOUBLE:
			cout << "\tmovsd	(%rsp), %xmm0\t\t# &stack top -> %xmm0"<<endl;
			cout << "\tsubq	$16, %rsp\t\t# allocation for 3 additional doubles"<<endl;
			cout << "\tmovsd %xmm0, 8(%rsp)"<<endl;
			cout << "\tmovq $FormatString2, %rdi\t# \"%lf\\n\""<<endl;
			cout << "\tmovq	$1, %rax"<<endl;
			cout << "\tcall	printf"<<endl;
			cout << "nop"<<endl;
			cout << "\taddq $24, %rsp\t\t\t# pop nothing"<<endl;
			break;
	case CHAR:
			cout<<"\tpop %rsi\t\t\t# get character in the 8 lowest bits of %si"<<endl;
			cout << "\tmovq $FormatString3, %rdi\t# \"%c\\n\""<<endl;
			cout << "\tmovl	$0, %eax"<<endl;
			cout << "\tcall	printf@PLT"<<endl;
			break;
	default:
			Error("DISPLAY ne fonctionne pas pour ce type de donnée.");
		}

}

// ForStatement := "For" ID ":=" Expression ("TO"|"DOWNTO") Expression "DO" Statement
void ForStatement() {
    unsigned long long tag = ++TagNumber;
    
    ReadKeyword("FOR");

    string id = lexer->YYText();  // nom de la variable utilisée lors de l'incrémentation (prochain token)
    bool found = false;
    string position;
    
    // Recherche de l'identifiant dans les arguments et variables locales si dans une sous-routine
    if (is_subroutine) {
        vector<Variable> argumentsVector = DeclaredSubroutines[currentSubroutine].arguments;
        for (int i = 0; i < argumentsVector.size(); i++) {
            if (argumentsVector[i].name == id) {
                position = to_string(16 + 8 * i) + "(%rbp)";
                found = true;
                break;
            }
        }
        if (!found) {
            vector<Variable> localVector = DeclaredSubroutines[currentSubroutine].local;
            for (int i = 0; i < localVector.size(); i++) {
                if (localVector[i].name == id) {
                    position = to_string(-8 - 8 * i) + "(%rbp)";
                    found = true;
                    break;
                }
            }
        }
    }
    
    if (!found) {
        position = id; // Variable globale
    }

    AssignementStatement();  // reconnaître AssignmentStatement

    if (current != KEYWORD) {
        Error("(ForStatement) Erreur: Mot clé attendu!");
    }

    string jump;
    string increment;
    if (strcmp("TO", lexer->YYText()) == 0) {
        jump = "\tja\tEndFor";
        increment = "\taddq\t$1, ";
    } else if (strcmp("DOWNTO", lexer->YYText()) == 0) {
        jump = "\tjb\tEndFor";
        increment = "\tsubq\t$1, ";
    } else {
        Error("(ForStatement) Erreur: Mots clés `TO` ou `DOWNTO` attendus!");
    }
    
    current = (TOKEN)lexer->yylex();

    cout << "For" << tag << ":" << endl;
    cout << "TestFor" << tag << ":" << endl;

    TYPES exprType = Expression();  // reconnaître Expression
    if (exprType != INTEGER) {
        Error("(ForStatement) Erreur: L'incrément doit être entier!");
    }

    cout << "\tpopq\t%rax" << endl;  // %rax contient le résultat
    cout << "\tcmpq\t%rax, " << position << endl;
    cout << jump << tag << endl;

    ReadKeyword("DO");

    Statement();  // reconnaître Statement

    cout << increment << position << endl;  // incrémenter l'entier
    cout << "\tjmp\tTestFor" << tag << endl;
    cout << "EndFor" << tag << ":" << endl;
}


// WhileStatement := "WHILE" Expression "DO" Statement
void WhileStatement(void){
	unsigned long long tag=TagNumber++;
	cout<<"While"<<tag<<":"<<endl;
	current=(TOKEN) lexer->yylex();
	if(Expression()!=BOOLEAN)
		Error("expression booléene attendue");
	cout<<"\tpop %rax\t# Get the result of expression"<<endl;
	cout<<"\tcmpq $0, %rax"<<endl;
	cout<<"\tje EndWhile"<<tag<<"\t# if FALSE, jump out of the loop"<<tag<<endl;
	if(current!=KEYWORD||strcmp(lexer->YYText(), "DO")!=0)
		Error("mot-clé DO attendu");
	current=(TOKEN) lexer->yylex();
	Statement();
	cout<<"\tjmp While"<<tag<<endl;
	cout<<"EndWhile"<<tag<<":"<<endl;
}

// BlockStatement := "BEGIN" Statement {";" Statement} "END"
void BlockStatement(void){
	current=(TOKEN) lexer->yylex();
	Statement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();	// Skip the ";"
		Statement();
	};
	if(current!=KEYWORD||strcmp(lexer->YYText(), "END")!=0)
		Error("mot-clé END attendu");
	current=(TOKEN) lexer->yylex();
}

// IfStatement := "IF" Expression "THEN" Statement ["ELSE" Statement]
void IfStatement(void){
	unsigned long long tag=TagNumber++;
	current=(TOKEN) lexer->yylex();
	if(Expression()!=BOOLEAN)
		Error("le type de l'expression doit être BOOLEAN");
	cout<<"\tpop %rax\t# Get the result of expression"<<endl;
	cout<<"\tcmpq $0, %rax"<<endl;
	cout<<"\tje Else"<<tag<<"\t# if FALSE, jump to Else"<<tag<<endl;
	if(current!=KEYWORD||strcmp(lexer->YYText(),"THEN")!=0)
		Error("mot-clé 'THEN' attendu");
	current=(TOKEN) lexer->yylex();
	Statement();
	cout<<"\tjmp Next"<<tag<<"\t# Do not execute the else statement"<<endl;
	cout<<"Else"<<tag<<":"<<endl; // Might be the same effective adress than Next:
	if(current==KEYWORD&&strcmp(lexer->YYText(),"ELSE")==0){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	cout<<"Next"<<tag<<":"<<endl;
}



// Statement := AssignementStatement|DisplayStatement|....
void Statement(void){
	if(current==KEYWORD){
		if(strcmp(lexer->YYText(),"DISPLAY")==0)
			DisplayStatement();
		else if(strcmp(lexer->YYText(),"IF")==0)
			IfStatement();
		else if(strcmp(lexer->YYText(),"FOR")==0)
			ForStatement();
		else if(strcmp(lexer->YYText(),"WHILE")==0)
			WhileStatement();
		else if(strcmp(lexer->YYText(),"BEGIN")==0)
			BlockStatement();
		else
			Error("mot clé inconnu");
	}
	else
		if(current==ID)
			AssignementStatement();
		else
			Error("instruction attendue");
	 
}

// StatementPart := Statement {";" Statement} "."
void StatementPart(void){
	cout << "\t.align 8"<<endl;	// Alignement on addresses that are a multiple of 8 (64 bits = 8 bytes)
	cout << "\t.text\t\t# The following lines contain the program"<<endl;
	cout << "\t.globl main\t# The main function must be visible from outside"<<endl;
	cout << "main:\t\t\t# The main function body :"<<endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<endl;
	Statement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// Program := [VarDeclarationPart] StatementPart
void Program(void){
	if(current==KEYWORD && strcmp(lexer->YYText(),"VAR")==0)
		VarDeclarationPart();
	StatementPart();	
}

int main(void){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler"<<endl;
	cout << ".data"<<endl;
	cout << "FormatString1:\t.string \"%llu\"\t# used by printf to display 64-bit unsigned integers"<<endl;
	cout << "FormatString2:\t.string \"%lf\"\t# used by printf to display 64-bit floating point numbers"<<endl; 
	cout << "FormatString3:\t.string \"%c\"\t# used by printf to display a 8-bit single character"<<endl; 
	cout << "TrueString:\t.string \"TRUE\"\t# used by printf to display the boolean value TRUE"<<endl; 
	cout << "FalseString:\t.string \"FALSE\"\t# used by printf to display the boolean value FALSE"<<endl; 
	// Let's proceed to the analysis and code production
	current=(TOKEN) lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<endl;
	cout << "\tret\t\t\t# Return from main function"<<endl;
	if(current!=FEOF){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}
		
			





