grammar Capybara;

compileUnit
	: packageDeclaration NEWLINE NEWLINE (code NEWLINE NEWLINE)* EOF
//	: packageDeclaration NEWLINE NEWLINE EOF
	;

packageDeclaration
	: 'package ' PACKAGE
	;

code
	: struct
	| def_
	;

struct
	: 'struct ' name=ALPH_NUM_STARTING_WITH_CAPITAL ' {' NEWLINE field* '}'
	;

field
	: INDENT name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ': ' type=fullyQualifiedType NEWLINE
	| INDENT '...' type=fullyQualifiedType NEWLINE
	;

def_
	: 'def ' name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' listOfParameters? ')' (': ' returnType=fullyQualifiedType)? ' {' NEWLINE defBody+ '}'
	;

listOfParameters
	: parameter (', ' parameter)*
	;

parameter
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ': ' type=fullyQualifiedType
	| type=fullyQualifiedType
	;

defBody
	: INDENT SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ' = ' expression (';'|NEWLINE)
	| INDENT 'return ' expression (';'|NEWLINE)
	;

expression
	: SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| expression op='^' expression
	| expression op=('*'|'/') expression
	| expression op=('+'|'-') expression
	| '(' expression ')'
	;

fullyQualifiedType
	: (PACKAGE '/')? ALPH_NUM_STARTING_WITH_CAPITAL
	;

PACKAGE : ('/' SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL)+ ;
SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL : [a-z_][a-z_0-9]* ;
ALPH_NUM_STARTING_WITH_CAPITAL : [A-Z][a-zA-Z0-9]* ;
//NEXT_EXPRESSION : (';'|NEWLINE) ;
TODO : '~TODO~' ;
NEWLINE : '\r'? '\n' ;
INDENT : [ \t]+ ;
//WS : [ \t\r\n]+ ;
COMMENT : ('#'|'//') .+? NEWLINE -> skip ;