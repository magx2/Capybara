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
	| fun_
	;

struct
	: 'struct ' name=ALPH_NUM_STARTING_WITH_CAPITAL ' {' NEWLINE field* '}'
	;

field
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ': ' type=fullyQualifiedType NEWLINE
	| '...' spread_type=fullyQualifiedType NEWLINE
	;

fun_
	: 'fun ' name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' listOfParameters? ')'
		(': ' returnType=fullyQualifiedType)? ' {' NEWLINE funBody+ '}'
	;

listOfParameters
	: parameter (', ' parameter)*
	;

parameter
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ': ' type=fullyQualifiedType
	| type=fullyQualifiedType
	;

funBody
	: assign_to=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ' = ' expression (';'|NEWLINE) funBody
	| 'return ' expression (';'|NEWLINE)
	;

expression
	: '(' in_parenthisis_expression=expression ')'
	| value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| constant
	| function_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' parameters? ')'
	| left=expression ' ' infix_operation ' ' right=expression
//	| 'if ' condition=expression ' {' NEWLINE true_expression=expression NEWLINE '} else {' NEWLINE false_expression=expression NEWLINE '}'
//	| condition=expression ' ? ' true_expression=expression ' : ' false_expression=expression
//	| argument_to_function=expression ' -> ' apply_to_function_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	;

constant
	: INTEGER
	| BOOLEAN
	| '\'' string_value=.+? '\''
	;

parameters
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL (', ' rest=parameters)?
	| expression (', ' rest=parameters)?
	;

infix_operation
	: '^'
	| '*'
	| '/'
	| '+'
	| '-'
	| '>'
	| '<'
	| '>='
	| '<='
	| '~='
	| '=='
	;

fullyQualifiedType
	: (PACKAGE '/')? ALPH_NUM_STARTING_WITH_CAPITAL
	;

// Types
INTEGER : ('-')?[0-9_]+ ;
BOOLEAN
	: 'true'
	| 'false'
	;

PACKAGE : ('/' SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL)+ ;
SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL : [a-z][a-z_0-9]* ;
ALPH_NUM_STARTING_WITH_CAPITAL : [A-Z][a-zA-Z0-9]* ;
//NEXT_EXPRESSION : (';'|NEWLINE) ;
TODO : '~TODO~' ;
NEWLINE : '\r'? '\n' INDENT* ;
INDENT : [ \t]+ ;
//WS : [ \t\r\n]+ ;
COMMENT : ('#'|'//') .+? NEWLINE -> skip ;