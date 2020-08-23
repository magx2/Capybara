grammar Capybara;

compileUnit
	: packageDeclaration NEWLINE NEWLINE (code NEWLINE NEWLINE)* EOF
	| packageDeclaration NEWLINE NEWLINE (imports NEWLINE)+ NEWLINE (code NEWLINE NEWLINE)* EOF
//	: packageDeclaration NEWLINE NEWLINE EOF
	;

packageDeclaration
	: 'package ' PACKAGE
	;

imports
	: 'import ' package_=PACKAGE (' { ' sub_import ( ', ' sub_import  )* ' }')?
	;

sub_import
	: struct_name=ALPH_NUM_STARTING_WITH_CAPITAL
	| fun_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
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
	| 'fun ' name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' listOfParameters? ')'
		(': ' returnType=fullyQualifiedType)? ' = ' returnExpression=expression
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
	| constant
	| '!' negate_expression=expression
	| value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| function_qualified_name=fully_qualified_function '(' parameters? ')'
	| left=expression ' ' infix_operation ' ' right=expression
	| 'if ' condition=expression ' {' NEWLINE true_expression=expression NEWLINE '} else {' NEWLINE false_expression=expression NEWLINE '}'
	| condition=expression ' ? ' true_expression=expression ' : ' false_expression=expression
	| argument_to_function=expression ' -> ' apply_to_function_qualified_name=fully_qualified_function
	| struct_name=fullyQualifiedType ' { ' struct_field_initializations ' }'
	;
struct_field_initializations
	: struct_field_initialization (', ' struct_field_initialization)*
	;

struct_field_initialization
	: field_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ': ' field_value=expression
	;

constant
	: INTEGER
	| BOOLEAN
	| string=STRING_DOUBLE_QUOTES
	| string=STRING_SINGLE_QUOTES
	;

fully_qualified_function
	: (package_=PACKAGE ':')? function_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	;

parameters : expression (', ' expression)* ;

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
	| '&&'
	| '||'
	| '!='
	| '=='
	;

fullyQualifiedType
	: (type_package=PACKAGE '/')? name=ALPH_NUM_STARTING_WITH_CAPITAL
	;

next
	: ';'
	| NEWLINE
	;

// Types
INTEGER : ('-')?[0-9_]+ ;
BOOLEAN : ('true'|'false') ;
STRING_SINGLE_QUOTES : '"' .+? '"' ;
STRING_DOUBLE_QUOTES : '\'' .+? '\'' ;

PACKAGE : ('/' SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL)+ ;
SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL : [a-z][a-z_0-9]* ;
ALPH_NUM_STARTING_WITH_CAPITAL : [A-Z][a-zA-Z0-9]* ;
//NEXT_EXPRESSION : (';'|NEWLINE) ;
TODO : '~TODO~' ;
NEWLINE : '\r'? '\n' INDENT* ;
INDENT : [ \t]+ ;
//WS : [ \t\r\n]+ ;
COMMENT : ('#'|'//') .+? NEWLINE -> skip ;