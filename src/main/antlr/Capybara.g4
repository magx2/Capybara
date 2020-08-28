grammar Capybara;

compileUnit
	: packageDeclaration NEWLINE NEWLINE (code NEWLINE NEWLINE)* EOF
	| packageDeclaration NEWLINE NEWLINE (imports NEWLINE)+ NEWLINE (code NEWLINE NEWLINE)* EOF
//	: packageDeclaration NEWLINE NEWLINE EOF
	;

packageDeclaration
	: 'package' PACKAGE
	;

imports
	: 'import' package_=PACKAGE (' {' sub_import ( ', ' sub_import )* '}')?
	;

sub_import
	: struct_name=ALPH_NUM_STARTING_WITH_CAPITAL
	| fun_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	;

code
	: struct
	| fun_
	| def_
	;

struct
	: 'struct' name=ALPH_NUM_STARTING_WITH_CAPITAL ' {' NEWLINE field* '}'
	;

field
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ':' type=fullyQualifiedType NEWLINE
	| '...' spread_type=fullyQualifiedType NEWLINE
	;

fun_
	: 'fun' name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' listOfParameters? ')'
		(':' returnType=fullyQualifiedType)? ' {' NEWLINE funBody+ '}'
	| 'fun' name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' listOfParameters? ')'
		(':' returnType=fullyQualifiedType)? '=' returnExpression=expression
	;

listOfParameters
	: parameter (', ' parameter)*
	;

parameter
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ':' type=fullyQualifiedType
	| type=fullyQualifiedType
	;

funBody
	: assigment (';'|NEWLINE) funBody
	| 'return' expression (';'|NEWLINE)
	;

expression
	: '(' in_parenthisis_expression=expression ')'
	| constant
	| value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| '!' negate_expression=expression
	| function_qualified_name=fully_qualified_function '(' parameters? ')'
	| left=expression infix_operation right=expression
	| 'if' condition=expression '{' NEWLINE true_expression=expression NEWLINE '}' 'else' '{' NEWLINE false_expression=expression NEWLINE '}'
	| condition=expression '?' true_expression=expression ':' false_expression=expression
	| argument_to_function=expression '->' apply_to_function_qualified_name=fully_qualified_function
	| struct_name=fullyQualifiedType ' {' struct_field_initializations '}'
	| newListExpression
	;

struct_field_initializations
	: struct_field_initialization (', ' struct_field_initialization)*
	;

newListExpression
	: '[' expression (', ' expression)*  ']'
	| '[]'
	;

struct_field_initialization
	: field_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ':' field_value=expression
	;

constant
	: INTEGER
	| BOOLEAN
	| string=STRING_DOUBLE_QUOTES
	| string=STRING_SINGLE_QUOTES
	| VOID
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

def_
	: 'def' name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '(' listOfParameters? ')'
      		(':' returnType=fullyQualifiedType)? ' {' NEWLINE defBody+ '}'
	;

defBody
	: statement ((';'|NEWLINE) defBody)?
	| 'return' return_expression=expression (';'|NEWLINE)
	;

statement
	: assigment
	| update_assigment
	| while_loop
	| for_loop
	;

while_loop
	: 'while ' while_=expression     ' {' NEWLINE* loop_body '}'
	;

loop_body
	: (statement next)+
	;

for_loop
	: 'for ' for_loop_expression ' {' NEWLINE* loop_body '}'
	;

for_loop_expression
	: assigment? ';' while_=expression ';' each_iteration=statement?
	;

assigment
	: assign_to=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL '=' expression
	;

update_assigment
	: assign_to=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL update_action expression
	;

update_action
	: '+='
	| '-='
	| '*='
	| '^='
	| '/='
	;

fullyQualifiedType
	: (type_package=PACKAGE '/')? name=ALPH_NUM_STARTING_WITH_CAPITAL ('[' generic_type=fullyQualifiedType']')?
	;

next
	: ';'
	| NEWLINE+
	| ';' NEWLINE+
	;

// Types
INTEGER : ('-')?[0-9_]+ ;
BOOLEAN : ('true'|'false') ;
VOID : 'void' ;
STRING_SINGLE_QUOTES : '"' .+? '"' ;
STRING_DOUBLE_QUOTES : '\'' .+? '\'' ;

PACKAGE : ('/' SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL)+ ;
SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL : [a-z][a-z_0-9]* ;
ALPH_NUM_STARTING_WITH_CAPITAL : [A-Z][a-zA-Z0-9]* ;
NEWLINE : '\r'? '\n';
//WS : [ \t\r]+ -> channel(HIDDEN);
//WS : [ \t]+ -> skip ;
WS : (' '|'\t') -> skip ;
COMMENT : ('#'|'//') .+? NEWLINE -> skip ;