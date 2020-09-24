grammar Capybara;

compileUnit
	: packageDeclaration NEWLINE NEWLINE (code NEWLINE NEWLINE)* EOF
	| packageDeclaration NEWLINE NEWLINE (imports NEWLINE)+ NEWLINE (code NEWLINE NEWLINE)* EOF
//	: packageDeclaration NEWLINE NEWLINE EOF
	;

packageDeclaration
	: PACKAGE_ PACKAGE
	;

imports
	: IMPORT package_=PACKAGE (CURLYL sub_import ( COMMA sub_import )* CURLYR)?
	;

sub_import
	: struct_name=ALPH_NUM_STARTING_WITH_CAPITAL
	| fun_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	;

code
	: struct
	| fun_
	| def_
	| union
	;

struct
	: STRUCT name=ALPH_NUM_STARTING_WITH_CAPITAL CURLYL NEWLINE? field* CURLYR
	;

field
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL COLON type=fullyQualifiedType NEWLINE
	| SPREAD spread_type=fullyQualifiedType NEWLINE
	;

fun_
	: FUN name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ROUNDL listOfParameters? ROUNDR
		(COLON returnType=fullyQualifiedType)? CURLYL NEWLINE funBody+ CURLYR
	| FUN name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ROUNDL listOfParameters? ROUNDR
		(COLON returnType=fullyQualifiedType)? EQUALS NEWLINE* returnExpression=expression
	;

listOfParameters
	: parameter (COMMA parameter)*
	;

parameter
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL COLON type=fullyQualifiedType
	| type=fullyQualifiedType
	;

funBody
	: assigment (SEMICOLON|NEWLINE) funBody
	| RETURN expression (SEMICOLON|NEWLINE)
	;

expression
	: ROUNDL in_parenthisis_expression=expression ROUNDR
	| constant
	| value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| BANG negate_expression=expression
	| function_qualified_name=fully_qualified_function ROUNDL parameters? ROUNDR
	| left=expression infix_operation right=expression
	| IF condition=expression CURLYL NEWLINE true_expression=expression NEWLINE CURLYR ELSE CURLYL NEWLINE false_expression=expression NEWLINE CURLYR
	| condition=expression QUESTION_MARK true_expression=expression COLON false_expression=expression
	| argument_to_function=expression ARROW_SLIM apply_to_function_qualified_name=fully_qualified_function
	| struct_name=fullyQualifiedType CURLYL NEWLINE* struct_field_initializations NEWLINE* CURLYR
	| newListExpression
	| structureAccessExpression
	| is_value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL IS is_type=fullyQualifiedType
	;

struct_field_initializations
	: struct_field_initialization (COMMA NEWLINE* struct_field_initialization)*
	;

newListExpression
	: SQUAREL expression (COMMA expression)*  SQUARER
	| SQUAREL SQUARER
	;

structureAccessExpression
	:	structure_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL SQUAREL structure_index=expression SQUARER
	;

struct_field_initialization
	: field_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL EQUALS field_value=expression
	;

constant
	: INTEGER
	| FLOAT
	| BOOLEAN
	| string=STRING_DOUBLE_QUOTES
	| string=STRING_SINGLE_QUOTES
	| NOTHING
	;

fully_qualified_function
	: (package_=PACKAGE COLON)? function_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	;

parameters : expression (COMMA expression)* ;

infix_operation
	: '^'
	| '*'
	| '/'
	| '~/'
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
	: DEF name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ROUNDL listOfParameters? ROUNDR
      		(COLON returnType=fullyQualifiedType)? CURLYL NEWLINE defBody+ CURLYR
	;

defBody
	: statement ((SEMICOLON|NEWLINE) defBody)?
	| RETURN return_expression=expression (SEMICOLON|NEWLINE)
	;

statement
	: assigment
	| update_assigment
	| while_loop
	| for_loop
	;

while_loop
	: WHILE while_=expression CURLYL NEWLINE* loop_body CURLYR
	;

loop_body
	: (statement next)+
	;

for_loop
	: FOR for_loop_expression CURLYL NEWLINE* loop_body CURLYR
	;

for_loop_expression
	: assigment? SEMICOLON while_=expression SEMICOLON each_iteration=statement?
	;

assigment
	: assign_to=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL EQUALS expression
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

union
	: UNION name=ALPH_NUM_STARTING_WITH_CAPITAL CURLYL NEWLINE? unionField+ CURLYR ;

unionField
	: fullyQualifiedType
	| fullyQualifiedType COMMA
	| fullyQualifiedType NEWLINE
	| fullyQualifiedType COMMA NEWLINE
	;

fullyQualifiedType
	: (type_package=PACKAGE SLASH)? name=ALPH_NUM_STARTING_WITH_CAPITAL (SQUAREL generic_type=fullyQualifiedType SQUARER)?
	;

next
	: SEMICOLON
	| NEWLINE+
	| SEMICOLON NEWLINE+
	;

// Brackets
ROUNDL :  '(' ;
ROUNDR :  ')' ;
CURLYL :  '{' ;
CURLYR :  '}' ;
SQUAREL : '[' ;
SQUARER : ']' ;
ANGLEL : '<' ;
ANGLER : '>' ;

COMMA : ',' ;
COLON : ':' ;
SEMICOLON : ';' ;
ARROW_SLIM : '->' ;
ARROW_FAT : '=>' ;
ARROW_SPREAD : '*->' ;
BANG : '!' ;
EQUALS : '=' ;
QUESTION_MARK : '?' ;
SPREAD : '...' ;
SLASH : '/' ;

IF : 'if' ;
ELSE : 'else' ;
RETURN : 'return' ;
FUN : 'fun' ;
DEF : 'def' ;
WHILE : 'while' ;
FOR : 'for' ;
PACKAGE_ : 'package' ;
IMPORT : 'import' ;
STRUCT : 'struct' ;
UNION : 'union';
IS : 'is' ;

// Types
INTEGER : ('-')?[0-9_]+ ;
FLOAT : ('-')?[0-9_]+ '.' [0-9_]+ ;
BOOLEAN : ('true'|'false') ;
NOTHING : 'nothing' ;
STRING_SINGLE_QUOTES : '"' .+? '"' ;
STRING_DOUBLE_QUOTES : '\'' .+? '\'' ;

PACKAGE : ('/' SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL)+ ;
SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL : [a-z][a-z_0-9]* ;
ALPH_NUM_STARTING_WITH_CAPITAL : [A-Z][a-zA-Z0-9]* ;
NEWLINE : '\r'? '\n';
//WS : [ \t\r]+ -> channel(HIDDEN);
//WS : [ \t]+ -> skip ;
WS : (' '|'\t')+ -> skip ;
COMMENT : ('#'|'//') .+? NEWLINE -> skip ;