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
	| native_python
	| union
	;

struct
	: STRUCT name=ALPH_NUM_STARTING_WITH_CAPITAL CURLYL NEWLINE* list_of_fields? NEWLINE* CURLYR
	;

list_of_fields
	: field (commaEnd field)*
	;

field
	: name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL COLON type=fullyQualifiedType
	| SPREAD spread_type=fullyQualifiedType
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
	: assignment (SEMICOLON|NEWLINE) funBody
	| RETURN expression (SEMICOLON|NEWLINE)
	;

expression
	: ROUNDL in_parenthisis_expression=expression ROUNDR
	| constant
	| value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| lambda=lambda_expression
	| structure_expression=expression DOT field_name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL
	| BANG negate_expression=expression
	| function_qualified_name=fully_qualified_function ROUNDL parameters? ROUNDR
	| normal_function_invocation=expression ROUNDL parameters? ROUNDR
	| left=expression infix_operation right=expression
	| IF ROUNDL condition=expression ROUNDR
		CURLYL NEWLINE* assignments_true=assignments NEWLINE*
		if_else*
		CURLYR ELSE CURLYL NEWLINE* assignments_false=assignments NEWLINE* CURLYR
	| condition=expression QUESTION_MARK true_expression=expression COLON false_expression=expression
	| argument_to_function=expression ARROW_SLIM apply_to_function_qualified_name=fully_qualified_function
	| argument_to_function=expression ARROW_SLIM apply_to_function_expression=expression
	| struct_name=fullyQualifiedType CURLYL NEWLINE* struct_field_initializations NEWLINE* CURLYR
	| newListExpression
	| structureAccessExpression
	| is_value=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL IS is_type=fullyQualifiedType
	| ASSERT check_expression=expression COMMA return_expression=expression (COMMA message_expression=expression)?
	;

if_else
	: CURLYR ELSE IF ROUNDL next_condition=expression ROUNDR CURLYL NEWLINE*
		assignments_next_true=assignments NEWLINE*
	;

lambda_expression
	: CURLYL NEWLINE* (listOfParameters ARROW_SLIM NEWLINE*)? lambda_body=assignments NEWLINE* CURLYR
	;

assignments
	: expression
	| (assignment semicolonEnd)+ expression
	;

struct_field_initializations
	: struct_field_initialization (commaEnd struct_field_initialization)*
	;

newListExpression
	: SQUAREL NEWLINE* expression (commaEnd expression)* NEWLINE* SQUARER
	| SQUAREL NEWLINE* SQUARER
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
      		(COLON returnType=fullyQualifiedType)? CURLYL NEWLINE+ defBody CURLYR
	;

defBody
	: (statement semicolonEnd)+
	| RETURN return_expression=expression semicolonEnd?
	| (statement semicolonEnd)+ RETURN return_expression=expression semicolonEnd?
	;

statement
	: assignment
	| update_assignment
	| while_loop
	| for_loop
	| def_call
	| assert_statement
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
	: assignment? SEMICOLON while_=expression SEMICOLON each_iteration=statement?
	;

assignment
	: assign_to=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL (COLON assignment_type=fullyQualifiedType)? EQUALS expression
	;

update_assignment
	: assign_to=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL update_action expression
	;

update_action
	: '+='
	| '-='
	| '*='
	| '^='
	| '/='
	;

def_call
	: def_qualified_name=fully_qualified_function ROUNDL parameters? ROUNDR
	;

assert_statement
	: ASSERT check_expression=expression (COMMA message_expression=expression)?
	;

native_python
	: NATIVE PYTHON name=SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL ROUNDL listOfParameters? ROUNDR
                       		(COLON returnType=fullyQualifiedType)? native_code=NATIVE_CODE
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
	: (type_package=PACKAGE SLASH)? name=ALPH_NUM_STARTING_WITH_CAPITAL (SQUAREL generic_types SQUARER)?
	;

generic_types
	: fullyQualifiedType (commaEnd fullyQualifiedType)*
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
DOT: '.' ;
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
NATIVE : 'native' ;
PYTHON : 'python' ;
NATIVE_CODE_OPEN : '{{{' ;
NATIVE_CODE_CLOSE : '}}}' ;
NATIVE_CODE : NATIVE_CODE_OPEN .+? NATIVE_CODE_CLOSE ;
WHILE : 'while' ;
FOR : 'for' ;
PACKAGE_ : 'package' ;
IMPORT : 'import' ;
STRUCT : 'struct' ;
UNION : 'union';
IS : 'is' ;
ASSERT: 'assert' ;

commaEnd
	: COMMA NEWLINE*
	| COMMA
	| NEWLINE+
	;

semicolonEnd
	: SEMICOLON NEWLINE*
	| SEMICOLON
	| NEWLINE+
	;

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