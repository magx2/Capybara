grammar Functional;

@header {
package pl.grzeslowski.capybara.parser.antlr;
}

program : definition+ EOF;

definition:
    functionDeclaration
    | typeDeclaration
    | dataDeclaration
    | singleDeclaration;

functionDeclaration: 'fun' identifier '(' parameters? ')' functionType? '=' expression;

typeDeclaration: 'type' genericTypeDeclaration '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*
               | 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*;
dataDeclaration: 'data' genericTypeDeclaration '{' fieldDeclarationList? '}'
               | 'data' genericTypeDeclaration '=' '{' fieldDeclarationList? '}';
singleDeclaration: 'single' TYPE;
fieldDeclarationList: fieldDeclaration (',' fieldDeclaration)*;
fieldDeclaration: NAME ':' type
                | STRING_LITERAL ':' type
                | SPREAD TYPE;
genericTypeDeclaration: TYPE ('[' TYPE (',' TYPE)* ']')?;

BOOL_LITERAL: 'true' | 'false';
COLLECTION: 'list' | 'set' | 'dict';
NAME : [a-z][a-zA-Z0-9_]*;
identifier: NAME | 'fun' | 'byte' | 'int' | 'long' | 'double' | 'bool' | 'string' | 'float' | 'nothing';
parameters: parameter (',' parameter)*;
parameter: identifier ':' type;
functionType: ':' type;
type: COLLECTION '[' type ']'
    | LPAREN type (COMMA type)+ RPAREN ARROW type
    | type ARROW type
    | 'byte'
    | 'int'
    | 'long'
    | 'double'
    | 'bool'
    | 'string'
    | 'float'
    | 'nothing'
    | qualifiedType ('[' type (',' type)* ']')?;
qualifiedType: TYPE (DOT TYPE)*;
TYPE: [A-Z][a-zA-Z0-9_]*
      | TYPE_FULL ;
TYPE_FULL: '/' [A-Za-z_][a-zA-Z0-9_]* ( '/' [A-Za-z_][a-zA-Z0-9_]* )+;
expression: letExpression* expressionNoLet;
letExpression: 'let' NAME '=' expressionNoLet ';'?;
expressionNoLet: ifExpression
               | lambdaExpression
               | reduceExpression
               | functionReference
               | functionCall
               | new_list
               | new_dict
               | '(' expression ')'
               | '{' expression '}'
               | new_set
               | BANG expressionNoLet
               | BITWISE_NOT expressionNoLet
               | MINUS expressionNoLet
               | expressionNoLet DOT NAME
               | expressionNoLet infixOperator expressionNoLet
               | value
               | newData
               | matchExpression;
lambdaExpression: identifier FAT_ARROW expressionNoLetNoPipe
                | LPAREN identifier (COMMA identifier)+ RPAREN FAT_ARROW expressionNoLetNoPipe;
reduceExpression: expressionNoLetNoPipe COMMA LPAREN NAME COMMA NAME RPAREN FAT_ARROW expressionNoLetNoPipe;
expressionNoLetNoPipe: ifExpression
                     | functionReference
                     | functionCall
                     | new_list
                     | new_dict
                     | '(' expressionNoLetNoPipe ')'
                     | new_set
                     | BANG expressionNoLetNoPipe
                     | BITWISE_NOT expressionNoLetNoPipe
                     | MINUS expressionNoLetNoPipe
                     | expressionNoLetNoPipe DOT NAME
                     | expressionNoLetNoPipe infixOperatorNoPipe expressionNoLetNoPipe
                     | value
                     | newData
                     | matchExpression;
ifExpression: 'if' expression 'then' expression 'else' expression;
functionReference: COLON identifier;
functionCall: identifier '(' argumentList? ')'
            | TYPE DOT identifier '(' argumentList? ')';
value: literal | identifier;
argumentList: expression (',' expression)*;
literal: BYTE_LITERAL | LONG_LITERAL | DOUBLE_LITERAL | INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL | NOTHING_LITERAL;
BYTE_LITERAL: '0' [xX] [0-9a-fA-F]+;
LONG_LITERAL: [0-9]+ [lL];
DOUBLE_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [dD];
FLOAT_LITERAL: [0-9]+ '.' [0-9]* EXPONENT? [fF]? | [0-9]+ EXPONENT [fF];
INT_LITERAL: [0-9]+;
fragment EXPONENT: [eE] [+\-]? [0-9]+;
STRING_LITERAL: '"' (~["\r\n] | '\\' .)* '"';
NOTHING_LITERAL: '???';

matchExpression: 'match' expression 'with' matchCaseList+;
matchCaseList: matchCase (',' matchCase)*;
matchCase: PIPE pattern FAT_ARROW expression;
pattern: TYPE
        | INT_LITERAL
        | BOOL_LITERAL
        | STRING_LITERAL
        | FLOAT_LITERAL
        | UNDERSCORE // wildcard pattern
        | constructorPattern;
constructorPattern: TYPE '{' fieldPatternList? '}';
fieldPatternList: NAME (',' NAME)*;

newData: type '{' fieldAssignmentList? '}';
new_list: '[' (expression (',' expression)* ','?)? ']';
new_set: '{' (expression (',' expression)* ','?)? '}';
new_dict: '{' dict_entry (',' dict_entry)* ','? '}';
dict_entry: expression ':' expression;
fieldAssignmentList: fieldAssignment (',' fieldAssignment)*;
fieldAssignment: NAME ':' expression
                | STRING_LITERAL ':' expression
                | SPREAD expression;

infixOperator: PLUS
             | MINUS
             | MUL
             | DIV
             | MOD
             | POWER
             | BITWISE_AND
             | BITWISE_NAND
             | BITWISE_OR
             | BITWISE_XOR
             | GT
             | LT
             | EQUAL
             | NOTEQUAL
             | LE
             | GE
             | PIPE
             | PIPE_MINUS
             | PIPE_FLATMAP
             | PIPE_REDUCE
             | QUESTION
             | AND
             | PIPE;
infixOperatorNoPipe: PLUS
                   | MINUS
                   | MUL
                   | DIV
                   | MOD
                   | POWER
                   | BITWISE_AND
                   | BITWISE_NAND
                   | BITWISE_OR
                   | BITWISE_XOR
                   | GT
                   | LT
                   | EQUAL
                   | NOTEQUAL
                   | LE
                   | GE
                   | QUESTION
                   | AND
                   | PIPE;

UNDERSCORE: '_';

// Separators

LPAREN : '(';
RPAREN : ')';
LBRACE : '{';
RBRACE : '}';
LBRACK : '[';
RBRACK : ']';
SEMI : ';';
COMMA : ',';
DOT : '.';
SPREAD : '...';

// Operators
PLUS : '+';
MINUS : '-';
ASSIGN : '=';
GT : '>';
LT : '<';
BANG : '!';
TILDE : '~';
QUESTION : '?';
BITWISE_AND : '.and.';
BITWISE_NAND : '.nand.';
BITWISE_OR : '.or.';
BITWISE_XOR : '.xor.';
BITWISE_NOT : '.not.';
PIPE_MINUS : '|-';
PIPE_FLATMAP : '|*';
PIPE_REDUCE : '|>';
PIPE : '|';
COLON : ':';
EQUAL : '==';
LE : '<=';
GE : '>=';
NOTEQUAL : '!=';
AND : '&';
INC : '++';
DEC : '--';
MUL : '*';
DIV : '/';
POWER: '^';
MOD : '%';
ARROW : '->';
FAT_ARROW : '=>';
COLONCOLON : '::';

ADD_ASSIGN : '+=';
SUB_ASSIGN : '-=';
MUL_ASSIGN : '*=';
DIV_ASSIGN : '/=';
AND_ASSIGN : '&=';
OR_ASSIGN : '|=';
XOR_ASSIGN : '^=';
MOD_ASSIGN : '%=';
LSHIFT_ASSIGN : '<<=';
RSHIFT_ASSIGN : '>>=';
URSHIFT_ASSIGN : '>>>=';

LINE_COMMENT : '//' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip;
