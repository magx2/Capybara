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

functionDeclaration: 'fun' NAME '(' parameters? ')' functionType? '=' expression;

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
identifier: NAME | 'fun';
parameters: parameter (',' parameter)*;
parameter: identifier ':' type;
functionType: ':' type;
type: COLLECTION '[' type ']'
    | LPAREN type (COMMA type)+ RPAREN ARROW type
    | type ARROW type
    | 'int'
    | 'bool'
    | 'string'
    | 'float'
    | TYPE ('[' type (',' type)* ']')?;
TYPE: [A-Z][a-zA-Z0-9]*
      | TYPE_FULL ;
TYPE_FULL: '/' [A-Z][a-zA-Z0-9]* ( '/' [A-Z][a-zA-Z0-9]* )*;
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
literal: INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL;
INT_LITERAL: [0-9]+;
FLOAT_LITERAL: [0-9]+ '.' [0-9]+;
STRING_LITERAL: '"' (~["\r\n] | '\\' .)* '"';

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
             | CARET
             | POWER
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
             | OR;
infixOperatorNoPipe: PLUS
                   | MINUS
                   | MUL
                   | DIV
                   | CARET
                   | POWER
                   | GT
                   | LT
                   | EQUAL
                   | NOTEQUAL
                   | LE
                   | GE
                   | QUESTION
                   | AND
                   | OR;

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
PIPE_MINUS : '|-';
PIPE_FLATMAP : '|*';
PIPE_REDUCE : '|>';
PIPE : '|';
COLON : ':';
EQUAL : '==';
LE : '<=';
GE : '>=';
NOTEQUAL : '!=';
AND : '&&';
OR : '||';
INC : '++';
DEC : '--';
MUL : '*';
DIV : '/';
BITAND : '&';
CARET : '^';
POWER: '**';
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
