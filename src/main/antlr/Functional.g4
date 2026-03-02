grammar Functional;

@header {
package pl.grzeslowski.capybara;
}

program : definition+ EOF;

definition: functionDeclaration;

functionDeclaration: 'fun' NAME '(' parameters? ')' ':' type '=' expression;

BOOL_LITERAL: 'true' | 'false';
NAME : [a-z][a-zA-Z0-9_]*;
parameters: parameter (',' parameter)*;
parameter: NAME ':' type;
type: NAME
    | 'int'
    | 'bool'
    | 'string';
expression: ifExpression
            | functionCall
            | '(' expression ')'
            | expression infixOperator expression
            | value;
boolExperssion: expression boolInfixOperator expression
            | functionCall
            | '(' boolExperssion ')'
            | BOOL_LITERAL;
ifExpression: 'if' boolExperssion 'then' expression 'else' expression;
functionCall: NAME '(' argumentList? ')';
value: literal | NAME;
argumentList: expression (',' expression)*;
literal: INT_LITERAL | BOOL_LITERAL | STRING_LITERAL;
INT_LITERAL: [0-9]+;
STRING_LITERAL: '"' (~["\r\n] | '\\' .)* '"';

infixOperator: GT
        | LT
        | PLUS
        | MINUS
        | MUL
        | DIV
        | AND
        | OR
        | EQUAL
        | NOTEQUAL
        | LE
        | GE
        | POW;

boolInfixOperator: GT
        | LT
        | EQUAL
        | NOTEQUAL
        | LE
        | GE;

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

// Operators
PLUS : '+';
MINUS : '-';
ASSIGN : '=';
POW : '^';
GT : '>';
LT : '<';
BANG : '!';
TILDE : '~';
QUESTION : '?';
COLON : ':';
EQUAL : '==';
LE : '<=';
GE : '>=';
NOTEQUAL : '!=';
AND : '&&';
OR : '||';
INC : '++';
DEC : '--';
ADD : '+';
SUB : '-';
MUL : '*';
DIV : '/';
BITAND : '&';
BITOR : '|';
CARET : '^';
MOD : '%';
ARROW : '->';
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

WS : [ \t\r\n]+ -> skip;
