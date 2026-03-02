grammar Functional;

@header {
package pl.grzeslowski.capybara.parser.antlr;
}

program : definition+ EOF;

definition: functionDeclaration;

functionDeclaration: 'fun' NAME '(' parameters? ')' functionType? '=' expression;

BOOL_LITERAL: 'true' | 'false';
COLLECTION: 'list' | 'set' | 'dict';
NAME : [a-z][a-zA-Z0-9_]*;
parameters: parameter (',' parameter)*;
parameter: NAME ':' type;
functionType: ':' type;
type: COLLECTION '[' type ']'
    | 'int'
    | 'bool'
    | 'string'
    | TYPE;
TYPE: [A-Z][a-zA-Z0-9]*
      | TYPE_FULL ;
TYPE_FULL: '/' [A-Z][a-zA-Z0-9]* ( '/' [A-Z][a-zA-Z0-9]* )*;
expression: ifExpression
            | functionCall
            | '(' expression ')'
            | expression infixOperator expression
            | value
            | newData;
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

newData: type '{' fieldAssignmentList? '}';
fieldAssignmentList: fieldAssignment (',' fieldAssignment)*;
fieldAssignment: NAME ':' expression
                | '"' NAME '"' ':' expression;

infixOperator: boolInfixOperator
        | PLUS
        | MINUS
        | MUL
        | DIV
        | CARET;

boolInfixOperator: GT
        | LT
        | EQUAL
        | NOTEQUAL
        | LE
        | GE
        | AND
        | OR;

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
