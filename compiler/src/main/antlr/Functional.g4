grammar Functional;

@header {
package pl.grzeslowski.capybara.parser.antlr;
}

program : definition+ EOF;

definition:
    functionDeclaration
    | typeDeclaration
    | dataDeclaration;

functionDeclaration: 'fun' NAME '(' parameters? ')' functionType? '=' expression;

typeDeclaration: 'type' TYPE '=' TYPE ('|' TYPE)*
               | 'type' TYPE '{' fieldDeclarationList? '}' '=' TYPE ('|' TYPE)*;
dataDeclaration: 'data' TYPE '{' fieldDeclarationList? '}';
fieldDeclarationList: fieldDeclaration (',' fieldDeclaration)*;
fieldDeclaration: NAME ':' type
                | '"' NAME '"' ':' type;

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
    | 'float'
    | TYPE;
TYPE: [A-Z][a-zA-Z0-9]*
      | TYPE_FULL ;
TYPE_FULL: '/' [A-Z][a-zA-Z0-9]* ( '/' [A-Z][a-zA-Z0-9]* )*;
expression: letExpression* expressionNoLet;
letExpression: 'let' NAME '=' expressionNoLet ';'?;
expressionNoLet: ifExpression
               | functionCall
               | new_list
               | new_dict
               | '(' expression ')'
               | '{' expression '}'
               | new_set
               | expressionNoLet infixOperator expressionNoLet
               | value
               | newData
               | matchExpression;
ifExpression: 'if' expression 'then' expression 'else' expression;
functionCall: NAME '(' argumentList? ')';
value: literal | NAME;
argumentList: expression (',' expression)*;
literal: INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL;
INT_LITERAL: [0-9]+;
FLOAT_LITERAL: [0-9]+ '.' [0-9]+;
STRING_LITERAL: '"' (~["\r\n] | '\\' .)* '"';

matchExpression: 'match' expression 'with' matchCaseList+;
matchCaseList: matchCase (',' matchCase)*;
matchCase: '|' pattern '=>' expression;
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
                | '"' NAME '"' ':' expression;

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
POWER: '**';
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

LINE_COMMENT : '//' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip;
