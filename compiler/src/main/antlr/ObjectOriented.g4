grammar ObjectOriented;

@header {
package dev.capylang.parser.antlr;
}

program : definition+ EOF;

definition
    : classDeclaration
    | traitDeclaration
    | interfaceDeclaration
    ;

classDeclaration
    : docComment* classModifier* 'class' TYPE constructorParameters? inheritance? typeBody
    ;

traitDeclaration
    : docComment* 'trait' TYPE inheritance? typeBody
    ;

interfaceDeclaration
    : docComment* 'interface' TYPE interfaceInheritance? interfaceBody
    ;

classModifier
    : 'open'
    | 'abstract'
    ;

inheritance
    : ':' typeReference (',' typeReference)*
    ;

interfaceInheritance
    : ':' typeReference (',' typeReference)*
    ;

typeBody
    : '{' member* '}'
    ;

interfaceBody
    : '{' interfaceMember* '}'
    ;

member
    : fieldDeclaration
    | methodDeclaration
    | initBlock
    ;

interfaceMember
    : interfaceMethodDeclaration
    ;

fieldDeclaration
    : visibility? 'field' NAME ':' type ('=' expression)?
    ;

methodDeclaration
    : visibility? methodModifier* 'fun' NAME '(' parameters? ')' ':' type ('=' expression)?
    ;

interfaceMethodDeclaration
    : visibility? methodModifier* 'fun' NAME '(' parameters? ')' ':' type
    ;

initBlock
    : 'init' block
    ;

methodModifier
    : 'open'
    | 'abstract'
    | 'override'
    | 'final'
    ;

visibility
    : 'public'
    | 'local'
    | 'private'
    ;

constructorParameters
    : '(' parameters? ')'
    ;

parameters
    : parameter (',' parameter)*
    ;

parameter
    : NAME ':' type
    ;

type
    : 'list' '[' type ']'
    | 'set' '[' type ']'
    | 'dict' '[' type ']'
    | 'tuple' '[' type (',' type)+ ']'
    | '(' ')' FAT_ARROW type
    | '(' type (',' type)+ ')' FAT_ARROW type
    | type FAT_ARROW type
    | primitiveType
    | typeReference ('[' type (',' type)* ']')?
    ;

primitiveType
    : 'byte'
    | 'int'
    | 'long'
    | 'double'
    | 'bool'
    | 'string'
    | 'float'
    | 'any'
    | 'nothing'
    ;

typeReference
    : TYPE (DOT TYPE)*
    ;

expression
    : ifExpression
    | additiveExpression
    ;

ifExpression
    : 'if' expression 'then' expression 'else' expression
    ;

additiveExpression
    : postfixExpression ((PLUS | MINUS | MUL | DIV | EQ | NEQ | GT | GTE | LT | LTE) postfixExpression)*
    ;

postfixExpression
    : primaryExpression postfixSuffix*
    ;

postfixSuffix
    : '.' NAME '(' argumentList? ')'
    | '.' TYPE '(' argumentList? ')'
    | '.' NAME
    | '(' argumentList? ')'
    ;

primaryExpression
    : literal
    | NAME
    | typeReference
    | 'this'
    | superCall
    | '(' expression ')'
    | block
    ;

superCall
    : 'super' '[' typeReference ']' '.' NAME '(' argumentList? ')'
    ;

block
    : '{' expression '}'
    ;

argumentList
    : expression (',' expression)*
    ;

literal
    : BYTE_LITERAL
    | LONG_LITERAL
    | DOUBLE_LITERAL
    | FLOAT_LITERAL
    | INT_LITERAL
    | BOOL_LITERAL
    | STRING_LITERAL
    ;

docComment
    : DOC_COMMENT
    ;

BOOL_LITERAL: 'true' | 'false';
NAME : [_]* [a-z] [a-zA-Z0-9_]*;
TYPE: [_]* [A-Z][a-zA-Z0-9_]* | TYPE_FULL;
TYPE_FULL: '/' [A-Za-z_][a-zA-Z0-9_]* ( '/' [A-Za-z_][a-zA-Z0-9_]* )+;
BYTE_LITERAL: '0' [xX] [0-9a-fA-F]+;
LONG_LITERAL: [0-9]+ [lL];
DOUBLE_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [dD];
FLOAT_LITERAL: [0-9]+ '.' [0-9]* EXPONENT? [fF]? | [0-9]+ EXPONENT [fF];
INT_LITERAL: [0-9]+;
fragment EXPONENT: [eE] [+\-]? [0-9]+;
STRING_LITERAL
    : '"' (~["\\\r\n] | '\\' .)* '"'
    | '\'' (~['\\\r\n] | '\\' .)* '\''
    ;
DOC_COMMENT: '///' ~[\r\n]*;
FAT_ARROW: '=>';
PLUS: '+';
MINUS: '-';
MUL: '*';
DIV: '/';
EQ: '==';
NEQ: '!=';
GTE: '>=';
LTE: '<=';
GT: '>';
LT: '<';
DOT: '.';
WS: [ \t\r\n]+ -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;
