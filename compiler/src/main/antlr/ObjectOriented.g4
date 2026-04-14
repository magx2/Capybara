grammar ObjectOriented;

@header {
package dev.capylang.parser.antlr;
}

program : definition+ EOF;

definition:
    classDeclaration
    | traitDeclaration
    | interfaceDeclaration;

classDeclaration: docComment* classModifier* 'class' TYPE constructorParameters? inheritanceClause? typeBody;
traitDeclaration: docComment* 'trait' TYPE inheritanceClause? typeBody;
interfaceDeclaration: docComment* 'interface' TYPE inheritanceClause? interfaceBody;

classModifier
    : 'open'
    | 'abstract'
    ;

inheritanceClause: ':' qualifiedType (',' qualifiedType)*;
typeBody: '{' memberDeclaration* '}';
interfaceBody: '{' interfaceMemberDeclaration* '}';
memberDeclaration: fieldDeclaration
                 | methodDeclaration
                 | initBlock;
interfaceMemberDeclaration: interfaceMethodDeclaration;
fieldDeclaration: docComment* visibility? 'field' identifier ':' type ('=' expression)?;
methodDeclaration: docComment* visibility? methodModifier* 'def' identifier '(' parameters? ')' functionType methodBody?;
methodBody: '=' expression
          | statementBlock;
interfaceMethodDeclaration: docComment* visibility? methodModifier* 'def' identifier '(' parameters? ')' functionType;
initBlock: docComment* 'init' statementBlock;
statementBlock: '{' statement* '}';
statement: letStatement
         | returnStatement
         | ifStatement
         | whileStatement
         | doWhileStatement
         | forEachStatement
         | statementBlock;
letStatement: 'let' identifier (':' type)? letBindingOperator expression ';'?;
returnStatement: 'return' expression ';'?;
ifStatement: 'if' expression statementBlock ('else' (ifStatement | statementBlock))?;
whileStatement: 'while' expression statementBlock;
doWhileStatement: 'do' statementBlock 'while' expression ';'?;
forEachStatement: ('for' | 'foreach') identifier (':' type)? 'in' expression statementBlock;

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

parameters: parameter (',' parameter)*;
parameter: identifier ':' type;
functionType: ':' type;

VISIBILITY: 'local';
BOOL_LITERAL: 'true' | 'false';
COLLECTION: 'list' | 'set' | 'dict';
NAME : [_]* [a-z] [a-zA-Z0-9_]*;
identifier: NAME | COLLECTION | 'def' | 'type' | 'byte' | 'int' | 'long' | 'double' | 'bool' | 'string' | 'float' | 'nothing' | 'any' | 'return';
type: COLLECTION '[' type ']'
    | 'tuple' '[' type (COMMA type)+ ']'
    | LPAREN RPAREN FAT_ARROW type
    | LPAREN type (COMMA type)+ RPAREN FAT_ARROW type
    | type FAT_ARROW type
    | 'byte'
    | 'int'
    | 'long'
    | 'double'
    | 'bool'
    | 'string'
    | 'float'
    | 'any'
    | 'data'
    | 'nothing'
    | qualifiedType ('[' type (',' type)* ']')?;

qualifiedType: TYPE (DOT TYPE)*;
TYPE: [_]* [A-Z][a-zA-Z0-9_]* | TYPE_FULL;
TYPE_FULL: '/' [A-Za-z_][a-zA-Z0-9_]* ( '/' [A-Za-z_][a-zA-Z0-9_]* )+;
INFIX_METHOD_LITERAL: '`' ('|l>' | [+\-*/\\^%$#@~!:<>|]+) '`';

expression: letExpression* expressionNoLet;
letExpression: 'let' identifier (':' type)? letBindingOperator expressionNoLet ';'?;
expressionNoPipe: letExpressionNoPipe* expressionNoLetNoPipe;
letExpressionNoPipe: 'let' identifier (':' type)? letBindingOperator expressionNoLet ';'?;
letBindingOperator: ASSIGN | LT MINUS;

expressionNoLet
    : ifExpression
    | lambdaExpression
    | reduceExpression
    | functionReference
    | functionCall
    | new_list
    | new_dict
    | tupleLiteral
    | LPAREN expression RPAREN
    | expressionBlock
    | new_set
    | BANG expressionNoLet
    | BITWISE_NOT expressionNoLet
    | MINUS expressionNoLet
    | expressionNoLet LBRACK indexLiteral RBRACK
    | expressionNoLet LBRACK sliceIndexLiteral? COLON sliceIndexLiteral? RBRACK
    | expressionNoLet LPAREN argumentList? RPAREN
    | expressionNoLet DOT methodIdentifier LPAREN methodArgumentList? RPAREN
    | expressionNoLet INFIX_METHOD_LITERAL expressionNoLet
    | expressionNoLet DOT identifier
    | expressionNoLet infixOperator expressionNoLet
    | value
    | newData
    | constructorData
    | matchExpression
    | thisExpression
    ;

indexLiteral: MINUS? INT_LITERAL;
sliceIndexLiteral: MINUS? INT_LITERAL;

lambdaExpression
    : lambdaArgument FAT_ARROW expressionNoPipe
    | LPAREN (lambdaArgument (COMMA lambdaArgument)*)? RPAREN FAT_ARROW expressionNoPipe
    ;

lambdaArgument: identifier | UNDERSCORE;
reduceExpression: expressionNoLetNoPipe COMMA LPAREN lambdaArgument COMMA lambdaArgument (COMMA lambdaArgument (COMMA lambdaArgument)?)? RPAREN FAT_ARROW expressionNoPipe;

expressionNoLetNoPipe
    : ifExpression
    | lambdaExpression
    | functionReference
    | functionCall
    | new_list
    | new_dict
    | tupleLiteral
    | LPAREN expression RPAREN
    | expressionBlock
    | new_set
    | BANG expressionNoLetNoPipe
    | BITWISE_NOT expressionNoLetNoPipe
    | MINUS expressionNoLetNoPipe
    | expressionNoLetNoPipe LBRACK indexNoPipeLiteral RBRACK
    | expressionNoLetNoPipe LBRACK sliceIndexNoPipeLiteral? COLON sliceIndexNoPipeLiteral? RBRACK
    | expressionNoLetNoPipe LPAREN argumentList? RPAREN
    | expressionNoLetNoPipe DOT methodIdentifier LPAREN methodArgumentList? RPAREN
    | expressionNoLetNoPipe INFIX_METHOD_LITERAL expressionNoLetNoPipe
    | expressionNoLetNoPipe DOT identifier
    | expressionNoLetNoPipe infixOperatorNoPipe expressionNoLetNoPipe
    | value
    | newData
    | constructorData
    | matchExpressionNoPipe
    | thisExpression
    ;

indexNoPipeLiteral: MINUS? INT_LITERAL;
sliceIndexNoPipeLiteral: MINUS? INT_LITERAL;
tupleLiteral: LPAREN expression (COMMA expression)+ RPAREN;

ifExpression: 'if' expression 'then' expression 'else' expression;
functionReference: COLON identifier;
functionCall
    : NAME LPAREN argumentList? RPAREN
    | COLLECTION LPAREN argumentList? RPAREN
    | 'def' LPAREN argumentList? RPAREN
    | 'byte' LPAREN argumentList? RPAREN
    | 'int' LPAREN argumentList? RPAREN
    | 'long' LPAREN argumentList? RPAREN
    | 'double' LPAREN argumentList? RPAREN
    | 'bool' LPAREN argumentList? RPAREN
    | 'string' LPAREN argumentList? RPAREN
    | 'float' LPAREN argumentList? RPAREN
    | 'nothing' LPAREN argumentList? RPAREN
    | 'any' LPAREN argumentList? RPAREN
    | qualifiedType DOT identifier LPAREN argumentList? RPAREN
    ;
value: literal | identifier | qualifiedType;
thisExpression: 'this';
expressionBlock: LBRACE expression RBRACE;
argumentList: expression (COMMA expression)*;
methodArgumentList: methodArgument (COMMA methodArgument)*;
methodArgument: namedMethodArgument | expression;
namedMethodArgument: identifier COLON expression;

literal: BYTE_LITERAL | LONG_LITERAL | DOUBLE_LITERAL | INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL | NOTHING_LITERAL;
BYTE_LITERAL: '0' [xX] [0-9a-fA-F]+;
LONG_LITERAL: [0-9]+ [lL];
DOUBLE_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [dD]?;
FLOAT_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [fF];
INT_LITERAL: [0-9]+;
fragment EXPONENT: [eE] [+\-]? [0-9]+;
STRING_LITERAL
    : '"' (~["\\\r\n] | '\\' .)* '"'
    | '\'' (~['\\\r\n] | '\\' .)* '\''
    ;
NOTHING_LITERAL: '???';

matchExpression: 'match' expression 'with' matchCaseList+;
matchCaseList: matchCase (COMMA matchCase)*;
matchCase: 'case' pattern ((PIPE | COMMA) pattern)* ('when' guard=expression)? MATCH_ARROW body=expression;
matchExpressionNoPipe: 'match' expressionNoPipe 'with' matchCaseNoPipeList+;
matchCaseNoPipeList: matchCaseNoPipe (COMMA matchCaseNoPipe)*;
matchCaseNoPipe: 'case' pattern ((PIPE | COMMA) pattern)* ('when' guard=expressionNoPipe)? MATCH_ARROW body=expressionNoPipe;

pattern
    : TYPE
    | INT_LITERAL
    | LONG_LITERAL
    | BOOL_LITERAL
    | STRING_LITERAL
    | FLOAT_LITERAL
    | typedPattern
    | identifier
    | wildcardPattern
    | constructorPattern
    ;

wildcardPattern: UNDERSCORE NAME?;
typedPattern: patternType (NAME | UNDERSCORE);
patternType
    : COLLECTION '[' type ']'
    | 'tuple' '[' type (COMMA type)+ ']'
    | 'byte'
    | 'int'
    | 'long'
    | 'double'
    | 'bool'
    | 'string'
    | 'float'
    | 'any'
    | 'data'
    | 'nothing'
    | qualifiedType ('[' type (',' type)* ']')?
    ;

constructorPattern: TYPE LBRACE fieldPatternList? RBRACE;
fieldPatternList: pattern (COMMA pattern)*;

newData: type BANG? LBRACE fieldAssignmentList? RBRACE;
constructorData: MUL LBRACE fieldAssignmentList? RBRACE;
new_list: LBRACK (expression (COMMA expression)* COMMA?)? RBRACK;
new_set: LBRACE (expression (COMMA expression)* COMMA?)? RBRACE;
new_dict: LBRACE (dict_entry (COMMA dict_entry)* COMMA? | COLON) RBRACE;
dict_entry: expression COLON expression;
fieldAssignmentList: fieldAssignment (COMMA fieldAssignment)*;
fieldAssignment
    : namedFieldAssignment
    | spreadFieldAssignment
    | positionalFieldAssignment
    ;
namedFieldAssignment: (identifier | STRING_LITERAL) COLON expression;
spreadFieldAssignment: SPREAD expression;
positionalFieldAssignment: expression;

infixOperator
    : PLUS
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
    | PIPE_MINUS
    | PIPE_FLATMAP
    | PIPE_REDUCE
    | PIPE_ANY
    | PIPE_ALL
    | PIPE
    | QUESTION
    | AND
    ;

infixOperatorNoPipe
    : PLUS
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
    ;

methodIdentifier: identifier | 'with' | INFIX_METHOD_LITERAL;
docComment: DOC_COMMENT;

UNDERSCORE: '_';
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
PLUS : '+';
MINUS : '-';
ASSIGN : '=';
GT : '>';
LT : '<';
BANG : '!';
QUESTION : '?';
BITWISE_AND : '.and.';
BITWISE_NAND : '.nand.';
BITWISE_OR : '.or.';
BITWISE_XOR : '.xor.';
BITWISE_NOT : '.not.';
PIPE_MINUS : '|-';
PIPE_FLATMAP : '|*';
PIPE_REDUCE : '|>';
PIPE_ANY : '|any?';
PIPE_ALL : '|all?';
PIPE : '|';
COLON : ':';
EQUAL : '==';
LE : '<=';
GE : '>=';
NOTEQUAL : '!=';
AND : '&';
MUL : '*';
DIV : '/';
POWER: '^';
MOD : '%';
FAT_ARROW : '=>';
MATCH_ARROW : '->';
DOC_COMMENT : '///' ~[\r\n]*;
LINE_COMMENT : '//' ~[\r\n]* -> skip;
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
WS : [ \t\r\n]+ -> skip;
