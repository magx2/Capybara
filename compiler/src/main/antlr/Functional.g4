grammar Functional;

@header {
package dev.capylang.parser.antlr;
}

program : definition+ EOF;

definition:
    functionDeclaration
    | typeDeclaration
    | enumDeclaration
    | dataDeclaration
    | singleDeclaration
    | constDeclaration;

functionDeclaration: docComment* VISIBILITY? 'fun' functionNameDeclaration '(' parameters? ')' functionType? '=' functionBody;
functionBody: expression
            | localDefinition+ '---' expression;
localDefinition: localFunctionDeclaration
               | localTypeDeclaration
               | localDataDeclaration
               | localConstDeclaration;
localFunctionDeclaration: docComment* 'fun' NAME '(' parameters? ')' functionType? '=' expression;
localTypeDeclaration: 'type' genericTypeDeclaration constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*
                    | 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*;
localDataDeclaration: 'data' genericTypeDeclaration '{' fieldDeclarationList? '}' constructorClause?
                    | 'data' genericTypeDeclaration '=' '{' fieldDeclarationList? '}' constructorClause?;
localConstDeclaration: docComment* 'const' privateLocalConstName (':' type)? '=' expressionNoLet;
privateLocalConstName: NAME | TYPE;
functionNameDeclaration: identifier | genericTypeDeclaration DOT methodIdentifier;
methodIdentifier: identifier | 'with' | INFIX_METHOD_LITERAL;
docComment: DOC_COMMENT;

typeDeclaration: docComment* VISIBILITY? 'type' genericTypeDeclaration constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*
               | docComment* VISIBILITY? 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*;
enumDeclaration: 'enum' TYPE '{' TYPE (COMMA TYPE)* COMMA? '}';
dataDeclaration: docComment* VISIBILITY? 'data' genericTypeDeclaration '{' fieldDeclarationList? '}' constructorClause?
               | docComment* VISIBILITY? 'data' genericTypeDeclaration '=' '{' fieldDeclarationList? '}' constructorClause?;
constructorClause: 'with' 'constructor' '{' expression '}';
singleDeclaration: 'single' TYPE;
constDeclaration: docComment* VISIBILITY? 'const' TYPE (':' type)? '=' expressionNoLet;
fieldDeclarationList: fieldDeclaration (',' fieldDeclaration)* ','?;
fieldDeclaration: identifier ':' type
                | STRING_LITERAL ':' type
                | SPREAD TYPE;
genericTypeDeclaration: TYPE ('[' TYPE (',' TYPE)* ']')?;

VISIBILITY: 'local' | 'private';
BOOL_LITERAL: 'true' | 'false';
COLLECTION: 'list' | 'set' | 'dict';
NAME : [_]* [a-z] [a-zA-Z0-9_]*;
identifier: NAME | COLLECTION | 'fun' | 'type' | 'byte' | 'int' | 'long' | 'double' | 'bool' | 'string' | 'float' | 'nothing' | 'any';
parameters: parameter (',' parameter)*;
parameter: identifier ':' type;
functionType: ':' type;
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
TYPE: [_]* [A-Z][a-zA-Z0-9_]*
      | TYPE_FULL ;
TYPE_FULL: '/' [A-Za-z_][a-zA-Z0-9_]* ( '/' [A-Za-z_][a-zA-Z0-9_]* )+;
INFIX_METHOD_LITERAL: '`' ('|l>' | [+\-*/\\^%$#@~!:<>|?]+) '`';
expression: letExpression* expressionNoLet;
letExpression: 'let' identifier (':' type)? letBindingOperator expressionNoLet ';'?;
expressionNoPipe: letExpressionNoPipe* expressionNoLetNoPipe;
letExpressionNoPipe: 'let' identifier (':' type)? letBindingOperator expressionNoLet ';'?;
letBindingOperator: ASSIGN | LT MINUS;
expressionNoLet: ifExpression
               | lambdaExpression
               | reduceExpression
               | functionReference
               | functionCall
               | new_list
               | new_dict
               | tupleLiteral
               | '(' expression ')'
               | '{' expression '}'
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
               | matchExpression;
indexLiteral: MINUS? INT_LITERAL;
sliceIndexLiteral: MINUS? INT_LITERAL;
lambdaExpression: lambdaArgument FAT_ARROW expressionNoPipe
                | LPAREN (lambdaArgument (COMMA lambdaArgument)*)? RPAREN FAT_ARROW expressionNoPipe;
lambdaArgument: identifier | UNDERSCORE;
reduceExpression: expressionNoLetNoPipe COMMA LPAREN lambdaArgument COMMA lambdaArgument (COMMA lambdaArgument (COMMA lambdaArgument)?)? RPAREN FAT_ARROW expressionNoPipe;
expressionNoLetNoPipe: ifExpression
                     | lambdaExpression
                     | functionReference
                     | functionCall
                     | new_list
                     | new_dict
                     | tupleLiteral
                     | '(' expression ')'
                     | '{' expression '}'
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
                     | matchExpressionNoPipe;
indexNoPipeLiteral: MINUS? INT_LITERAL;
sliceIndexNoPipeLiteral: MINUS? INT_LITERAL;
tupleLiteral: LPAREN expression (COMMA expression)+ RPAREN;

ifExpression: 'if' expression 'then' expression 'else' expression;
functionReference: COLON identifier;
functionCall: NAME '(' argumentList? ')'
            | COLLECTION '(' argumentList? ')'
            | 'fun' '(' argumentList? ')'
            | 'byte' '(' argumentList? ')'
            | 'int' '(' argumentList? ')'
            | 'long' '(' argumentList? ')'
            | 'double' '(' argumentList? ')'
            | 'bool' '(' argumentList? ')'
            | 'string' '(' argumentList? ')'
            | 'float' '(' argumentList? ')'
            | 'nothing' '(' argumentList? ')'
            | 'any' '(' argumentList? ')'
            | TYPE DOT identifier '(' argumentList? ')';
value: literal | identifier | qualifiedType;
argumentList: expression (',' expression)*;
methodArgumentList: methodArgument (',' methodArgument)*;
methodArgument: namedMethodArgument | expression;
namedMethodArgument: identifier COLON expression;
literal: BYTE_LITERAL | LONG_LITERAL | DOUBLE_LITERAL | INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL | NOTHING_LITERAL | REGEX_LITERAL;
BYTE_LITERAL: '0' [xX] [0-9a-fA-F]+;
LONG_LITERAL: DECIMAL_DIGITS [lL];
DOUBLE_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [dD]?;
FLOAT_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [fF];
INT_LITERAL: DECIMAL_DIGITS;
fragment EXPONENT: [eE] [+\-]? DECIMAL_DIGITS;
fragment DECIMAL_DIGITS: [0-9] ([0-9_]* [0-9])?;
STRING_LITERAL
    : '"' (~["\\\r\n] | '\\' .)* '"'
    | '\'' (~['\\\r\n] | '\\' .)* '\''
    ;
REGEX_LITERAL: 'regex/' ( '\\/' | '\\\\' | ~[/\r\n\\] )* '/' [ims]*;
NOTHING_LITERAL: '???';

matchExpression: 'match' expression 'with' matchCaseList+;
matchCaseList: matchCase (',' matchCase)*;
matchCase: 'case' pattern ((PIPE | COMMA) pattern)* ('when' guard=expression)? MATCH_ARROW body=expression;
matchExpressionNoPipe: 'match' expressionNoPipe 'with' matchCaseNoPipeList+;
matchCaseNoPipeList: matchCaseNoPipe (',' matchCaseNoPipe)*;
matchCaseNoPipe: 'case' pattern ((PIPE | COMMA) pattern)* ('when' guard=expressionNoPipe)? MATCH_ARROW body=expressionNoPipe;
pattern: TYPE
        | INT_LITERAL
        | LONG_LITERAL
        | BOOL_LITERAL
        | STRING_LITERAL
        | FLOAT_LITERAL
        | typedPattern
        | identifier
        | wildcardPattern
        | constructorPattern;
wildcardPattern: UNDERSCORE NAME?;
typedPattern: patternType (NAME | UNDERSCORE);
patternType: COLLECTION '[' type ']'
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
           | qualifiedType ('[' type (',' type)* ']')?;
constructorPattern: TYPE '{' fieldPatternList? '}';
fieldPatternList: pattern (',' pattern)*;

newData: type BANG? '{' fieldAssignmentList? '}';
constructorData: MUL '{' fieldAssignmentList? '}';
new_list: '[' (expression (',' expression)* ','?)? ']';
new_set: '{' (expression (',' expression)* ','?)? '}';
new_dict: '{' (dict_entry (',' dict_entry)* ','? | COLON) '}';
dict_entry: expression ':' expression;
fieldAssignmentList: fieldAssignment (',' fieldAssignment)*;
fieldAssignment: namedFieldAssignment
               | spreadFieldAssignment
               | positionalFieldAssignment;
namedFieldAssignment: (identifier | STRING_LITERAL) COLON expression;
spreadFieldAssignment: SPREAD expression;
positionalFieldAssignment: expression;

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
             | PIPE_ANY
             | PIPE_ALL
             | QUESTION
             | TILDE
             | TILDE_TILDE
             | TILDE_GT
             | DIV_GT
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
                   | TILDE
                   | TILDE_TILDE
                   | TILDE_GT
                   | DIV_GT
                   | AND;

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
TILDE_TILDE : '~~';
TILDE_GT : '~>';
DIV_GT : '/>';
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
INC : '++';
DEC : '--';
MUL : '*';
DIV : '/';
POWER: '^';
MOD : '%';
FAT_ARROW : '=>';
MATCH_ARROW : '->';
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

DOC_COMMENT : '///' ~[\r\n]*;
LINE_COMMENT : '//' ~[\r\n]* -> skip;
BLOCK_COMMENT : '/*' .*? '*/' -> skip;
WS : [ \t\r\n]+ -> skip;
