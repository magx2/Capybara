grammar Functional;

@header {
package pl.grzeslowski.capybara.parser.antlr;
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
functionBody: localDefinition* expression;
localDefinition: localFunctionDeclaration
               | localTypeDeclaration
               | localDataDeclaration
               | localConstDeclaration;
localFunctionDeclaration: 'fun' NAME '(' parameters? ')' functionType? '=' expression;
localTypeDeclaration: 'type' genericTypeDeclaration '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*
                    | 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*;
localDataDeclaration: 'data' genericTypeDeclaration '{' fieldDeclarationList? '}'
                    | 'data' genericTypeDeclaration '=' '{' fieldDeclarationList? '}';
localConstDeclaration: 'const' TYPE (':' type)? '=' expressionNoLet;
functionNameDeclaration: identifier | genericTypeDeclaration DOT methodIdentifier;
methodIdentifier: identifier | INFIX_METHOD_LITERAL;
docComment: DOC_COMMENT;

typeDeclaration: VISIBILITY? 'type' genericTypeDeclaration '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*
               | VISIBILITY? 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*;
enumDeclaration: 'enum' TYPE '{' TYPE (COMMA TYPE)* COMMA? '}';
dataDeclaration: VISIBILITY? 'data' genericTypeDeclaration '{' fieldDeclarationList? '}'
               | VISIBILITY? 'data' genericTypeDeclaration '=' '{' fieldDeclarationList? '}';
singleDeclaration: 'single' TYPE;
constDeclaration: VISIBILITY? 'const' TYPE (':' type)? '=' expressionNoLet;
fieldDeclarationList: fieldDeclaration (',' fieldDeclaration)*;
fieldDeclaration: NAME ':' type
                | STRING_LITERAL ':' type
                | SPREAD TYPE;
genericTypeDeclaration: TYPE ('[' TYPE (',' TYPE)* ']')?;

VISIBILITY: 'local';
BOOL_LITERAL: 'true' | 'false';
COLLECTION: 'list' | 'set' | 'dict';
NAME : [_]* [a-z] [a-zA-Z0-9_]*;
identifier: NAME | COLLECTION | 'fun' | 'byte' | 'int' | 'long' | 'double' | 'bool' | 'string' | 'float' | 'nothing' | 'any';
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
INFIX_METHOD_LITERAL: '`' [+\-*/\\^%$#@~!:<>|]+ '`';
expression: letExpression* expressionNoLet;
letExpression: 'let' NAME (':' type)? '=' expressionNoLet ';'?;
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
               | expressionNoLet DOT methodIdentifier LPAREN argumentList? RPAREN
               | expressionNoLet INFIX_METHOD_LITERAL expressionNoLet
               | expressionNoLet DOT NAME
               | expressionNoLet infixOperator expressionNoLet
               | value
               | newData
               | matchExpression;
indexLiteral: MINUS? INT_LITERAL;
sliceIndexLiteral: MINUS? INT_LITERAL;
lambdaExpression: lambdaArgument FAT_ARROW expressionNoLetNoPipe
                | LPAREN (lambdaArgument (COMMA lambdaArgument)*)? RPAREN FAT_ARROW expressionNoLetNoPipe;
lambdaArgument: identifier | UNDERSCORE;
reduceExpression: expressionNoLetNoPipe COMMA LPAREN lambdaArgument COMMA lambdaArgument (COMMA lambdaArgument (COMMA lambdaArgument)?)? RPAREN FAT_ARROW expressionNoLetNoPipe;
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
                     | expressionNoLetNoPipe DOT methodIdentifier LPAREN argumentList? RPAREN
                     | expressionNoLetNoPipe INFIX_METHOD_LITERAL expressionNoLetNoPipe
                     | expressionNoLetNoPipe DOT NAME
                     | expressionNoLetNoPipe infixOperatorNoPipe expressionNoLetNoPipe
                     | value
                     | newData
                     | matchExpression;
indexNoPipeLiteral: MINUS? INT_LITERAL;
sliceIndexNoPipeLiteral: MINUS? INT_LITERAL;
tupleLiteral: LPAREN expression (COMMA expression)+ RPAREN;

ifExpression: 'if' expression 'then' expression 'else' expression;
functionReference: COLON identifier;
functionCall: identifier '(' argumentList? ')'
            | TYPE DOT identifier '(' argumentList? ')';
value: literal | identifier | qualifiedType;
argumentList: expression (',' expression)*;
literal: BYTE_LITERAL | LONG_LITERAL | DOUBLE_LITERAL | INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL | NOTHING_LITERAL;
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
NOTHING_LITERAL: '???';

matchExpression: 'match' expression 'with' matchCaseList+;
matchCaseList: matchCase (',' matchCase)*;
matchCase: PIPE pattern (COMMA pattern)* MATCH_ARROW expressionNoLetNoPipe;
pattern: TYPE
        | INT_LITERAL
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

newData: type '{' fieldAssignmentList? '}';
new_list: '[' (expression (',' expression)* ','?)? ']';
new_set: '{' (expression (',' expression)* ','?)? '}';
new_dict: '{' (dict_entry (',' dict_entry)* ','? | COLON) '}';
dict_entry: expression ':' expression;
fieldAssignmentList: fieldAssignment (',' fieldAssignment)*;
fieldAssignment: namedFieldAssignment
               | spreadFieldAssignment
               | positionalFieldAssignment;
namedFieldAssignment: (NAME | STRING_LITERAL) COLON expression;
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






