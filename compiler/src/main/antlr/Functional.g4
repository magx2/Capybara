grammar Functional;

@header {
package dev.capylang.parser.antlr;
}

@lexer::members {
private boolean lineStart = true;

private boolean containsNewline(String text) {
    return text.indexOf('\n') >= 0 || text.indexOf('\r') >= 0;
}

@Override
public org.antlr.v4.runtime.Token emit() {
    lineStart = false;
    return super.emit();
}
}

program : definition+ EOF;

definition:
    functionDeclaration
    | deriverDeclaration
    | typeDeclaration
    | enumDeclaration
    | dataDeclaration
    | singleDeclaration
    | constDeclaration;

functionDeclaration: docComment* VISIBILITY? 'fun' recFunctionMarker functionNameDeclaration '(' parameters? ')' functionType? '=' functionBody
                   | docComment* VISIBILITY? 'fun' functionNameDeclaration '(' parameters? ')' functionType? '=' functionBody;
recFunctionMarker: REC;
functionBody: expression
            | localDefinition+ '---' expression;
localDefinition: localFunctionDeclaration
               | localTypeDeclaration
               | localDataDeclaration
               | localSingleDeclaration
               | localConstDeclaration;
localFunctionDeclaration: docComment* 'fun' recFunctionMarker localFunctionNameDeclaration '(' parameters? ')' functionType? '=' expression
                        | docComment* 'fun' localFunctionNameDeclaration '(' parameters? ')' functionType? '=' expression;
localFunctionNameDeclaration: NAME | REC;
localTypeDeclaration: 'type' genericTypeDeclaration constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*
                    | 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)*;
localDataDeclaration: 'data' genericTypeDeclaration '{' dataBody? '}' constructorClause?
                    | 'data' genericTypeDeclaration '=' '{' dataBody? '}' constructorClause?;
localSingleDeclaration: 'single' TYPE;
localConstDeclaration: docComment* 'const' privateLocalConstName (':' type)? '=' expressionNoLet;
privateLocalConstName: NAME | TYPE;
functionNameDeclaration: identifier | genericTypeDeclaration DOT methodIdentifier;
methodIdentifier: identifier | 'with' | INFIX_METHOD_LITERAL;
docComment: DOC_COMMENT;

typeDeclaration: docComment* VISIBILITY? 'type' genericTypeDeclaration constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)* deriveClause?
               | docComment* VISIBILITY? 'type' genericTypeDeclaration '{' fieldDeclarationList? '}' constructorClause? '=' genericTypeDeclaration (PIPE genericTypeDeclaration)* deriveClause?;
enumDeclaration: 'enum' TYPE '{' TYPE (COMMA TYPE)* COMMA? '}';
dataDeclaration: docComment* VISIBILITY? 'data' genericTypeDeclaration '{' dataBody? '}' constructorClause? deriveClause?
               | docComment* VISIBILITY? 'data' genericTypeDeclaration '=' '{' dataBody? '}' constructorClause? deriveClause?;
constructorClause: 'with' 'constructor' '{' expression '}';
deriveClause: 'derive' TYPE (COMMA TYPE)* COMMA?;
deriverDeclaration: docComment* VISIBILITY? 'deriver' TYPE '{' deriverMethodDeclaration+ '}';
deriverMethodDeclaration: docComment* 'fun' identifier '(' parameters? ')' functionType '=' expression;
singleDeclaration: 'single' TYPE;
constDeclaration: docComment* VISIBILITY? 'const' TYPE (':' type)? '=' expressionNoLet;
fieldDeclarationList: fieldDeclaration (',' fieldDeclaration)* ','?;
fieldDeclaration: identifier ':' type
                | STRING_LITERAL ':' type
                | SPREAD TYPE;
dataBody: NATIVE_LITERAL | fieldDeclarationList;
genericTypeDeclaration: TYPE (typeLbrack TYPE (',' TYPE)* RBRACK)?;
typeLbrack: LBRACK | LINE_START_LBRACK;

VISIBILITY: 'local' | 'private';
BOOL_LITERAL: 'true' | 'false';
REC: 'rec';
NAME : [_]* [a-z] [a-zA-Z0-9_]*;
identifier: NAME | REC | 'derive' | 'deriver' | 'fun' | 'type' | 'enum' | 'byte' | 'int' | 'long' | 'double' | 'bool' | 'float' | 'nothing' | 'any';
parameters: parameter (',' parameter)*;
parameter: identifier ':' type;
functionType: ':' type;
type: LPAREN RPAREN FAT_ARROW type
    | LPAREN type (COMMA type)+ RPAREN FAT_ARROW type
    | type FAT_ARROW type
    | 'byte'
    | 'int'
    | 'long'
    | 'double'
    | 'bool'
    | 'float'
    | 'any'
    | 'data'
    | 'enum'
    | 'nothing'
    | qualifiedType (typeLbrack type (',' type)* RBRACK)?;
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
               | placeholder
               | new_list
               | new_dict
               | tupleLiteral
               | '(' expression ')'
               | '{' expression '}'
               | new_set
               | BANG expressionNoLet
               | BITWISE_NOT expressionNoLet
               | MINUS expressionNoLet
               | expressionNoLet LBRACK sliceIndexLiteral? COLON sliceIndexLiteral? RBRACK
               | expressionNoLet LBRACK argumentList? RBRACK
               | expressionNoLet LPAREN argumentList? RPAREN
               | expressionNoLet DOT methodIdentifier LPAREN methodArgumentList? RPAREN
               | expressionNoLet INFIX_METHOD_LITERAL expressionNoLet
               | expressionNoLet DOT identifier
               | expressionNoLet infixOperator expressionNoLet
               | value
               | newData
               | constructorData
               | matchExpression;
sliceIndexLiteral: MINUS? INT_LITERAL;
lambdaExpression: lambdaArgument FAT_ARROW expressionNoPipe
                | LPAREN (lambdaArgument (COMMA lambdaArgument)*)? RPAREN FAT_ARROW expressionNoPipe;
lambdaArgument: identifier | UNDERSCORE;
reduceExpression: expressionNoLetNoPipe COMMA LPAREN lambdaArgument COMMA lambdaArgument (COMMA lambdaArgument (COMMA lambdaArgument)?)? RPAREN FAT_ARROW expressionNoPipe;
expressionNoLetNoPipe: ifExpression
                     | lambdaExpression
                     | functionReference
                     | functionCall
                     | placeholder
                     | new_list
                     | new_dict
                     | tupleLiteral
                     | '(' expression ')'
                     | '{' expression '}'
                     | new_set
                     | BANG expressionNoLetNoPipe
                     | BITWISE_NOT expressionNoLetNoPipe
                     | MINUS expressionNoLetNoPipe
                     | expressionNoLetNoPipe LBRACK sliceIndexNoPipeLiteral? COLON sliceIndexNoPipeLiteral? RBRACK
                     | expressionNoLetNoPipe LBRACK argumentList? RBRACK
                     | expressionNoLetNoPipe LPAREN argumentList? RPAREN
                     | expressionNoLetNoPipe DOT methodIdentifier LPAREN methodArgumentList? RPAREN
                     | expressionNoLetNoPipe INFIX_METHOD_LITERAL expressionNoLetNoPipe
                     | expressionNoLetNoPipe DOT identifier
                     | expressionNoLetNoPipe infixOperatorNoPipe expressionNoLetNoPipe
                     | value
                     | newData
                     | constructorData
                     | matchExpressionNoPipe;
sliceIndexNoPipeLiteral: MINUS? INT_LITERAL;
tupleLiteral: LPAREN expression (COMMA expression)+ RPAREN;

ifExpression: 'if' expression 'then' expression 'else' expression;
functionReference: COLON identifier;
placeholder: UNDERSCORE;
functionCall: NAME '(' argumentList? ')'
            | REC '(' argumentList? ')'
            | 'derive' '(' argumentList? ')'
            | 'deriver' '(' argumentList? ')'
            | 'fun' '(' argumentList? ')'
            | 'byte' '(' argumentList? ')'
            | 'int' '(' argumentList? ')'
            | 'long' '(' argumentList? ')'
            | 'double' '(' argumentList? ')'
            | 'bool' '(' argumentList? ')'
            | 'float' '(' argumentList? ')'
            | 'nothing' '(' argumentList? ')'
            | 'any' '(' argumentList? ')'
            | TYPE DOT identifier '(' argumentList? ')';
value: literal | identifier | qualifiedType;
argumentList: expression (',' expression)*;
methodArgumentList: methodArgument (',' methodArgument)*;
methodArgument: namedMethodArgument | expression;
namedMethodArgument: identifier COLON expression;
literal: BYTE_LITERAL | LONG_LITERAL | DOUBLE_LITERAL | INT_LITERAL | BOOL_LITERAL | STRING_LITERAL | FLOAT_LITERAL | NOTHING_LITERAL | NATIVE_LITERAL | REGEX_LITERAL;
BYTE_LITERAL: '0' [xX] [0-9a-fA-F]+;
LONG_LITERAL: DECIMAL_DIGITS [lL];
DOUBLE_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [dD]?;
FLOAT_LITERAL: ([0-9]+ '.' [0-9]* EXPONENT? | [0-9]+ EXPONENT) [fF];
INT_LITERAL: DECIMAL_DIGITS;
fragment EXPONENT: [eE] [+\-]? [0-9]+;
fragment DECIMAL_DIGITS: [0-9] ([0-9_]* [0-9])?;
STRING_LITERAL
    : '"' (~["\\\r\n] | '\\' .)* '"'
    | '\'' (~['\\\r\n] | '\\' .)* '\''
    ;
REGEX_LITERAL: 'regex/' ( '\\/' | '\\\\' | ~[/\r\n\\] )* '/' [ims]*;
NOTHING_LITERAL: '???';
NATIVE_LITERAL: '<native>';

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
        | patternType
        | identifier
        | wildcardPattern
        | constructorPattern;
wildcardPattern: UNDERSCORE NAME?;
typedPattern: patternType (NAME | UNDERSCORE);
patternType: 'byte'
           | 'int'
           | 'long'
           | 'double'
           | 'bool'
           | 'float'
           | 'any'
           | 'data'
           | 'enum'
           | 'nothing'
           | qualifiedType (typeLbrack type (',' type)* RBRACK)?;
constructorPattern: TYPE '{' fieldPatternList? '}';
fieldPatternList: pattern (',' pattern)*;

newData: type BANG? '{' fieldAssignmentList? '}';
constructorData: MUL '{' fieldAssignmentList? '}';
new_list: (LBRACK | LINE_START_LBRACK) (expression (',' expression)* ','?)? RBRACK;
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
LINE_START_LBRACK : {lineStart}? '[';
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
BLOCK_COMMENT : '/*' .*? '*/' { if (containsNewline(getText())) lineStart = true; } -> skip;
WS : [ \t\r\n]+ { if (containsNewline(getText())) lineStart = true; } -> skip;
