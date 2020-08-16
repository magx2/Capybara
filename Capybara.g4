grammar Capybara;

compileUnit
	: packageDeclaration  EOF
	;

packageDeclaration
	: 'package '
	;

code
	: struct code
	;

struct
	: 'struct'
	;

NEWLINE : '\r'? '\n' ;
INDENT : [ \t]+ ;
//WS : [ \t\r\n]+ -> skip ;