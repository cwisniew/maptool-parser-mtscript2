lexer grammar MTScript2Lexer;

@header {
  package net.rptools.maptool.mtscript;
}

TOOMANY : '[[[' '['+ ;
OPEN    : '[[[' -> mode(MACRO_ISLAND) ;
TOOFEW  : '[' '['? ;
TEXT    : ~'['+ ;

mode MACRO_ISLAND;
CLOSE   : ']]]' -> mode(DEFAULT_MODE) ;

// Keywords
ASSERT  : 'assert';
BREAK   : 'break';
CASE    : 'case';
CONST   : 'const';
CONTINUE: 'continue';
DEFAULT : 'default';
DO      : 'do';
ELSE    : 'else';
FOR     : 'for';
FOREACH : 'foreach';
FUNCTION: 'function';
IF      : 'if';
RETURN  : 'return';
SWITCH  : 'switch';
WHILE   : 'while';
TRY		: 'try';
CATCH	: 'catch';
FINALLY	: 'finally';
THROW	: 'throw';
INSTANCEOF	: 'instanceof';

INTEGER : 'integer';
NUMBER  : 'number';
STRING  : 'string';
ROLL    : 'roll';
BOOLEAN : 'bool';
DICT    : 'dict';

// Literals
DECIMAL_LITERAL : ( '0' | [1-9] (Digits? | '_' + Digits) ) ;
HEX_LITERAL     : '0' [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])? ;
NUMBER_LITERAL  : (Digits '.' Digits? | '.' Digits) ;
BOOL_LITERAL    : 'true' | 'false' ;
STRING_LITERAL  : ( '\'' (~['] | EscapeSequence)* '\''  )
                | ( '"'  (~["] | EscapeSequence)* '"'   )
                ;
NULL_LITERAL    : 'null';

// Separators
LPAREN  : '(';
RPAREN  : ')';
LBRACE  : '{';
RBRACE  : '}';
LBRACK  : '[';
RBRACK  : ']';
SEMI    : ';';
COMMA   : ',';
DOT     : '.';

// Operators
ASSIGN      : '=';
GT          : '>';
LT          : '<';
BANG        : '!';
TILDE       : '~';
QUESTION    : '?';
COLON       : ':';
EQUAL       : '==';
LE          : '<=';
GE          : '>=';
NOTEQUAL    : '!=';
AND         : '&&';
OR          : '||';
INC         : '++';
DEC         : '--';
ADD         : '+';
SUB         : '-';
MUL         : '*';
DIV         : '/';
BITAND      : '&';
BITOR       : '|';
CARET       : '^';
MOD         : '%';

ADD_ASSIGN  : '+=';
SUB_ASSIGN  : '-=';
MUL_ASSIGN  : '*=';
DIV_ASSIGN  : '/=';
AND_ASSIGN  : '&=';
OR_ASSIGN   : '|=';
XOR_ASSIGN  : '^=';
MOD_ASSIGN  : '%=';

// Whitespace and comments
WS              : [ \t\r\n\u000C]+  -> channel(HIDDEN);
COMMENT         : '/*' .*? '*/'     -> channel(HIDDEN);
LINE_COMMENT    : '//' ~[\r\n]*     -> channel(HIDDEN);

// Identifiers
IDENTIFIER          : Letter LetterOrDigit* ;

// Fragment rules
fragment EscapeSequence	: '\\' [btnfr"'\\] ;

fragment Digits         : [0-9] ([0-9_]* [0-9])?
                        ;

fragment LetterOrDigit  : Letter
                        | [0-9]
                        ;

fragment Letter         : [a-zA-Z$_] // Java letters below 0x7F
                        | ~[\u0000-\u007F\uD800-\uDBFF] // covers all characters above 0x7F which are not a surrogate
                        | [\uD800-\uDBFF] [\uDC00-\uDFFF] // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
                        ;
