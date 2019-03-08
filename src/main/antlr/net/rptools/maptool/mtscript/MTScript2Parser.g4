parser grammar MTScript2Parser;

options { tokenVocab=MTScript2Lexer; }

@header {
  package net.rptools.maptool.mtscript;
}

compilationUnit : (TOOMANY | macroBlock | TEXT | TOOFEW)* ;

macroBlock      : OPEN statement* CLOSE ;

literal			: integerLiteral
				| NUMBER_LITERAL
				| STRING_LITERAL
				| BOOL_LITERAL
				| NULL_LITERAL 
				;

integerLiteral	: DECIMAL_LITERAL
				| HEX_LITERAL
				;

methodDeclaration	: FUNCTION IDENTIFIER formalParameters block ;

formalParameters	: '(' formalParameterList? ')' ;

formalParameterList	: formalParameter (',' formalParameter)* ;

formalParameter		: type variableDeclaratorId;

block	: '{' blockStatement* '}' ;

blockStatement	: localVariableDeclaration ';'
				| statement
				;
				
localVariableDeclaration	: type variableDeclarators ;

statement		: blockLabel=block
				| ASSERT expression (':' expression)? ';'
				| IF parExpression block (ELSE IF parExpression block)* (ELSE block)?
				| FOR '(' forControl ')' block
				| WHILE parExpression block
				| DO block WHILE parExpression ';'
				| TRY block (catchClause+ finallyBlock? | finallyBlock)
				| SWITCH parExpression '{' switchBlockStatementGroup* switchLabel* '}'
				| RETURN expression? ';'
				| THROW expression ';'
				| BREAK ';'
				| CONTINUE ';'
				| SEMI
				| statementExpression=expression ';'
				;
				
				
				
catchClause		: CATCH '(' IDENTIFIER ')' block ;

finallyBlock	: FINALLY block ;

switchBlockStatementGroup	: switchLabel+ blockStatement+ ;

switchLabel		: CASE constantExpression=expression ':'
				| DEFAULT ':'
				;

forControl		: foreachControl
				| forInit? ';' expression? ';' forUpdate=expressionList?
				;

forInit         : localVariableDeclaration
                | expressionList
                ;

foreachControl 	: type variableDeclaratorId ':' expression ;

parExpression	: '(' expression ')' ;
expressionList  : expression (',' expression)* ;

methodCall 		: IDENTIFIER '(' expressionList? ')' ;

expression      : '(' expression ')'
                | literal
                | IDENTIFIER
                | expression bop='.'
                  ( IDENTIFIER
                  | methodCall
                  )
                | expression '[' expression ']'
                | methodCall
                | prefix='!' expression
                | expression bop=('*' | '/' | '%') expression
                | expression bop=('+' | '-') expression
                | expression bop=('<=' | '>=' | '>' | '<') expression
                | expression bop=INSTANCEOF type
                | expression bop=('==' | '!=') expression
                | expression bop='&' expression
                | expression bop='^' expression
                | expression bop='|' expression
                | expression bop='&&' expression
                | expression bop='||' expression
                | expression bop='?' expression ':' expression
                | <assoc=right> expression bop=('=' | '+=' | '-=' | '*=' | '/=' | '&=' | '|=' | '^=' | '%=' ) expression
                ;

fieldDeclaration: type variableDeclarators ';' ;

constantDeclaration : type constantDeclarator (',' constantDeclarator)* ';' ;

constantDeclarator  : IDENTIFIER ('[' ']')* '=' variableInitializer ;

variableDeclarators : variableDeclarator (',' variableDeclarator)* ;

variableDeclarator      : variableDeclaratorId ( '=' variableInitializer)? ;

variableDeclaratorId    : IDENTIFIER ( '[' ']' )* ;

variableInitializer : arrayInitializer
                    | expression
                    ;

arrayInitializer    : '{' (variableInitializer ( ',' variableInitializer )* (',')? )? '}' ;

////////

arguments       : '(' expressionList? ')' ;

type            : BOOLEAN
                | INTEGER
                | NUMBER
                | STRING
                | ROLL
                | DICT
                ;
