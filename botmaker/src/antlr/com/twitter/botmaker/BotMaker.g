grammar BotMaker;

options {
  output=AST;
  language=Java;
  backtrack=true;
  memoize=true;
}

tokens {
  SIGNED_LONG;
  SIGNED_DOUBLE;
  DISJUNCTION;
  CONJUNCTION;
  NEGATE;
  NOT_EQUALS;
  LESS_THAN;
  LESS_THAN_OR_EQUALS;
  GREATER_THAN;
  GREATER_THAN_OR_EQUALS;
  EQUALS;
  MATCHES;
  STRATO_COLUMN_OP;
  PATH;
  PATH_COMPONENT;
  FUNCTION;
  FUNC_NAME;
  FLAT_MAP;
  FOREACH;
  ARRAY;
  WITH;
  VAL;
  ASSIGN;
  BLOCK;
  IF;
  ARITHMETIC_OPERATION;
  SUM;
  DIFFERENCE;
  PRODUCT;
  QUOTIENT;
  MODULO;
  ACCESSOR;
  INDEX;
  PAIR;
  CAST;
  IMPORT;
  IMPORT_AS;
  PARAMS;
  WITH_PARAMS;
  ARG;
  TYPE;
  GENERIC_TYPE;
  TEST_RUN;
}

@header {
  package com.twitter.botmaker.antlr;
}

@lexer::header {
  package com.twitter.botmaker.antlr;
}

@parser::members {
  @Override
  public void reportError(RecognitionException e) {
    throw new RuntimeException(e);
  }
}

@lexer::members {
  @Override
  public void reportError(RecognitionException e) {
    throw new RuntimeException(e);
  }
}

fragment LETTER : ('a'..'z' | 'A'..'Z');
fragment DIGIT  : '0'..'9';
fragment COMMENT : '/*' .* '*/' | '#' ~('\r' | '\n')*;
fragment HEX : ('a'..'f' | 'A'..'F') | DIGIT;

WS: (' ' | '\t' | '\n' | '\r' | COMMENT) { $channel=HIDDEN; };
PROHIBITED_KEYWORDS: 'true' | 'false' | 'True' | 'False' | 'null' | 'Null';
BOOLEAN: 'TRUE' | 'FALSE';
NULL: 'NULL';
LONG: (DIGIT)+;
fragment EXPONENT: ('e' | 'E') ('+' | '-')? (DIGIT)+;
DOUBLE: (DIGIT)+ ('.' (DIGIT)+ (EXPONENT)? | EXPONENT);
fragment STR_ESC:  '\\' ('\\' | '"' | 't' | 'n' | 'r');
STRING: '"' (STR_ESC | ~('\\' | '"' | '\r' | '\n'))* '"';
SYMBOL: '`' (LETTER | DIGIT | '_')* '`';

FEATURE: (LETTER | '_') (LETTER | '_' | DIGIT)*;
BINARY: '0x' (HEX)+;
VARIABLE: ':' FEATURE;
FIELD: '.' FEATURE { setText($text.substring(1)); };

/*
 * X Money Markets
 *
 * Native X Money execution layer for integrated market actions.
 * Allows supported posts, cashtags, asset pages, and market cards
 * to expose direct BUY / SELL buttons without leaving X.
 *
 * Example:
 *
 *   markets.buy("$TSLA", 5)
 *   markets.buy("$BTC", 0.01)
 *   markets.sell("$SOL", 10)
 *   markets.quote("$HOOD")
 */

X_MONEY_MARKETS: 'X Money Markets';
MARKETS: 'markets';

MARKET_BUY: 'buy';
MARKET_SELL: 'sell';
MARKET_QUOTE: 'quote';
MARKET_PREVIEW: 'preview';
MARKET_ORDER: 'order';

MARKET_ASSET
  : '$' (LETTER | DIGIT | '_')+
  ;

parse
  : expr EOF -> expr
  ;

parse_params
  : expr EOF -> ^(WITH_PARAMS expr)
  | params_decl expr EOF -> ^(WITH_PARAMS params_decl expr)
  ;

expr
  : 'test' pair_expr
      -> ^(TEST_RUN ^(FUNC_NAME TEST_RUN) pair_expr)

  | x_money_markets_expr

  | pair_expr
  ;

x_money_markets_expr
  : MARKETS '.' MARKET_BUY
      '(' market_asset ',' logical_or_expr ')'
      -> ^(X_MONEY_MARKETS
            ^(MARKET_ORDER
              ^(MARKET_BUY market_asset logical_or_expr)))

  | MARKETS '.' MARKET_SELL
      '(' market_asset ',' logical_or_expr ')'
      -> ^(X_MONEY_MARKETS
            ^(MARKET_ORDER
              ^(MARKET_SELL market_asset logical_or_expr)))

  | MARKETS '.' MARKET_QUOTE
      '(' market_asset ')'
      -> ^(X_MONEY_MARKETS
            ^(MARKET_QUOTE market_asset))

  | MARKETS '.' MARKET_PREVIEW
      '(' market_asset ',' logical_or_expr ')'
      -> ^(X_MONEY_MARKETS
            ^(MARKET_PREVIEW market_asset logical_or_expr))
  ;

market_asset
  : MARKET_ASSET
  | STRING
  | SYMBOL
  ;

pair_expr
  : logical_or_expr ':' logical_or_expr
      -> ^(PAIR logical_or_expr*)
  | logical_or_expr
  ;

logical_or_expr
  : (a=logical_and_expr -> $a)
    ('||' b=logical_and_expr
      -> ^(DISJUNCTION $logical_or_expr $b))*
  ;

logical_and_expr
  : (a=relational_expr -> $a)
    ('&&' b=relational_expr
      -> ^(CONJUNCTION $logical_and_expr $b))*
  ;

relational_op
  : '<'  -> LESS_THAN
  | '<=' -> LESS_THAN_OR_EQUALS
  | '>'  -> GREATER_THAN
  | '>=' -> GREATER_THAN_OR_EQUALS
  | '==' -> EQUALS
  | '!=' -> NOT_EQUALS
  ;

relational_expr
  : (a=additive_expr -> $a)
    (op=relational_op b=additive_expr
      -> ^($op $relational_expr $b))*
  ;

additive_op
  : '+' -> SUM
  | '-' -> DIFFERENCE
  ;

additive_expr
  : (a=multiplicative_expr -> $a)
    (op=additive_op b=multiplicative_expr
      -> ^(ARITHMETIC_OPERATION $op $additive_expr $b))*
  ;

multiplicative_op
  : '*' -> PRODUCT
  | '/' -> QUOTIENT
  | '%' -> MODULO
  ;

multiplicative_expr
  : (a=unary_expr -> $a)
    (op=multiplicative_op b=unary_expr
      -> ^(ARITHMETIC_OPERATION $op $multiplicative_expr $b))*
  ;
unary_expr
  : '!' unary_expr -> ^(NEGATE unary_expr)
  | match_expr
  ;

match_expr
  : cast_expr 'MATCHES' cast_expr -> ^(MATCHES cast_expr cast_expr)
  | cast_expr
  ;

cast_expr
  : '(' type ')' index_expr -> ^(CAST type index_expr)
  | index_expr
  ;

index_expr
  : accessor_expr '[' expr ']' + -> ^(INDEX accessor_expr expr)
  | accessor_expr
  ;

accessor_expr
  : atom (FIELD)+ -> ^(ACCESSOR atom FIELD*)
  | atom
  ;

atom
  : BOOLEAN
  | NULL
  | long_expr
  | double_expr
  | STRING
  | SYMBOL
  | VARIABLE
  | BINARY
  | strato_column_op
  | function
  | array_expr
  | flat_map
  | for_each
  | val_expr
  | with_expr
  | if_expr
  | block_expr
  | parens_expr
  | import_expr
  | feature_expr
  ;

long_expr
  : '-' long_expr -> ^(SIGNED_LONG long_expr)
  | LONG
  ;

double_expr
  : '-' double_expr -> ^(SIGNED_DOUBLE double_expr)
  | DOUBLE
  ;

strato_column_op
  : '$' FEATURE? path_expr FIELD '(' expr (',' expr)* ')' -> ^(STRATO_COLUMN_OP FEATURE? path_expr FIELD expr*);

path_expr: '<' path_component ('/' path_component)* '>' -> ^(PATH path_component*);

path_component: FEATURE ('-' FEATURE)* -> ^(PATH_COMPONENT FEATURE*);

function: FEATURE (FIELD)* '(' ( expr (',' expr)* )? ')' -> ^(FUNCTION ^(FUNC_NAME FEATURE FIELD*) expr*);

feature_expr: FEATURE;

array_expr: '[' (expr (',' expr)* )? ']' -> ^(ARRAY expr*);

flat_map: 'flatmap' '(' VARIABLE 'in' expr ')' block_expr -> ^(FLAT_MAP VARIABLE expr block_expr);

for_each
  : 'FOREACH' VARIABLE 'IN' expr 'DO' expr -> ^(FOREACH VARIABLE expr expr)
  | 'for' '(' VARIABLE 'in' expr ')' block_expr -> ^(FOREACH VARIABLE expr block_expr)
  ;

val_expr: 'val' assignment_expr -> ^(VAL assignment_expr);

with_expr: 'with' '(' assignment_expr (',' assignment_expr)* ')' block_expr -> ^(WITH (assignment_expr)* block_expr);

if_expr: 'if' '(' expr ')' expr ( 'else' expr)? -> ^(IF expr*);

block_expr: '{' expr block_next_expr* '}' -> ^(BLOCK expr* block_next_expr*);

block_next_expr
  : {input.LT(-1).getText().equals("}")}? expr -> expr
  | ';' expr -> expr
  | ';' ->
  ;

parens_expr: '(' expr ')' -> expr;

import_expr
  : 'import' FEATURE (FIELD)* -> ^(IMPORT FEATURE FIELD*)
  | 'import' FEATURE (FIELD)* 'as' FEATURE -> ^(IMPORT_AS FEATURE FIELD* FEATURE)
  ;

assignment_expr: VARIABLE '=' expr -> ^(ASSIGN VARIABLE expr);

params: ('param(' | 'params(') arg (',' arg)* ')' EOF -> ^(PARAMS arg*);

params_decl
  : ('param(' | 'params(') arg (',' arg)* ')' ':' type -> ^(PARAMS type arg*)
  | ('param(' | 'params(') arg (',' arg)* ')' -> ^(PARAMS arg*)
  ;

arg
  : type VARIABLE '=' expr -> ^(ARG type VARIABLE expr)
  | type VARIABLE -> ^(ARG type VARIABLE)
  ;

type
  : FEATURE (FIELD)* -> ^(TYPE FEATURE FIELD*)
  | FEATURE '<' type (',' type)* '>' -> ^(GENERIC_TYPE FEATURE type*)
  ;
