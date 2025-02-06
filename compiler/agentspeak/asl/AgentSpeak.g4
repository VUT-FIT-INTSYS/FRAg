grammar AgentSpeak;

// --- rules  ---------------------------------------------------------------------------

agent:
    init_bels ( plan | init_goal)+
    ;

//agent:
//    init_bels init_goals plans
//    ;

init_bels:
    beliefs rules
    ;

beliefs:
    ( literal '.' )*
    ;

rules:
    ( literal ':-' log_expr '.')*
    ;

//init_goals:
//    ( init_goal )*
//    ;

init_goal:
    '!' literal '.'
    ;

//plans:
//    ( plan )*
//    ;

plan: 
    ('@' atomic_formula )? triggering_event
    (':' context )?
    ( '<-' body )? '.'
    ;

triggering_event:
    ( '+' | '-' ) ( '!' | '?' )? literal
    ;

literal:
    '~'? atomic_formula
    ;

context:
    log_expr | ATOM
    ;

log_expr:
    simple_log_expr
    | 'not' log_expr
    | log_expr '&' log_expr
    | log_expr '|' log_expr
    | '(' log_expr ')'
    ;

simple_log_expr:
    ( literal | rel_expr | VAR )
    ;

body: 
    body_formula ( ';' body_formula )*
    ;

body_formula:
    ( '!' | '!!' | '?' | '+' | '-' | '-+' ) literal
    | atomic_formula
    | VAR
    | rel_expr
    | internal_action
    ;

atomic_formula:
    ( ATOM | VAR )
    ( '(' list_of_terms ')' )?
    ( '[' list_of_terms ']' )?
    ;

list_of_terms:
    term ( ',' term )*
    ;

term:
    term_value
    | arithm_expr
    | literal
    | list_structure
    | VAR
    | internal_action
    ;

term_value :
    | NUMBER
    | STRING
    ;

internal_action:
    '.' ATOM '(' list_of_terms ')'
    ;

list_structure:
    '[' ']'
    | '[' term ( ',' term )* ']'
    | '[' term ( ',' term )* '|' ( list_structure | VAR ) ']'
    ;

rel_expr:
    rel_term
    ( ('<' | '<=' | '>' | '>=' | '==' | '\\==' | '=' | '=..') rel_term )*
    ;

rel_term: 
    literal | arithm_expr
    ;

arithm_expr:
    arithm_term
    ( ( '+' | '-' | '*' | '**' | '/' | 'div' | 'mod' )
    arithm_term )*
    ;

arithm_term:
    NUMBER
    | VAR
    | '-' arithm_term
    | '(' arithm_expr ')'
    ;

// --- terminal symbols -----------------------------------------------------------------

//ATOM:
//    LC_LETTER | '.' CHAR (CHAR | '.' CHAR)*
//    | '\'' (~['])* '\''
//    ;

ATOM :
    'true'          // přidáno
    | 'false'       // přidáno pokud chcete i false
    | LC_LETTER ( LC_LETTER | UP_LETTER | DIGIT | '_' )*
    ;

NUMBER:
    DIGIT (DIGIT)*
    | (DIGIT)* '.' (DIGIT)+ ([eE] ([+-])? (DIGIT)+)?
    | (DIGIT)+ ([eE] ([+-])? (DIGIT)+)
    ;

VAR:
    '_'    // anonymous variable
    |
    ( UP_LETTER | LC_LETTER )
    ( LC_LETTER | UP_LETTER | DIGIT | '_' )*
    ;

//CHAR:
//    LC_LETTER
//    | UP_LETTER
//    | DIGIT
//    | '_'
//;

fragment LC_LETTER :
    [a-z]
;

fragment UP_LETTER :
    [A-Z]
    ;

fragment DIGIT:
    [0-9]
    ;

STRING:
    '"' ~('"')* '"'
    | '\'' ~('\'')* '\''
    ;

// --- skip items -----------------------------------------------------------------------

WS: // while space
   (' ' | '\t' | '\n' | '\r')+ -> skip
   ;

LC:  // line comment
    '//' .*? '\r'? '\n' -> skip
    ;

BC: // block comment
    '/*' .*? '*/' -> skip
    ;
