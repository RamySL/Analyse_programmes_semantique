
main :- read(user_input, X), type_check(X).

type_check(prog(P)) :- type_prog(prog(P), void), write("OK\n").
type_check(_) :- write("KO\n").

context_init([
    /*(true, bool),
    (false, bool),
    (not, ([bool], bool)),
    (eq, ([int, int], bool)),
    (lt, ([int, int], bool)),
    
    (sub, ([int, int], int)),
    (mul, ([int, int], int)),
    (div, ([int, int], int)) */
    (add, ([int, int], int))
]).
/** !! Il ya une association entre ce qui est choisit dans prologTerm.ml et les noms d'atom ici **/
/**
    Utils
*/

find([(X,T)|_], X, T).

/* find([(true, bool)], true, T). */ 
find([_|XS], X, T) :- find(XS, X, T).

/**
Pultot que de réflechir comme une procédure, plutot avec les prédicat.
Quand est ce que get_types(ID_Ts, Ts) est vrai ? elle est vrai si Ts
contient tous les types qui sont dans ID_Ts et dans le meme ordre

get_types([(a,bool), (b, int), (c, test)], TS).
**/
get_types([], []).
get_types([(_, T)], [T]).
get_types([(_, T) | TAIL], [T | TS]) :- get_types(TAIL, TS).
/**
NEW_G représente bien un contexte dans lequel on a TO_ADD @ G quand ?
add_list_context([(a,bool), (b, int), (c, test)], [(d,string), (e,float)], G).
*/

add_list_context(G, [], G).
add_list_context(G, [(X, T) | TAIL], [(X, T) | TAIL2]) :- add_list_context(G, TAIL, TAIL2).


/**
Vérifie que les type des expressions EXPRS match celui de TYPES dans l'ordre
*/

match_exprs_types(_, [], []).

match_exprs_types(G, [E | ES], [T | TS]) :-  
    type_expr(G, E, T),
    match_exprs_types(G, ES, TS).

/**
Vérifie que les type des expressions EXPRS match celui de TYPES dans l'ordre.
Version APS1a pour la traitement du passage var valeur/référence
*/

match_exprPs_types(_, [], []).

match_exprPs_types(G, [E | ES], [T | TS]) :-  
    type_exprP(G, E, T),
    match_exprPs_types(G, ES, TS).

/* extrait la listes des identificateurs des paramètres associés à leur type marqué ou non de ref selon qu’ils ont
été déclarés avec la modalité var ou no
*/

extract_id_var([], []).
extract_id_var([(var(ID), T) | TAIL], [(ID, ref(T)) | TS]) :- extract_id_var(TAIL, TS).
extract_id_var([(ID, T) | TAIL], [(ID, T) | TS]) :- extract_id_var(TAIL, TS).

/* Prog */

type_prog(prog(P), void) :- 
    context_init(G0),
    type_block(G0, P, void).

/** Defintions **/

type_def(G, const(X, T, E), [(X, T) | G]) :- type_expr(G, E, T).
/* 
Dans l'environnement de type de défintion d'une fonction 
est définit par un tupe (liste de type de params, type de retour)
*/
                                                        /* ' * * * -> tret ' */
type_def(G, fun(F, T_RET, ID_T_PARAMS, BODY), [(F, (T_PARAMS, T_RET)) | G]) :- 
    get_types(ID_T_PARAMS, T_PARAMS),
    add_list_context(G, ID_T_PARAMS, G_EVAL),
    type_expr(G_EVAL, BODY, T_RET).

type_def(G, fun_rec(F, T_RET, ID_T_PARAMS, BODY), [(F, (T_PARAMS, T_RET)) | G]) :- 
    get_types(ID_T_PARAMS, T_PARAMS),
    add_list_context(G, ID_T_PARAMS, G_EVAL),
    /** La différence avec l'ancienne c'est dans l'env d'eval on a mis la fonction elle même **/
    type_expr([(F, (T_PARAMS, T_RET)) | G_EVAL], BODY, T_RET).

type_def(G, var(ID, T), [(ID, ref(T)) | G]).

type_def(G, proc(P, ID_T_PARAMS, BODY), [(P, (T_PARAMS, void)) | G]) :- 
    extract_id_var(ID_T_PARAMS, EXTRACTED_VAR),
    get_types(EXTRACTED_VAR, T_PARAMS),
    add_list_context(G, EXTRACTED_VAR, G_EVAL),
    type_block(G_EVAL, BODY, void).

type_def(G, proc_rec(P, ID_T_PARAMS, BODY), [(P, (T_PARAMS, void)) | G]) :- 
    extract_id_var(ID_T_PARAMS, EXTRACTED_VAR),
    get_types(EXTRACTED_VAR, T_PARAMS),
    add_list_context(G, EXTRACTED_VAR, G_EVAL),
    type_block([(P, (T_PARAMS, void)) | G_EVAL], BODY, void).


/* Commands */
type_cmds(G, defs(D, CS), void) :- 
    type_def(G, D, NEW_G),
    type_cmds(NEW_G, CS, void).

type_cmds(G, stats(S, CS), void) :- 
    type_stat(G, S, void),
    type_cmds(G, CS, void).

type_cmds(_, end, void).


/* Statements */
type_stat(G, echo(E), void) :- type_expr(G,E,int).

type_stat(G, set(ID, E) ,void) :- 
    find(G, ID, ref(T)),
    type_expr(G, E, T).

type_stat(G, if_stat(E, BK1, BK2), void) :-
    type_expr(G, E, bool), 
    type_block(G, BK1, void), 
    type_block(G, BK2, void).

type_stat(G, while(E, BK), void) :-
    type_expr(G, E, bool), 
    type_block(G, BK, void).

type_stat(G, call(ID, ARGS), void) :- 
    type_expr(G, ID, (T_PARAMS, void)),
    match_exprPs_types(G, ARGS, T_PARAMS).   


/* Expressions */
type_expr(_, num(_), int).
type_expr(G, if(E1,E2,E3), T) :- type_expr(G, E1, bool), type_expr(G, E2, T), type_expr(G, E3, T).
type_expr(G, and(E1,E2), bool) :- type_expr(G, E1, bool), type_expr(G, E2, bool).
type_expr(G, or(E1,E2), bool) :- type_expr(G, E1, bool), type_expr(G, E2, bool).
/* IDR */
type_expr(G, id(X), T) :- find(G, X, ref(T)).
/* IDV */
type_expr(G, id(X), T) :- find(G, X, T).
type_expr(G, app(FCT, ARGS), T_RET) :-
    type_expr(G, FCT, (T_PARAMS, T_RET)),
    match_exprs_types(G, ARGS, T_PARAMS).
/*TODO: repition pour app  ?*/
type_expr(G, app(FCT, ARGS), T_RET) :-
    type_expr(G, FCT, (T_PARAMS, T_RET)),
    match_exprs_types(G, ARGS, T_PARAMS).
type_expr(G, abs(ID_T_PARAMS, BODY), (T_PARAMS, T_RET)) :-
    add_list_context(G, ID_T_PARAMS, NEW_G),
    type_expr(NEW_G, BODY, T_RET),
    get_types(ID_T_PARAMS, T_PARAMS).
/* Expressions d'arguments APS1a */
type_exprP(G, adr(X), ref(T)) :- find(G, X, ref(T)).
/* TODO: détaille à mettre dans le rapport que ça : type_exprP(G, E, T) :- type_expr(G, E, T).
tout seule, ça ne suffit pas, pour imposer que l'absence de adr ne permette pas de d'effet de bord mémoire.
*/
/*FIXME: Problème de ref(ref(ref)) */
type_exprP(G, E, T) :- type_expr(G, E, ref(T))!. /* Puisque pas de 'adr' on unwrap le ref pour ne pas permettre de SET */
type_exprP(G, E, T) :- type_expr(G, E, T).

/* APS1 */
type_block(G, block(CS), void) :- type_cmds(G, CS, void).


