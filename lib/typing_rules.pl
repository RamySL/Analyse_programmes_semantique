
main :- read(user_input, X), type_check(X).
/** !! Il ya une association entre ce qui est choisit dans prologTerm.ml et les noms d'atom ici **/
/**
    Utils
*/

find([(X,T)|_], X, T).
find([_|XS], X, T) :- find(XS, X, T).

/**
Pultot que de réflechir comme une procédure, plutot avec les prédicat.
Quand est ce que get_types(ID_Ts, Ts) est vrai ? elle est vrai si Ts
contient tous les types qui sont dans ID_Ts et dans le meme ordre

get_types([(a,bool), (b, int), (c, test)], TS).
**/
get_types([], []).
get_types([(X, T)], [T]).
get_types([(X, T) | TAIL], [T | TS]) :- get_types(TAIL, TS).
/**
NEW_G représente bien un contexte dans lequel on a TO_ADD @ G quand ?
add_list_context([(a,bool), (b, int), (c, test)], [(d,string), (e,float)], G).
*/

add_list_context(G, [], G).
add_list_context(G, [(X, T) | TAIL], [(X, T) | TAIL2]) :- add_list_context(G, TAIL, TAIL2).
 
/* **** */
type_prog(_, prog(p), void). 

/** Defintions **/

type_def(G, const(id(X), T, E), [(X, T) | G]) :- type_expr(G, E, T).
/* 
Dans l'environnement de type de défintion d'une fonction 
est définit par un tupe (liste de type de params, type de retour)
*/
                                                        /* ' * * * -> tret ' */
type_def(G, fun(id(F), T_RET, ID_T_PARAMS, BODY), [(F, (T_PARAMS, T_RET)) | G]) :- 
    get_types(ID_T_PARAMS, T_PARAMS),
    add_list_context(G, ID_T_PARAMS, G_EVAL),
    type_expr(G_EVAL, BODY, T_RET).

type_def(G, funRec(id(F), T_RET, ID_T_PARAMS, BODY), [(F, (T_PARAMS, T_RET)) | G]) :- 
    get_types(ID_T_PARAMS, T_PARAMS),
    add_list_context(G, ID_T_PARAMS, G_EVAL),
    /** La différence avec l'ancienne c'est dans l'env d'eval on a mis la fonction elle même **/
    type_expr([(F, (T_PARAMS, T_RET)) | G_EVAL], BODY, T_RET).


/* Commands */


/* Statements */
type_stat(_, echo(E), void) :- type_expr(_,E,int).


/* Expressions */
type_expr(_, num(_), int).
type_expr(_, true, bool).
type_expr(_, false, bool).
/** Rajouter un tfun() ? un nouveau type */
type_expr(G, if(E1,E2,E3), T) :- type_expr(G, E1, bool), type_expr(G, E2, T), type_expr(G, E3, T).
type_expr(G, and(E1,E2), bool) :- type_expr(G, E1, bool), type_expr(G, E2, bool).
type_expr(G, or(E1,E2), bool) :- type_expr(G, E1, bool), type_expr(G, E2, bool).
typr_expr(G, id(X), T) :- find(G, X, T).
type_expr(G, app(FCT, ARGS), T) :-



type_check(prog) :- write("KO\n").
