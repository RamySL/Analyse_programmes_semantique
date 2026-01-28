
main :- read(user_input, X), type_check(X).
/** Rmplace le _ par G là ou l'env est le meme entre le predicat et le statement*/
/* Expressions */
type_expr(_, num(_), int).
type_expr(_, true, bool).
type_expr(_, false, bool).
type_expr(_, if(E1,E2,E3), T) :- type_expr(_, E1, bool), type_expr(_, E2, T), type_expr(_, E3, T).
type_expr(_, and(E1,E2), bool) :- type_expr(_, E1, bool), type_expr(_, E2, bool).
type_expr(_, or(E1,E2), bool) :- type_expr(_, E1, bool), type_expr(_, E2, bool).

/* Commandes */
                        /* Affirme les deux conditions : S appartient aux statements et il est typé à void */
type_cmd(_, S, void) :- type_stat(_, S, void).
type_cmd(_, S, void) :- type_stat(_, S, void).

type_stat(_, echo(E), void) :- type_expr(_,E,int).


type_check(_) :- write("KO\n").
