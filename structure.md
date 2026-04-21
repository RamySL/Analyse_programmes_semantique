## Structure du projet

```text
.
├── bin/
│   ├── dune
│   ├── main.ml
│   └── test_aps.ml
├── examples/
│   ├── APS0/
│   ├── APS1/
│   ├── APS1a/
│   └── APS2/
├── lib/
│   ├── FrontEnd/
│   │   ├── Parsing/
│   │   │   ├── dune
│   │   │   ├── lexer.mll
│   │   │   └── parser.mly
│   │   └── ast.ml
│   ├── Interpreter/
│   │   ├── helper.ml
│   │   ├── Interpreter.ml
│   │   └── types.ml
│   ├── Typer/
│   │   ├── prologTerm.ml
│   │   └── typing_rules.pl
│   └── Utils/
│       ├── manip_sys.ml
│       ├── pretty.ml
│       ├── tests_suits.ml
│       └── dune
├── dune-project
├── README.md
└── who.md
```

### Détail des dossiers

#### `bin/`

Contient les exécutables du projet :

- `main.ml` : point d’entrée pour lancer l’analyse, le typage et l’évaluation d’un programme APS.
- `test_aps.ml` : point d’entrée dédié à l’exécution de la suites de tests.

#### `examples/`

Contient les programmes `.aps` utilisés comme exemples et comme base de tests, organisés par version du langage :

- `APS0` pour le noyau fonctionnel .
- `APS1` pour l’extension impérative .
- `APS1a` pour le passage par référence .
- `APS2` pour les tableaux et l’allocation mémoire.

#### `lib/FrontEnd/`

- `Parsing/lexer.mll` : analyse lexicale avec `ocamllex` .
- `Parsing/parser.mly` : analyse syntaxique .
- `ast.ml` : définition de l’arbre de syntaxe abstraite du langage.

#### `lib/Interpreter/`

- `Interpreter.ml` : logique principale d’évaluation .
- `types.ml` : définition des types utilisés côté interprétation .
- `helper.ml` : fonctions auxiliaires pour la gestion de l’évaluation.

#### `lib/Typer/`

Contient la partie liée au typage :

- `prologTerm.ml` : conversion du programme OCaml vers une représentation sous forme de termes Prolog .
- `typing_rules.pl` : règles de typage écrites en Prolog.

#### `lib/Utils/`

Contient les modules utilitaires partagés par le projet :

- `manip_sys.ml` : gestion pratique des fichiers, des chemins et des appels système .
- `pretty.ml` : utilitaires pour le pretty-printer dans [./lib/Typer/prologTerm.ml](./lib/Typer/prologTerm.ml).
- `tests_suits.ml` : Contients les entrées pour les suites de tests, avec les résultat attendu pour chaque programme, et des utilitaires.