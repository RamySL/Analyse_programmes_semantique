# Projet APS
Réalisé dans le cadre du cours d’**Analyse de Programmes et Sémantique** en M1 **STL** à Sorbonne Université.  
Projet réalisé par **Ramy SAIL**.

## Description générale

Ce projet consiste à concevoir et implémenter un interprète pour le langage **APS**, construit progressivement à travers plusieurs extensions successives.

Le langage est développé par étapes, chaque version ajoutant de nouveaux mécanismes classiques de la conception des langages :

- **APS0** : noyau fonctionnel, avec définitions de constantes et de fonctions, abstractions fonctionnelles, expressions conditionnelles et affichage `ECHO`.
- **APS1** : extension impérative avec variables, affectation, conditionnelle, boucle `WHILE` et procédures.
- **APS1a** : ajout du passage de paramètres par référence avec `var` et `adr`.
- **APS2** : ajout des tableaux, de l’allocation mémoire explicite et des opérations `alloc`, `len`, `nth`, `vset`.

Au-delà de l’implémentation elle-même, ce projet met en valeur plusieurs aspects intéressants, à la fois du point de vue théorique et pratique :

- une **définition formelle** du langage à chaque étape, avec une séparation nette entre la **syntaxe**, les **règles de typage** et la **sémantique d’évaluation** 
- l’étude de la transition entre un **langage purement fonctionnel** et un **langage impératif avec mémoire** 
- l’introduction progressive de notions centrales en théorie des langages comme les **fermetures**, la **récursion**, les **procédures**, le **passage par valeur et par référence**.
- l’utilisation de **Prolog** pour exprimer le typage sous forme de règles logiques, ce qui permet de rester très proche des règles formelles du cours.
- un projet à l’intersection de plusieurs domaines : **langages de programmation**, **sémantique formelle**, **programmation fonctionnelle**, **programmation logique** et **tests**.

Ce dépôt illustre ainsi une démarche complète de construction de mini-langages : partir d’un noyau simple, puis enrichir progressivement le système tout en maintenant une cohérence entre la théorie présentée en cours et l’implémentation.

## Prérequis

Le projet a été développé en **OCaml** avec **Dune** et **Prolog** pour le typeur.
Versions utilisées :
- **OCaml** : 5.4.0
- **Dune** : 3.21.0
- **Prolog** : 9.0.4

## Utilisation

Le projet fournit deux points d’entrée principaux :

- [bin/main.ml](bin/main.ml) pour exécuter un programme APS donné en argument ;
- [bin/test_aps.ml](bin/test_aps.ml) pour lancer une suite de tests.

**Execution par fichier**

L’exécutable principal prend en argument le chemin vers un fichier `.aps`.

Commande :

```bash
dune exec main chemin/fichier.aps
```
En sortie, le programme affiche :

- le terme Prolog qui représente l'AST du programme et qui sera passé au typeur.
- le résultat du typage (OK ou KO) ;
- puis, si le typage réussit, les valeurs produites par l’évaluation.

**Execution par pile de tests**

L’exécutable `test_aps` permet de lancer automatiquement les programmes de test présent dans [examples](examples).

**Lancer tous les tests**

```bash
dune exec test_aps
```

Sans argument, la suite complète est lancée pour toutes les versions disponibles.

**Lancer les tests d’une version donnée**

```bash
dune exec test_aps  0
dune exec test_aps  1
dune exec test_aps  1a
dune exec test_aps  2
```
Les tests vérifient à la fois :

- le résultat attendu du typage ;
- la sortie attendue de l’évaluation ;
- ou, dans certains cas, qu’une erreur d’exécution est bien levée.

**Utiliser sa propre pile de tests**

La suite de tests est centralisée dans [lib/Utils/tests_suits.ml](lib/Utils/tests_suits.ml).

Chaque campagne de tests y est décrite par une liste de triplets de la forme :

```ocaml
(string * string * int list option)
```

Chaque triplet correspond à :

- le **nom du fichier** à exécuter ;
- le **résultat de typage attendu** (`"OK"` ou `"KO"`) ;
- le **résultat d’évaluation attendu** :
  - `Some [...]` si l’on attend une sortie précise ;
  - `None` si l’on attend une erreur d’évaluation.

**Ajouter un test à une version existante**

Pour ajouter un nouveau test à une version déjà présente, il suffit de rajouter un fichier `.aps` dans `examples` dans le sous dossier de la version correspendante et ensuite rajouter une entrée dans la liste comme par exemple :

```ocaml
let l_test_2 = [
  ...
  (Manip_sys.testfile_name "2" 17, "OK", Some [42]);
]
```

Ici, `Manip_sys.testfile_name "2" 17` permet de construire automatiquement le chemin du fichier de test correspondant à la version `2` et au programme `prog17.aps`.

## Difficultés rencontrées

Une des principales difficultés du projet a été le fait qu’il pouvait y avoir parfois des incohérences dans les spécifications des différentes versions du langage.

Par exemple, en **APS1**, la mémoire est présentée comme une fonction des adresses vers les entiers. Mais en **APS2**, on voit qu’elle est utilisée comme une fonction vers toutes les valeurs du langage, alors qu'en APS2 elle est étendue à juste les bloques mémoire en plus des entiers.

L’autre difficulté importante a été l’utilisation de **Prolog** pour le typage. Ce n’est pas seulement un nouveau langage à apprendre, c’est aussi une manière différente de programmer et de réfléchir. D'habitude, on pense facilement en termes de calcul, de structures de données et de fonctions. Avec Prolog, il faut plutôt raisonner en termes de règles, de relations et de recherche de solutions.

La difficulté principale venait surtout du **backtracking**. Quand une règle est trop générale, Prolog peut revenir en arrière et essayer d’autres chemins, ce qui peut produire des comportements inattendus ou des résultats faux. Ce mécanisme est puissant, mais il peut aussi rendre certaines erreurs difficiles à comprendre.

Le débogage était aussi parfois pénible avec `trace.`. Cet outil est utile, mais quand l’exécution devient longue ou qu’il y a beaucoup de retours en arrière, la trace devient vite difficile à lire et à suivre. Pour certaines erreurs, cela rendait le débogage assez long et fatigant.
