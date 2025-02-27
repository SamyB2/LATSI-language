# GAS-Project

## Compilation et Nettoyage

- Pour compiler le projet, exécutez la commande suivante : 
```shell 
./exec.sh build
```
- Pour nettoyer le répertoire de travail, exécutez la commande suivante : 
```shell
./exec.sh clean
```
## Exécution

- Après la compilation, un lien vers le fichier exécutable nommé **gas_project** sera créé.
- Pour exécuter le programme, utilisez la commande : 
```shell
./gas_project
```
- Cela ouvrira un terminal où vous pourrez écrire votre programme Latsi.
- Vous pouvez également rediriger l'entrée standard vers un autre fichier en utilisant la commande 
```shell
./gas_project < fic
```
- Le fichier `execTests.sh` permet d'exécuter les fichiers tests présents dans `Testing-files`
- Pour s-y faire vous devez executer la commande suivante :
```shell
./execTests.sh
```

## Fonctionnalités Implémentées

- Génération de tokens avec ocamllex.
- Définition d'une grammaire avec menhir.
- Construction d'un arbre de syntaxe abstrait.
- Exécution d'un programme LATSI avec l'instruction VEVERS.
- Extension 1 : Déclarations multiples de variables, par exemple `I = 1 , A = 2 * I , B = A - I`.
- Extension 2 : Fonctions MIN et MAX avec la syntaxe `MIN (expression {, expression})` et `MAX (expression {, expression})`.
- Extension 3 : Ajout d'une instruction SINON supplémentaire. 
    Il est désormais possible de compléter ou pas l'instruction `SI instruction ALORS instruction` 
    avec `SINON instruction`.
- Extension 4 : Ajout des `&&` `et` , `||` `ou` et `!` `not` pour les conditions

## Répartition des Tâches

**Matyas MARQUES MARTINS :**
- Définition des types Ocaml pour l'arbre de syntaxe abstrait.
- Élaboration de la grammaire du langage LATSI avec menhir.
- Ajout de l'extension 1.
- Ajout de l'extension 4.

**Sami BELMELLAT :**
- Génération des jetons avec ocamllex.
- Évaluation de l'arbre de syntaxe généré.
- Ajout des extensions 2.
- Ajout des extensions 3.

**Travail réalisé en commun :**
- Résolution des conflits SHIFT/REDUCE et REDUCE/REDUCE.
- Création de fichiers de test pour chaque version du projet.
- Organisation du répertoire du projet.
- Documentation du code.

## Auteurs
**Matyas MARQUES MARTINS**
**Sami BELMELLAT**
