Mini-projet 1 : solveur DPLL récursif
=====================================

Objectif du mini-projet
-----------------------

Le but du mini-projet est d'implémenter un solveur DPLL récursif en
OCaml. Vous pouvez partir de ce code en forkant ce projet Gitlab, votre projet devra 
être privé.

Vous devez compléter pour cela le code dans le fichier `dpll_solver/lib/dpll.ml` :

 - la fonction `simplifie : literal -> cnf -> cnf `
 - la fonction `unitaire : cnf -> literal option`
 - la fonction `pur : cnf -> literal option`
 - la fonction `solveur_dpll_rec : cnf -> interpretation -> interpretation option`

Les types et les commentaires dans `dpll.ml` sont indicatifs. D'autres
choix peuvent être pertinents; par exemple, `unitaire` et `pur`
pourraient aussi être de type `int list list -> int` et utiliser les exceptions. 
Aussi, les définitions `let foo ...` peuvent être transformées en `let rec foo ...` ou
inversement.

Documentation 
-------------

Une attention particulière sera portée sur la documentation de votre
code : la pertinence et la clarté de cette dernière comptera pour un
pourcentage important de votre note finale. Vous devrez donc soigner
les explications qui accompagneront votre code et ce sur deux volets
principaux :

1. Le fichier `RENDU` contient des questions précises sur votre
   implémentation. Vous êtes requis d'y répondre en complétant le même
   fichier, qui devra faire partie de votre rendu final. Vos réponses
   doivent être claires, précises et préférablement succinctes. À la
   lecture de vos réponses, une compréhension globale, sans ambiguïté,
   de votre implémentation devra facilement se dégager.

   Un mini-projet sans fichier `RENDU` rempli ne recevra **pas de note**.

2. Vous devez impérativement commenter votre code dans le but de
   complémenter les explications fournies lors du remplissage du
   fichier `RENDU` (volet 1 ci-dessus). Dans le même esprit, vos
   commentaires doivent être clairs et précis, ils doivent donner une
   compréhension plus fine des détails de votre implémentation.

   Un code non commenté entraînera automatiquement une note finale
   lourdement pénalisée.

Compiler son projet
-------------------

Le projet est fourni avec les fichiers de configuration permettant de le compiler avec le système `dune`.
`dune`et les dépendances nécessaires ont été installées sur les machines de l'UFR d'Informatique.
(Sous un système de type debian/ubuntu/…, cela peut nécessiter l'installation de paquets via la commande `sudo apt-get install ocaml-dune libppx-inline-test-ocaml-dev libppx-inline-test-ocaml libppx-assert-ocaml libppx-assert-ocaml-dev libppx-hash-ocaml libppx-hash-ocaml-dev libppx-enumerate-ocaml libppx-enumerate-ocaml-dev libjane-street-headers-ocaml libjane-street-headers-ocaml-dev`)

Pour compiler votre programme, placez vous dans le répertoire `dpll_solver` et tapez:
```
dune build
```

Il est normal de voir des *Warning* qui indiquent des variables non-utilisées dans le code à compléter.

Pour exécuter votre programme, il faut utiliser la commande:
```
dune exec dpll_solver ../examples/SAT/sudoku-4x4.cnf
```

Code correct vs. code optimisé
------------------------------

Assurez-vous que votre code compile avant de le rendre.

En outre, vous devez prioriser vos objectifs d'une manière saine : un
code correct et lent est meilleur qu'un code erroné et
rapide. Résolvez le problème, puis optimiser votre code (_do it right,
then do it better_). Une implémentation optimisée n'aura un impact
positif sur votre note finale que si elle est correcte.

Tester son mini-projet
----------------------

Pour tester votre projet vous devez d'abord tester la correction :
  - écrire des tests unitaires, qui testent la correction de chacunes des fonctions du module dpll. Pour cela, vous pouvez utiliser les tests _inline_, des exemples sont donnés dans le fichier `dpll.ml`.
  Pour lancer les tests vous pouvez lancer la commande:
  ```
  dune test
  ```

  - puis écrire des tests qui vérifient que votre programme se comporte correctement de manière globale sur les exemples fournis. Votre programme ne va pas résoudre forcément tous les exemples en un temps raisonnable, mais quand il termine il doit renvoyer le bon résultat. Comment tester votre programme ? Vous pouvez saisir à la main le résultat obtenu mais aussi utiliser des tests dos à dos et/ou écrire une fonction qui vérifie le résultat dans le cas SAT.

Ensuite vous pouvez passer aux tests de perfomance :
  - sur chacune des fonctions intermédiaires
  - sur le programme global  

Outre les exemples inclus dans le répertoire test (`exemple_3_12`,
`exemple_7_2`, `exemple_7_4`, `exemple_7_8`, `systeme`, `coloriage`), vous
pouvez utiliser `dune` pour construire le projet depuis le répertoire `dpll_solver` en appelant
```
dune build
```
pour compiler un exécutable natif et le tester sur des fichiers au
format DIMACS. 

Vous trouverez des exemples de fichiers dans le repertoire `examples`.

Par exemple,
```
dune exec dpll_solver ../examples/SAT/sudoku-4x4.cnf
```
devrait répondre
```
SAT
-111 -112 113 -114 -121 -122 -123 124 -131 132 -133 -134 141 -142 -143 -144 -211 212 -213 -214 221 -222 -223 -224 -231 -232 -233 234 -241 -242 243 -244 311 -312 -313 -314 -321 322 -323 -324 -331 -332 333 -334 -341 -342 -343 344 -411 -412 -413 414 -421 -422 423 -424 431 -432 -433 -434 -441 442 -443 -444 0
```

⚠️ Si vous faites des affichages pour vos propres tests, il faudra penser
à commenter ces tests pour que votre programme répondre exactement
comme indiqué juste ci-dessus. 

Indication de barème
--------------------

Un projet bien commenté et bien expliqué et qui fonctionne : 8/10
Si en plus la correction et les perfomances ont été bien testées et la méthodologie de test expliquée : 9/10 
Si vous vous avez optimisé les perfomances en introduisant des structures de données, des heuristiques, ou des choix algorithmiques pertinents, et que la perfomance est bien évaluée sur des benchmarks : 10/10

Rendre son mini-projet
----------------------

 - date limite : **25 octobre 2025, 18h59**
 - sur la page Moodle du cours
     https://moodle.u-paris.fr/course/view.php?id=1657
 - sous la forme d'une archive XX-nom1-nom2.zip où `XX` est le numéro
   de binôme déclaré à la page
     https://moodle.u-paris.fr/mod/choicegroup/view.php?id=82687
   et `nom1` et `nom2` sont les noms de famille des deux membres du
   binôme, contenant l'arborescence suivante :
     XX-nom1-nom2/dpll.ml
     XX-nom1-nom2/dimacs.ml
     XX-nom1-nom2/Makefile
     XX-nom1-nom2/RENDU
