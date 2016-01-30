
# Compétition informatique 2016

## Description

Ce repo contient certaines des questions et des réponses de l'édition
2016 de la compétition informatique du CegInfo et du CegL. Les
participants avaient aussi à travailler sur d'autres questions écrites
par d'autres étudiants et sur le volet des questions d'entreprises.

La compétition est disponible sur
[hackerrank](https://www.hackerrank.com/competition-informatique-ceginfo-cegl-2016).

## Format des épreuves

Les paticipants devaient rédiger des programmes qui lisent certaines
entrées et dont les sorties sont vérifiées avec celles de la bonne
solution pour résoudre les problèmes.

Les énoncés des problèmes sont rédigés en markdown dans des fichiers
Org de Emacs et suivent le format d'épreuve de Hackerrank. Les
solutions sont légèrement commentées afin de clarifier la démarche.

Des scripts pour générer les cas de test servant à vérifier les
soumissions des participants sont aussi présents dans chaque
répertoire d'épreuve.

## Comment utiliser ce répertoire

* Lisez les énoncés des problèmes dans `<problème>/problem/problem.org`

* Accédez aux solutions dans `<problème>/solution`

* Obtenez les test cases en allant dans `<problème>/problem/test_cases`
  et en faisant `make`

#### Note

Il est à noter que les makefiles sont très peu propres et qu'ils ont
été faits avec l'approche "tant que ça marche". Le code de génération
de cas de test est souvent lent, comme il n'était pas une priorité
qu'il s'exécute rapidement.