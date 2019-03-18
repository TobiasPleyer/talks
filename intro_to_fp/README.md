# Einführung in die funktionale Programmierung

Die Beispiele für diesen Vortrag sind in Haskell und Python geschrieben.

Um die Beispiele ausführen zu können wird Folgendes benötigt:

    * Git
    * stack (https://docs.haskellstack.org/en/stable/README/)
    * Python3.6 oder höher
    * pyparsing (https://pypi.org/project/pyparsing/)

Wer die HTML Folien auch produzieren möchte braucht auch noch das hier

    * pandoc (https://pandoc.org/)

## Kurze Erklärung zu stack

*stack* ist ein Tool zur einfachen Erstellung von Projekten in der
Programmiersprache Haskell. Zusätzlich erleichtert *stack* die Handhabung von
Abhängigkeiten zu anderen Paketen.

*stack* benutzt intern den *Glasgow Haskell Compiler* (ghc) um den Quellcode zu
kompilieren. Der Download von ghc kann einige Zeit in Anspruch nehmen, deshalb
empfehle ich frühzeitig den Download zu starten.

Dazu bitte folgendes machen:

1. Kommandozeile öffnen (egal ob shell oder cmd)
2. `cd` in den Ordner `code/logparser/logparser_project`
3. `stack build` eingeben

Von da an macht *stack* den Rest alleine. Wenn alles durchgelaufen ist befindet
sich ghc-8.6.3 auf dem Rechner. Außerdem sollte der Build erfolgreich sein.
