---
title: Einführung in die funktionale Programmierung
author: Tobias Pleyer
patat:
    incrementalLists: true
...

# Funktionale Programmierung

## Agenda

1. Grundlagen
    * Was bedeutet funktionale Programmierung?
    * Definition
    * Terminologie
    * Unterschiede zu imperativen und objektorientierten Sprachen
2. Konzepte
    * Algebraic Data Types
    * Lazy Evaluation
    * Purity
    * Currying
3. Funktionale Programmierung in der Praxis
    * Everything is an expression
    * if/else
    * Schleifen
    * Tail Call Optimization (TOC)
    * Komposition
    * Idiome
4. Beispiele
    * FizzBuzz Kata
    * Parsen von Logdateien
    * HTML Parser
    * Einfacher Webserver
    * Funktionale Programmierung mit Python
    * Monadic Computations mit Python

# Grundlagen

## Was bedeutet funktionale Programmierung?

Auf diese Frage gibt es keine standardisierte Antwort. Es kursieren viele
verschiede Definitionen. Eine die man oft hört ist:

. . .

    Eine Programmiersprache mit Funktionen höherer Ordnung sowie Funktionen als
    First-Class-Objects der Sprache.

. . .

**Aber:** Nach dieser Definition wären auch Python oder C++ funktionale
Programmiersprachen.

. . .

```python
>>> def f42(): return 42
...
>>> type(f42)
<class 'function'>
>>> def HighFunc(f): return f()+1
...
>>> HighFunc(f42)
43
>>>
```

. . .

Michael Snoyman, eine große Figur in der Haskell-Szene, hat dazu einen
interessanten Artikel verfasst.

_https://www.fpcomplete.com/blog/2018/10/is-rust-functional_

## Definition

Hier meine persönliche Definition einer funktionalen Programmiersprache

    Eine Sprache welche die effiziente Nutzung von Funktionen als elementaren
    logischen Baustein befürwortet, ermutigt und durch den Entwurf der Sprache,
    der Syntax und der Standardbibliothek fördert.

. . .

Nach dieser Definition sind Python und C++ keine funktionalen
Programmiersprachen.

. . .

*Die Standardbibliothek in die Definition mit einzubeziehen ist wichtig, weil
sie eine Art Gravitationsfeld hat: Man kann sich ihr nicht entziehen. Wenn die
Standardbibliothek einen gewissen Stil vorgibt dann nimmt das großen Einfluss
auf die gesamte Bibliothekenlandschaft der Sprache.*

## Gemeinsamkeiten aller funktionalen Programmiersprachen

- Funktionen, Funktionen, Funktionen
- Closures (Funktionsabschlüsse)
- Lambda-Funktionen
- Immutability
- Keine Anweisungen (Statements), alles ist ein Ausdruck (Expression)

Über diese Punkte hinaus kann man keine gesicherten Aussagen treffen.

_Beispiele_

- Haskell ist statisch typisiert, Clojure ist dynamisch typisiert
- Haskell kennt keine Klassen, F# kann .net-Klassen instanziieren
- In Haskell ist Lazy-Evaluation der Default, in Scala nicht
- Haskell hat Tail-Call-Optimization (TOC), Clojure und F# nicht

# Konzepte

## Anmerkung

Die meisten Beispiele sind in der Programmiersprache Haskell gegeben. Dies hat
die folgenden zwei Gründe:

- Ich kenne mich mit Haskell relativ gut aus
- Haskell besitzt fast alle Eigenschaften über die ich sprechen möchte

. . .

**Aber:** Die hier vorgestellten Konzepte beschränken sich nicht auf Haskell und
typischerweise wird jede funktionale Programmiersprache zumindest eine
Teilmenge der hier vorgestellten Konzepte umfassen.

Die folgenden Konzepte sind nicht direkt oder zwangsläufig Teil der
funktionalen Programmierung und sind weder eine Notwendigkeit noch
Ausschlusskriterium für funktionale Programmierung.

## Algebraic Data Types (ADT)

Programmiersprachen mit ADTs: Haskell, F#, Rust

ADTs sind zusammengesetzte Datentypen mit zwei Hauptklassen

- Summen-Typen (ODER)
- Produkt-Typen (UND)

. . .

_Beispiele_

```haskell
-- singly linked lists in Haskell
data List a = Nil
            | Cons a (List a)

[1,2,3] == 1 : 2 : 3 : [] == Cons 1 (Cons 2 (Cons 3 Nil))
```

. . .

```haskell
-- definition for a binary tree in Haskell
data Tree a = Empty
            | Leaf a
            | Node (Tree a) (Tree a)
```

## Algebraic Data Types (ADT)

Algebraische Datentypen erlauben Pattern-Matching

```fsharp
type Option<'a> =  // Generic, can hold every type
   | Some of 'a    // Some means we have a value
   | None          // None means the abscence of a value

let myOption = Some 42

match myOption with
| Some i -> printfn "My Option holds value %A" i
| None   -> printfn "My Option holds no value"

```

. . .

F#'s Standardbibliothek macht regen Gebrauch von Options, z.B. List.tryFind

. . .

Von der Dokumentation auf *https://msdn.microsoft.com*

*// Signature:*
List.tryFind : ('T -> bool) -> 'T list -> 'T option
*// Usage:*
List.tryFind predicate list

    Returns the first element for which the given function returns true.
    Returns None if no such element exists.

## Lazy Evaluation

Programmiersprachen mit Laziness: Haskell, Clojure, F#

Lazy-Evaluation bedeutet das ein Ausdruck nur dann evaluiert wird wenn er
benötigt wird. Das heißt wird ein Ausdruck gar nicht benötigt wird er auch nie
evaluiert. Dieser Umstand erlaubt auch das Arbeiten mit unendlichen Listen und
anderen Datentypen welche unter strikter Evaluierung sämtlichen Speicher
verbrauchen würden.

. . .

_Beispiele_

```haskell
-- Definition of the factorial function
fac :: Integer -> Integer
fac n
  | n <= 1 = 1
  | otherwise = n * (fac (n-1))

main = do
    print $ fac 5 -- fac 5 is evaluated because it is printed
    let bigFac = fac 10000000 -- used nowhere -> not evaluated
    putStrLn "Done"
```

. . .

```haskell
let h = head [1..] -- get the first element of an infinite list
```

## Purity

Programmiersprachen mit Purity: Haskell

Eine Funktion die als Pure kategorisiert ist darf keine Seiteneffekte haben,
d.h. IO-Aufrufe vornehmen oder Variablen außerhalb des eigenen Funktionsscope
manipulieren. Es gibt nur wenige Programmiersprachen welche dies über den
Compiler und das Typesystem sicherstellen, Haskell ist eine davon.

. . .

```haskell
myFunc :: String -> IO () -- A function with side effects must have type IO
myFunc = putStrLn

main = myFunc "Hello impure World!"
```

. . .

```haskell
readFromFile :: String -> String
readFromFile filename = readFile filename
```

```
error
Couldn't match type ‘IO String’ with ‘[Char]’
Expected type: String
  Actual type: IO String
```

## Currying

Programmiersprachen mit Currying: Haskell

Zitiert von Wikipedia

    Currying is the technique of translating the evaluation of a function that
    takes multiple arguments into evaluating a sequence of functions, each with
    a single argument.

. . .

_Beispiele_

```haskell
add5 = (+5)
add5 17 == 22
```

. . .

```python
>>> from functools import partial
>>> curry = partial
>>> def plus(x,y): return x+y
>>> add5 = curry(plus, 5)
>>> add5(17) == 22
True
```

# Funktionale Programmierung in der Praxis

## Übersicht

Funktionale Programmierung in der Praxis bedeutet

- Arbeiten mit Collections und Vermeidung von temporären Hilfsvariablen

. . .

_Beispiel_

Vergleiche folgenden idiomatischen Haskell Code

```haskell
let s  = foldr (+) 0 [1..10]
let s2 = sum [1..10] -- even shorter
```

. . .

mit dem entsprechenden idiomatischen C++ Code

```cpp
int a[] = {1,2,3,4,5,6,7,8,9,10};
int i,s,l;
s = 0;
l = sizeof(a) / sizeof(*a);
for(i=0; i<l; i++)
{
    s += a[i];
}
```

## Übersicht

Funktionale Programmierung in der Praxis bedeutet

> - Arbeiten mit Collections und Vermeidung von temporären Hilfsvariablen
> - Alles ist ein Ausdruck

## Alles ein Ausdruck

Funktionale Programmierung mag manchmal befremdlich erscheinen für alle die die
bereits eine "konventionelle" Sprache beherrschen. Dies liegt daran das vieles
anders gemacht wird/werden muss.

. . .

Ein Beispiel ist die Tatsache, dass es in der funktionalen Programmierung keine
Anweisungen gibt, alles besteht aus Ausdrücken. Die Anwendung selbst ist ein
einziger Ausdruck bestehend aus vielen (möglicherweise rekursiven)
Sub-Ausdrücken.

. . .

**Ein Ausdruck hat immer einen Rückgabewert**

## if/else

Alles muss ein Ausdruck sein, das gilt auch für if/else Konstrukte. Als
Konsequenz darf es kein if ohne else geben.

. . .

**Grund:** Für jeden möglichen Eingabewert muss es einen Rückgabewert geben, denn
ein Ausdruck kann immer einer Variablen zugewiesen werden.

. . .

Aus dem selben Grund müssen beide if-Zweige auch den gleichen Datetyp zurück
geben!

. . .

_Beispiel_

Python besitzt eine funktionale (einzeilige) Version von if/else um einer
Variablen einen Wert zuzuweisen. In diesem Fall muss immer ein else-Zweig
vorhanden sein

```python
>>> x = 42
>>> y = 5 if x > 0
  File "<stdin>", line 1
    y = 5 if x > 0
                 ^
SyntaxError: invalid syntax
>>> y = 5 if x > 0 else -1
>>> y
5
```

## if/else

In Haskell kann sich die Zuweisung über mehrere Zeilen strecken. Aber auch hier
müssen alle möglichen Pfade den gleichen Rückgabetyp besitzen.

```haskell
-- Simple echo application
main = do
  l <- getLine
  if l == "quit"
  -- main always has type IO (), so we have to return the singelton value ()
  then return ()
  -- the return value can be arbitrarily complex, event recursive, but the
  -- overall type has to be IO ()
  else do
    putStrLn l
    main
```

## Explizite Funktionsparameter

## Rekursion und Tail Call Optimization (TOC)

## Komposition

# Beispiele

## FizzBuzz Kata

## Parsen von Logdateien

## HTML Parser

## Einfacher Webserver

## Monadic Computations mit Python
