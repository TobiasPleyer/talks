---
title: Einführung in die funktionale Programmierung
author: Tobias Pleyer
patat:
    incrementalLists: true
...

# Funktionale Programmierung

## Agenda

> 1. Grundlagen
> 2. Haskell Schnellkurs
> 3. Konzepte
> 4. Funktionale Programmierung in der Praxis
> 5. Warum funktionale Programmierung?
> 6. Beispiele & Demos

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
Programmiersprachen. Das heißt aber nicht dass man nicht auch in diesen
Sprachen von den im Folgenden vorgestellten Prinzipien profitieren kann.

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

. . .

Über diese Punkte hinaus kann man keine gesicherten Aussagen treffen.

. . .

_Beispiele_

- Haskell ist statisch typisiert, Clojure ist dynamisch typisiert
- Haskell kennt keine Klassen, F# kann .net-Klassen instanziieren
- In Haskell ist Lazy-Evaluation der Default, in Scala nicht
- Haskell hat Tail-Call-Optimization (TOC), Clojure und F# nicht

# Haskell Schnellkurs

## Syntax

- Die gesamte Syntax von Haskell ist darauf ausgelegt das Arbeiten mit
  Funktionen so einfach und knapp wie möglich zu machen
- In Haskell werden Funktionen ohne Klammern aufgerufen: `f x y z`
- Lambda-Funktionen in Haskell schreiben sich so: `\x y -> x + y` Zwischen
  Backslash und Pfeil stehen die Argumente und der Funktionskörper folgt nach
  dem Pfeil
- Jede Funktion in Haskell hat immer einen Datentyp, die sog. Signatur der
  Funktion, welche aber in der Regel nicht mit angegeben werden muss sondern
  nur optional ist
  Schreibweise: `myFunction :: String -> Int -> String`
- Funktionen welche Seiteneffekte haben können haben immer einen
  Rückgabewert von `IO`
  Beispiel: `getLine :: IO String`


# Konzepte

## Anmerkung

Die meisten Beispiele sind in der Programmiersprache Haskell gegeben. Dies hat
die folgenden zwei Gründe:

- Ich kenne mich mit Haskell relativ gut aus
- Haskell umfasst alle funktionalen Prinzipien über die ich sprechen werde

. . .

**Aber**

. . .

    Die hier vorgestellten Konzepte beschränken sich nicht auf Haskell und
    typischerweise wird jede funktionale Programmiersprache zumindest eine
    Teilmenge der hier vorgestellten Konzepte umfassen.

. . .

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

<!-- TODO: Maybe give a Rust example here. -->

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
benötigt wird. Das heißt **wird ein Ausdruck nie benötigt wird er auch nie
evaluiert**. Dieser Umstand erlaubt auch das Arbeiten mit unendlichen Listen und
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

Eine Funktion die als **Pure** kategorisiert ist darf **keine Seiteneffekte** haben,
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

*Zitiert von Wikipedia*

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

## Currying

Warum ist Currying nützlich?

Nehmen wir an wir haben folgende Funktion, welche die Daten eines Kunden
bearbeitet und die veränderten Daten anschließend abspeichern möchte.

. . .

```haskell
changeCustomerData :: (Customer -> IO ()) -> Customer -> IO ()
```

. . .

Die **Signatur einer Funktion ist wie ein Interface**. Wir können die Signatur
wie folgt lesen

. . .

    Gib mir eine Funktion mit der ich einen Kunden abspeichern kann und einen
    Kunden und ich werde meine Arbeit erledigen und die Daten sichern.

. . .

Nehmen wir nun an die folgenden Funktionen sind ebenfalls definiert

```haskell
saveCustomerDb    :: DbConn -> Customer -> IO ()
saveCustomerFile  :: FilePath -> Customer -> IO ()
saveCustomerCloud :: String -> String -> String -> Customer -> IO ()
```

. . .

Keine der obigen Funktionen erfüllt die erwartete Schnittstelle der Funktion
`changeCustomerData`.

. . .

**Aber**

. . .

Jede der folgenden Funktionen schon

```haskell
saveCustomer1 = saveCustomerDb dbconn
saveCustomer2 = saveCustomerFile filename
saveCustomer3 = saveCustomerCloud url username password
```

. . .

Jede der Funktionen `saveCustomerX` hat die Signatur `Customer -> IO ()`.
Weil wir die Funktionsparameter der Funktionen nur partiell anwenden entstehen
neue Funktionen welche alle die **gleiche Signatur** besitzen und somit das **gleichen
Interface** bedienen können!

. . .

Man könnte die partiellen Funktionen auch direkt als Argument übergeben

. . .

```haskell
main = do
  username <- askUsername
  password <- askPassword
  dbconn <- openDbConn username password
  customers <- fetchCustomers dbconn
  forM_ customers (changeCustomerData (saveCustomerDb dbconn))
```

## Currying

Weiteres Beispiel: Das Filtern von Listen

. . .

*Vergleiche die folgenden Versionen*

. . .

**Haskell**

```haskell
-- Find all lists that contain 42
l = [[1,3,5], [2,42,6], [5,7,42]]
hits = filter (elem 42) l
```

. . .

**Python**

```python
# Find all lists that contain 42
l = [[1,3,5], [2,42,6], [5,7,42]]
hits = filter(lambda x: 42 in x, l)
```

. . .

    In der Python-Version sind wir gezwungen die temporäre Listenvariable `x`
    explizit zu verwenden.

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

. . .

    Funktionale Programmierung mag manchmal befremdlich erscheinen für alle die
    die bereits eine "konventionelle" Sprache beherrschen. Dies liegt daran das
    vieles anders gemacht wird/werden muss.

. . .

    Ein Beispiel ist die Tatsache, dass es in der funktionalen Programmierung
    keine Anweisungen gibt, alles besteht aus Ausdrücken. Die Anwendung selbst
    ist ein einziger Ausdruck bestehend aus vielen (möglicherweise rekursiven)
    Sub-Ausdrücken.

. . .

**Ein Ausdruck hat immer einen Rückgabewert**

. . .

    => Es kann kein if ohne else geben

. . .

**Grund:** Für jeden möglichen Eingabewert muss es einen Rückgabewert geben, denn
ein Ausdruck kann immer einer Variablen zugewiesen werden.

. . .

    Aus dem selben Grund müssen beide if-Zweige auch den gleichen Datetyp
    zurück geben!

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

## Übersicht

Funktionale Programmierung in der Praxis bedeutet

> - Arbeiten mit Collections und Vermeidung von temporären Hilfsvariablen
> - Alles ist ein Ausdruck

_Fun Fact_

    Python's Lambda-Funktionen erlauben nur einen einzigen Ausdruck als ihren
    Funktionskörper, d.h. keine Anweisungen. Das bedeutet Python's
    Lambda-Funktionen akzeptieren jedes gültige funktionale Programm.

## Übersicht

Funktionale Programmierung in der Praxis bedeutet

> - Arbeiten mit Collections und Vermeidung von temporären Hilfsvariablen
> - Alles ist ein Ausdruck
> - Keine impliziten oder versteckten Abhängigkeiten

. . .

_Beispiel_

Vergleiche folgenden Haskell Code

```haskell
data Rect = Rect {
  width  :: Int,
  height :: Int
}

area :: Rect -> Int
area (Rect w h) = w * h


main = do
  let rect = Rect 3 4
  putStrLn ("rect area: " ++ (show (area rect)))
```

. . .

mit diesem C++ Code

```cpp
#include <iostream>
using namespace std;

class Rectangle {
    int width, height;
  public:
    Rectangle (int w, int h) : width(w), height(h) {};
    int area () {return (width*height);}
};

int main () {
  Rectangle rect (3,4);
  cout << "rect area: " << rect.area() << endl;
  return 0;
}
```

. . .

Die Klassenmethode `area` benutzt die Variablen der Klasse **implizit**. Die
Funktion selbst hat **keine Kontrolle über deren Werte**. Jeder Aufruf dieser
Methode kann zu einem **anderen Ergebnis** führen wenn die Variablen in der
Zwischenzeit in diesem oder einem anderen Thread verändert wurden!

. . .

Anders in Haskell: Hier liefert die Funktion immer **den selben Wert für den
gleichen Parameter**. Dies liegt daran das die Funktion ihr Ergebnis nur über
ihre eigenen Parameter berechnet. Es existieren keine versteckten Parameter.

. . .

In diesem einfachen C++ Beispiel ist der Zusammenhang leicht ersichtlich, aber
jeder der einmal versucht hat C++ Code über mehr als drei Vererbungslinien
nachzuvollziehen weiss dass das schnell ausufert!

## Übersicht

Funktionale Programmierung in der Praxis bedeutet

> - Arbeiten mit Collections und Vermeidung von temporären Hilfsvariablen
> - Alles ist ein Ausdruck
> - Keine impliziten oder versteckten Abhängigkeiten
> - Rekursion

. . .

    Rekursion ist ein fester Bestandteil der funktionalen Programmierung

. . .

_Beispiel_

```haskell
-- REPL in Haskell
main = do
  line <- getLine
  result <- evaluate line
  print result
  main
```

. . .

    Weil Haskell Code für diese sog. tails calls optimiert ist, führt dieses
    Beispiel im Gegensatz zu anderen Programmiersprachen nicht zu einem stack
    overflow.

## Übersicht

Funktionale Programmierung in der Praxis bedeutet

> - Arbeiten mit Collections und Vermeidung von temporären Hilfsvariablen
> - Alles ist ein Ausdruck
> - Keine impliziten oder versteckten Abhängigkeiten
> - Rekursion
> - Komposition

. . .

    Funktionale Programmierung fängt dann an ihre wahre Stärke auszuspielen
    wenn Funktionen effektiv miteinander kombiniert werden. Anders als Klassen
    lassen sich Funktionen sehr gut kombinieren.

. . .

**Fakt:** Klassen eignen sich nicht besonders gut als kleinste kombinatorische
Einheit - Funktionen schon.

. . .

_Beispiel_

```haskell
-- Assume the functions f1,f2,f3 exist and the types match
-- Then this
let new_func x = f3 (f2 (f1 x))
-- Can also be written like this in point-free notation
let new_func = f3 . f2 . f1
```

## Komposition und Point-Free-Style

**Point-Free-Style:** Die Funktionsargumente werden nicht explizit mit
angegeben, wenn diese unmissverständlich aus dem Zusammenhang hervorgehen

. . .

    Dies führt zu extrem kurzen und übersichtlichen Funktionsdefinitionen

. . .

_Beispiel_

Dieser Code stammt aus einem echten HTML Parser den ich geschrieben habe

```haskell
ingredients = ( map (T.unpack . T.unwords . map fromTagText . filter isTagText)
              . groupBy "tr"
              . convertFraction
              . filter notEmptyText
              . normalize
              . takeWhile (~/= "</table>")
              . tail
              . dropWhile (~/= "<table class=incredients>")) tags
```

Der Code muss wie bei einem Stack von Unten nach Oben gelesen werden.

Quelle:
*https://github.com/TobiasPleyer/chefkoch/blob/master/src/Chefkoch/Html/Parser.hs*

# Warum funktionale Programmierung?

## Vorteile

- Wartbarkeit
- Testbarkeit
- Extrem Ausdrucksstark
- Schließt eine Vielzahl von üblichen Fehlern aus
- Anerkannte Design-Patterns ergeben sich auf natürliche Weise

## Nachteile

- Verlangt Umdenken
- Weniger Leute auf dem Markt
- Abstraktionen und zusätzliche Sicherheiten verringern Performanz

# Beispiele

## FizzBuzz Kata

_Problemstellung_

    Write a program that prints the numbers from 1 to 100. But for multiples of
    three print “Fizz” instead of the number and for the multiples of five
    print “Buzz”. For numbers which are multiples of both three and five print
    “FizzBuzz “.

. . .

_Lösung_

```haskell
#!/usr/bin/env stack
{- stack script --resolver lts-11.8 --package base -}

import Data.Foldable (traverse_)

showValue :: Int -> String
showValue i
    | i `mod`  3 == 0 = "Fizz"
    | i `mod`  5 == 0 = "Buzz"
    | i `mod` 15 == 0 = "FizzBuzz"
    | otherwise       = show i

main = do
  traverse_ (putStrLn . showValue) [1..100]
```

## Parsen von Logdateien

## HTML Parser

## Einfacher Webserver

## Monadic Computations mit Python
