## Language Guide

DreamLisp is a Lisp dialect that started out as an implementation of [MAL (Make a Lisp)](https://github.com/kanaka/mal/) and then diverged from the MAL language spec to include additional functionalities like modules, lazy collections and changes to some of the core functions, and is a Lisp-2 because we can have both variable and functions with the same name. DreamLisp is written in Objective-C and as such can be extended further to work with Objective-C runtime capabilites.

### Data Types

All data types in DreamLisp are immutable in nature.

#### List

Lists are the building blocks for any Lisp program. Lists unlike vector, which we will see later, has special semantics however, in general. The first element of the list is almost always considered as a function and the rest as the argument to that function.

```
user> (+ 1 2 3 4 5 6)
21
```

Here, `+` is a function and `1`, `2`, `3` are arguments. A list always evaluates the elements. To supress evaluation, we can quote the list.

```
user> (quote (list 1 2 3))
(1 2 3)

user> '(1 2 3)
(1 2 3)
```

Here both the expressions are same. The second expression is a short version which uses `'` quote symbol which has this special semantics, which being supress expression evaluation.

```
user> (first (list 1 2 3))
1

user> (first '(1 2 3))
1

user> (first (1 2 3))
[error] Expected 'function' but obtained 'number'
```

Here the `(list 1 2 3)` produces a list. The first argument `list` is a function which creates a list out of the given elements. We then apply the result to the `first` function which returns the first element from the list.
Note that, the inner expression is evaluated and the result is passed to the outer `first` function.

The second expression uses list notation form. When using list as notation, the first argument is the number `1`, which is not a function. So if we did not quote the list, the list will be evaluated with `1` assumed as function with arguments `2`, `3`, `4`, which will fail, as in the thrid sample expression. So we supress the evaluation passing the list as such.

#### Vector

Vectors are similar to lists, except that vectors does not evaluate the values like list does. So we can intuit it like a quoted list.

```
user> [1 2 3]
[1 2 3]

user> (first [1 2 3])
1

user> (rest (vector 1 2 3))
[2 3]
```

The vector notation is `[]` squre braces. We can use the function `vector` to create a vector as well. The `rest` is a function that operates on sequences (`list`, `vector`, `string`) returning every element except the first. This is same as `head`, `tail`, or `car`, `cdr` functions one will encounter in other Lisp dialects.

#### Hash Map

Hash maps are dictionary collection which can have key value pair. The short notation for hash-map is `{}`.

```
user> {"name" "Olive", "age" 42, "location" "Liestal"}
{"name" "Olive" "age" 42}

user> (def person {:first-name "Jane" :last-name "Doe" :location "Montreux"})
{:first-name "Jane" :last-name "Doe" :location "Montreux"}

user> (:first-name person)
"Jane"

user> (get :last-name person)
"Doe"
```

Hash map does not gurantee ordering of elements. Here the first expression uses strings as keys, where as the second one uses a keyword. Keywords has same sematics as that of Clojure keyword. We use `def` to bind a value to a variable. Here we bind the hash-map to the variable `person`. 

When using keyword in the function position of a list with argument as a hash-map, we get the value associated with that key in the hash-map if present, else `nil`. If we are using a different key type, we can use the `get` function to get the value associated with the key as in the last expression.

#### Keyword

Keywords are denoted using an atom with `:` prefix. 

```
user> :sunday
:sunday

user> (def weekend [:saturday :sunday])
```
The last example shows a weekend variable which is a vector containing keywords.

#### String

String are anything within a double quotes. Most of the list functions also works on strings. 

```
user> (first "Olive")
"O"

user> (concat "Jane" " " "Doe")
"Jane Doe"
```

#### Atom

Atoms are a container holding another value type where operations on the atom takes place atomically. At present DreamLisp is only single threaded from user's perspective, so this distinction does not hold much.

```
user> (atom {:count 2 :displayed? false})
(atom {:displayed? false :count 2})
```

#### nil

All functions returns value, the default being `nil`.

```
user> nil
nil
```

### Function

A function represents a unit of computation with a set of expressions grouped together. Functions can be named or anonymous. Functions are tail recursive.

```
(defun inc (n)
  "Increment the given number"
  (+ n 1))
  
(defun dec (n)
  "Decrement the given number"
  (- n 1))
```

Here `inc/1` and `dec/1` are two functions which takes a argument `n` and increments or decrements by one. The string given after the argument is the doc string.

Note: When using the REPL, on a line break the expression will be evaluated. We can define the function without line breaks or we can load from a file.

```
user> (inc 41)
42
```

### let

`let` is used for local binding within an expression. The variables defined within `let` is only visible within its lexical scope and also available within a closure.

```
(let [name-xs ["Olive" "Jane" "Chole"] 
      greet (fn (n)
              (doall (map (fn (x) (println "Hello" x)) n)))]
  (greet name-xs) 
  nil)
    
; Hello Olive
; Hello Eva
; Hello Jane
; nil
```
Here we define variables `name-xs` which is a vector of names and an anonymous function `greet` that takes a list of names and print a message to the console. 

### Macros

Now to the best part, macros! Macros as the Lisp lore says are programs, that write programs. Macros are compile time only in the sense that when we apply a macro within some function, and later change the macro, the existing defined function does not change, because that code was already produced from the previous macro expansion when the function was defined. To make variable names to not have conflicts as the macros are unhygienic, we can make use of `gensym` which produces unique symbols through out.

```
(defmacro when (x & form)
  "Evaluates the first expression and if `true`, evaluates the body."
  `(if ~x (do ~@form)))
```

Here we require a macro because we require conditional evaluation of arguments. If we use a function instead, all the arguments passed to the function will be first evaluated which is not what we want. The `when` macro evaluates the expression first and if it is `true`, it then executes the body. We use special reader macros that are available only within a macro definition. These have the standard Lisp semantics.

The back tick, which is unquote \`, is used to write code as is. Within the unquote, using tilde `~`, which is unquote evaluates the variable that follows and replaces it with its value. If the variable is a list, we use splice-unquote `~@`. Invoking macro will then replace the call site with the code, which is called the macro expansion. Macros can be considered like templating in a sense, but it's not mere string substitution.

## Module

DreamLisp has a module system similar to that of Erlang or [LFE](https://lfe.gitbooks.io/quick-start/3.html) and uses similar function notations with the MFA, which is the Module, Function, Arity. Since the language supports `n` arity functions, which are functions that can accept variable number of arguments, we represent them using `fn/n`.

```
(defmodule utils (export (collatz 1)))

(defun collatz (n)
  (collatz n []))

(defun collatz (n acc)
  (if (not= n 1)
    (if (even? n)
      (collatz (/ n 2) (conj acc n))
      (collatz (+ (* 3 n) 1) (conj acc n)))
    (conj acc n)))
```

Say, this is the `utils.dlisp` file, which contains `collatz` function. The filename and module name needs to be the same as a convention. Here we define a module using `defmodule` followed by the module name and then the module exports. The exports are function name followed by arity. Similarly if we are importing from another module, we can use the `import` special form. Only exported functions and macros are visible to the other modules.

```
(defmodule utils-test 
  (export all)
  (import (from test (deftest n) (testing n) (is 2) (is 3) (run 1))))
``` 

Here we are importing `deftest` which takes a variable number of arguments which is represented by `n`. `is` takes only two arguments, so its arity is `2`.

The default module is called `user` which is the module that the REPL initializes into. REPL also loads one other module which is the `core`.

## Lazy sequence

Higher order collection functions like `map/n`, `foldl/3`, `foldr/3`, etc., by default produces a lazy sequence. The operations are deferred until we force evaluation either by applying other list functions over the lazy sequence like `first/1` or `take/2`, or using `doall/1`, `dorun/1`.

## Core Functions

DreamLisp has a set of built-in core functions which are natively implemented and helpers which are written in DreamLisp itself and loaded into the REPL during initialization. These core functions are in the module `core` and is availble to all other modules without having to explictly import them.

### Sequence Functions
Sequence functions works on `list`, `vector` and `string` types.

#### list

Create a new list.

```
user> (list "Coleridge" "Frost" "Hardy")
("Coleridge" "Frost" "Hardy")
```

#### list?

`list?` is a predicate function which checks if the given element is a list type.

```
user> (list? [1 2 3])
false

user> (list? '("beauty" "and" "the" "beast"))
true
```

### vector

Create a new vector.

```
user> (vector "Austen" "Brontë" "Woolf")  ;; create a vector
["Austen" "Brontë" "Woolf"]
```

#### vector?

`vector?` is a predicate function which checks if the given element is a vector type.

```
user> (vector? [1 2 3])
true

user> (vector? "castle in the sky")
false
```

### first

```
user> (first '(42 43 44))
42

user> (first ["Oxford" "London" "Cambridge"])
"Oxford"
```

The `first` function returns the first element of a sequence if present or `nil`.

#### rest

```
user> (def uni "York")
"York"
user> (rest ["Manchester" uni "Oxford" "Cambridge"])
["York" "Oxford" "Cambridge"]
user>(rest "amnesia")
["m" "n" "e" "s" "i" "a"]
```

The `rest` function takes a sequence and returns another sequence with all elements except the first one or an empty sequence if no element are present.


#### empty?

`empty?` is a predicate function that checks if the given sequence is empty.

```
user> (empty? '())
true

user> (empty? [])
true

user> (empty? [5 7 13 17])
false

user> (empty? "")
true

user> (empty? "rainbows and unicorns")
false
```

#### count

Returns the count of the given sequence.

```
user> (count '("a" "e" "i" "o" "u"))
5

user> (count ["Luna" "Hermione" "Harry"])
3

user> (count "opposite opposite")
17
```

#### cons

Takes any element and adds it to the first of the given list, returning the new list.

```
user> (cons 1 '(2 3 4))
(1 2 3 4)

user> (cons '(1) '(2 3 4))
((1) 2 3 4)
```

#### concat

Takes a list of sequences and combines them into one sequence.

```
user> (concat '(-2 -1 0) '(1 2 3 4))
(-2 -1 0 1 2 3 4)

user> (concat ["hello"] ["world"])
["hello" "world"]

user> (concat "Bul" "bul")
"Bulbul"

user> (concat "After" [1 2 "B"] "C")
"After12BC"
```

#### nth

Returns the nth element from the sequence.

```
user> (nth 1 [3.14 159 265])
159

user> (nth 0 [35 89 79])
35

user> (nth 2 "NSValue")
"V"
```

#### nth-tail

Takes a start, an end index, a sequence and returns a sub-sequence within the given indices inclusive.

```
user> (nth-tail 2 4 '(0 1 2 3 4 5))
(2 3 4)
```

#### map

`map` is a higher order function which takes a function and a sequence and applies the function to each element in the sequence.

```
(map (fn (x) 
```



## Loading code from file
