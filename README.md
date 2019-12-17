## Language Guide

DreamLisp is a Lisp dialect that started out as an implementation of [MAL (Make a Lisp)](https://github.com/kanaka/mal/) and then diverged from the MAL language specification to include additional functionalities like modules, lazy collections and changes to some of the core functions, and is a Lisp-2 because we can have both variable and functions with the same name. DreamLisp is written in Objective-C and as such can be extended further to work with Objective-C runtime capabilites. At present the language runs in interpreted mode.

### Data Types

All data types in DreamLisp are immutable in nature.

#### List

Lists are the building blocks for any Lisp program. Lists unlike vector, which we will see later, has special semantics however, in general. The first element of the list is almost always considered as a function and the rest as the argument to that function.

```
λ user> (+ 1 2 3 4 5 6)
21
```

Here, `+` is a function and `1`, `2`, `3` are arguments. A list always evaluates the elements. To supress evaluation, we can quote the list.

```
λ user> (quote (list 1 2 3))
(1 2 3)

λ user> '(1 2 3)
(1 2 3)
```

Here both the expressions are same. The second expression is a short version which uses `'` quote symbol which has this special semantics, which being supress expression evaluation.

```
λ user> (first (list 1 2 3))
1

λ user> (first '(1 2 3))
1

λ user> (first (1 2 3))
[error] Expected 'function' but obtained 'number'
```

Here the `(list 1 2 3)` produces a list. The first argument `list` is a function which creates a list out of the given elements. We then apply the result to the `first` function which returns the first element from the list.
Note that, the inner expression is evaluated and the result is passed to the outer `first` function.

The second expression uses list notation form. When using list as notation, the first argument is the number `1`, which is not a function. So if we did not quote the list, the list will be evaluated with `1` assumed as function with arguments `2`, `3`, `4`, which will fail, as in the thrid sample expression. So we supress the evaluation passing the list as such.

#### Vector

Vectors are similar to lists, except that vectors does not evaluate the values like list does. So we can intuit it like a quoted list.

```
λ user> [1 2 3]
[1 2 3]

λ user> (first [1 2 3])
1

λ user> (rest (vector 1 2 3))
[2 3]
```

The vector notation is `[]` squre braces. We can use the function `vector` to create a vector as well. The `rest` is a function that operates on sequences (`list`, `vector`, `string`) returning every element except the first. This is same as `head`, `tail`, or `car`, `cdr` functions one will encounter in other Lisp dialects.

#### Hash Map

Hash maps are dictionary collection which can have key value pair. The short notation for hash-map is `{}`.

```
λ user> {"name" "Olive", "age" 42, "location" "Liestal"}
{"name" "Olive" "age" 42}

λ user> (def person {:first-name "Jane" :last-name "Doe" :location "Montreux"})
{:first-name "Jane" :last-name "Doe" :location "Montreux"}

λ user> (:first-name person)
"Jane"

λ user> (get :last-name person)
"Doe"
```

Hash map does not gurantee ordering of elements. Here the first expression uses strings as keys, where as the second one uses a keyword. Keywords has same sematics as that of Clojure keyword. We use `def` to bind a value to a variable. Here we bind the hash-map to the variable `person`. 

When using keyword in the function position of a list with argument as a hash-map, we get the value associated with that key in the hash-map if present, else `nil`. If we are using a different key type, we can use the `get` function to get the value associated with the key as in the last expression.

#### Keyword

Keywords are denoted using an atom with `:` prefix. 

```
λ user> :sunday
:sunday

λ user> (def weekend [:saturday :sunday])
```
The last example shows a weekend variable which is a vector containing keywords.

#### String

String are anything within a double quotes. Most of the list functions also works on strings. 

```
λ user> (first "Olive")
"O"

λ user> (concat "Jane" " " "Doe")
"Jane Doe"
```

#### Atom

Atoms are a container holding another value type where operations on the atom takes place atomically. At present DreamLisp is only single threaded from user's perspective, so this distinction does not hold much.

```
λ user> (atom {:count 2 :displayed? false})
(atom {:displayed? false :count 2})
```

#### nil

All functions returns value, the default being `nil`.

```
λ user> nil
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
λ user> (inc 41)
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

Higher order collection functions like `map/n`, `foldl/3`, `foldr/3`, etc., by default produces a lazy sequence. The operations are deferred until we force evaluation either by applying other list functions over the lazy sequence like `first/1` or `take/2`, or using `doall/1`.

## Core Functions

DreamLisp has a set of built-in core functions which are natively implemented and helpers which are written in DreamLisp itself and loaded into the REPL during initialization. These core functions are in the module `core` and is availble to all other modules without having to explictly import them.

### Sequence Functions
Sequence functions works on `list`, `vector` and `string` types.

#### list/n

Create a new list.

```
λ user> (list "Coleridge" "Frost" "Hardy")
("Coleridge" "Frost" "Hardy")
```

#### list?/1

`list?` is a predicate function which checks if the given element is a list type.

```
λ user> (list? [1 2 3])
false

λ user> (list? '("beauty" "and" "the" "beast"))
true
```

#### vector/n

Create a new vector.

```
λ user> (vector "Austen" "Brontë" "Woolf")  ;; create a vector
["Austen" "Brontë" "Woolf"]
```

#### vector?/1

`vector?` is a predicate function which checks if the given element is a vector type.

```
λ user> (vector? [1 2 3])
true

λ user> (vector? "castle in the sky")
false
```

### first/1

The `first` function returns the first element of a sequence if present or `nil`.

```
λ user> (first '(42 43 44))
42

λ user> (first ["Oxford" "London" "Cambridge"])
"Oxford"
```

#### rest/1

The `rest` function takes a sequence and returns another sequence with all elements except the first one or an empty sequence if no element are present.

```
λ user> (def uni "York")
"York"

λ user> (rest ["Manchester" uni "Oxford" "Cambridge"])
["York" "Oxford" "Cambridge"]

λ user>(rest "amnesia")
["m" "n" "e" "s" "i" "a"]
```

#### last/1

Returns the last element of the list.

```
λ user> (last ["Lisp" "Clojure" "ClojureScript"])
"ClojureScript"

λ user> (last '())
nil
```

#### drop/1

`drop` returns a new list by removing `n` element from the given list. If `n` is negative, elements are removed from the last.

```
λ user> (drop 1 '(1 2 3))
(2 3)

λ user> (drop -2 ["Belgium" "Ibiza", "Miami", "Chicago"])
["Belgium" "Ibiza"]
```

#### empty?/1

`empty?` is a predicate function that checks if the given sequence is empty.

```
λ user> (empty? '())
true

λ user> (empty? [])
true

λ user> (empty? [5 7 13 17])
false

λ user> (empty? "")
true

λ user> (empty? "rainbows and unicorns")
false
```

#### count/1

Returns the count of the given sequence.

```
λ user> (count '("a" "e" "i" "o" "u"))
5

λ user> (count ["Luna" "Hermione" "Harry"])
3

λ user> (count "opposite opposite")
17
```

#### cons/2

Takes any element and adds it to the first of the given list, returning the new list.

```
λ user> (cons 1 '(2 3 4))
(1 2 3 4)

λ user> (cons '(1) '(2 3 4))
((1) 2 3 4)
```

#### concat/n

Takes a list of sequences and combines them into one sequence.

```
λ user> (concat '(-2 -1 0) '(1 2 3 4))
(-2 -1 0 1 2 3 4)

λ user> (concat ["hello"] ["world"])
["hello" "world"]

λ user> (concat "Bul" "bul")
"Bulbul"

λ user> (concat "After" [1 2 "B"] "C")
"After12BC"
```

#### conj/n

Takes a vector and n elements, appends the elements to the vector and return resulting new vector. If a list is given the elements are appended to the head of the list giving a reversed list.

```
λ user> (conj '(1) 2 3 4) ; (4 3 2 1)
(4 3 2 1)

λ user> (conj [1 2 3] 4 5 6)
[1 2 3 4 5 6]
```

#### append/3

Takes an element, an index, a sequence and appends the element at that index.

```
λ user> (append 4 4 '(0 1 2 3 5))
(0 1 2 3 4 5)
```

#### nth/2

Returns the nth element from the sequence.

```
λ user> (nth 1 [3.14 159 265])
159

λ user> (nth 0 [35 89 79])
35

λ user> (nth 2 "NSValue")
"V"
```

#### nth-tail/3

Takes a start, an end index, a sequence and returns a sub-sequence within the given indices inclusive.

```
λ user> (nth-tail 2 4 '(0 1 2 3 4 5))
(2 3 4)
```

#### filter/2

Takes a filter predicate function and a collection, and returns a lazy sequence which when realised applies the function to each element in the collection and returns the resulting filtered collection.

```
λ user> (doall (filter (fn (x) (< x 0)) '(-1 4 0 -4 -5 2)))
(-1 -4 -5)
```

#### flatten/1

Takes any nested collection and returns a lazy sequence which when realised return its contents as a single collection.

```
λ user> (doall (flatten [[1] [2 [3]] [[[4]]] [5]]))
[1 2 3 4 5]
```

#### foldl/3

`foldl` takes a function, an initial value and a sequence, initialises the accumulator with the initial value, applies the function to each value in the collection with the result in the accumlator, returning the accumulator when done. The function application takes place from left to right.

```
λ user> (foldl (fn (x acc) (str acc x)) "" ["a" 1 "b" 2 "c" 3])
"a1b2c3"
```

#### foldr/3

Same as `foldl/3` but the function application takes place from right to left.

```
λ user> (foldr (fn (x acc) (str acc x)) "" ["a" 1 "b" 2 "c" 3])
"3c2b1a"
```

#### index-of/2

Returns the index of the given element if present in the collection, else `nil`.

```
λ user> (index-of 11 '(21 11 22))
1
```

#### into/2

Takes a sequence and any element, adds the element into the sequence, returning the given initial sequence type.

```
λ user> (into [1 2] "a")
[1 2 "a"]

λ user> (into [] '("kanji" "kana" "hiragana"))
["kanji" "kana" "hiragana"]
```

#### join/2

Take any element and a sequence or collection, joins, the element at each intermediate position, returning a sequence.

```
λ user> (join "->" "abcd")
"a->b->c->d"

λ user> (join 0 '(1 2 3))
(1 0 2 0 3)
```

#### map/n

`map` is a higher order function which takes a function and a sequence and applies the function to each element in the sequence. Built-in higher order function returns a lazy sequence by default.

```
λ user> (map (fn (a) (* 2 a)) [1 2 3])
#<lazy-seq [1 2 3]>
```

To realise the value of the lazy sequence, we can use the `doall` function.

```
λ user> (doall (map (fn (a) (* 2 a)) [1 2 3]))
[2 4 6]
```

#### partition/2

Takes a predicate function and a collection, applies the function to each element in the collection and returns the resulting collection partitioned into two, where first one satisifies the pedicate and the second does not.

```
λ user> (partition (fn (x) (> x 4)) [4 2 5 3 1 7])
[[5 7] [4 2 3 1]]
```

#### take/2

Takes `n` elements from the given sequence. If the sequence is lazy, this makes the elements of sequence to be evaluated as required.

```
λ user> (take 2 (map (fn (a) a) [1 2 3 4]))

λ user> (take 3 [1 2 3 4])
[1 2 3]
```

#### reverse/1

Returns a lazy sequence which when realised reverse the elements in the given sequence.

```
λ user> (doall (reverse "apple"))

λ user> (doall (reverse [11 121 1221]))
[1221 121 11]
```

#### sort/2

Returns the collection sorted by the given criteria. The first argument depends on the data structure provided. It takes a keyword `:asc`, `:desc` for `list`, `vector` and `string` elements. For `hash-map`, it takes these in a `vector` with first element preferably the sort indicator `:key` or `:value`. The first argument can also take a comparison function which returns an integer value in case of the former data types. For `hash-map`, the function is passed as the second element in the `vector`.

```
λ user> (sort :asc [3 5 2 4])
[2 3 4 5]

λ user> (sort :desc '(3 5 2 4))
(5 4 3 2)

λ user> (sort :asc "Magic")
"Macgi"
     
λ user> (sort [:key :asc] {:z 2 :a 4 :p -5})
[:a :p :z]

λ user> (sort [:value :desc] {:z 2 :a 4 :p -5})
[4 2 -5]

λ user> (sort (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1)) ["We" "are" "Legends"])
["Legends" "We" "are"]

λ user> (sort [:value (fn (a b) (cond (= a b) 0 (> a b) 1 (< a b) -1))] {:x "We" :y "are" :z "Legends"})
["Legends" "We" "are"]
```

#### seq

Takes a list, vector or a string and returns a list containing individual elements that can be iterated.

```
λ user> (seq '(1 2 3))
(1 2 3)

λ user> (seq [1 2 3])
(1 2 3)

λ user> (seq "abc")
("a" "b" "c")
```

### Hash map functions

Hash maps are dictionary based data structure.

#### hash-map/n

Creates a new hash map which takes key and value pairs. The elements are unordered by default. We can also use the literal notation `{}`.

```
λ user> (hash-map "a" 1)
{"a" 1}

λ user> {:b 22 :a 11}
{:a 11 :b 22}
```

#### hash-map?/1

A predicate which checks if the given element is a `hash-map`.

```
λ user> (hash-map? {:b 22 :a 11})
true

λ user> (hash-map? [:b 22 :a 11])
false
```

#### assoc/n

Takes a hash map and key value pairs, add them to the hash map and return a resulting new hash map.

```
λ user> (assoc {:a 1} :b 2 :c 3)
{:a 1 :c 4 :b 3}
```

#### dissoc/n

Takes a hash map and keys, removes the value associated for the key if present and returns the resulting new hash map.

```
λ user> (dissoc {:a 1 :b 2} :b :c)
{:a 1}
```

#### get/2

Takes a key, a hash map and returns the value associated with the key if present else `nil`. If the key is a keyword, we can use it at function position directly.

```
λ user> (get "y" {"z" 21 "y" 32 "x" 43})
32

λ user> (:b {:a 1 :b 2 :c 3})
2

λ user> (get :v {:a 1 :b 2 :c 3})
nil
```

#### contains?/2

Checks if the sequence or collection contains the given object.

```
λ user> (contains? :a {:a 1 :b 2 :c 3})
true
```

#### keys/1

Returns a list containing the hash map keys.

```
λ user> (keys {:a 1 :b 2 :c 3})
(:a :c :b)
```

#### values/1

Returns a list containing the hash map values.

```
λ user> (values {:a 1 :b 2 :c 3})
(1 3 2)
```

#### keywordize/1

Keywordize the given hash-map keys which are not a keyword already.

```
λ user> (keywordize {"name" "finder" "location" "mac"})
{:name "finder" :location "mac"}
```

### Misc

Miscellaneous functions

#### atom/1

Create an atom with the given element as its value. Basically, it acts as a container and has an atomic notion to it. However, DreamLisp is single threaded from user's perspective.

```
λ user> (atom {:success 0})
(atom {:success 0})
```

### atom?/1

A predicate which checks if the given element is an atom.

```
λ user> (atom? (atom {:success 0}))
true
```

#### deref/1

Dereferences an atom returning the value it holds.

```
λ user> (deref (atom {:success 0}))
{:success 0}
λ user> @(atom {:success 0})
{:success 0}
```

#### reset!/2

Mutates the value of an atom to the new given value.

```
λ user> (def a (atom 0))
(atom 0)

λ user> (reset! a 11)
11

λ user> a
(atom 11)
```

#### swap!/n

Takes an atom, a function and arguments, applies the function to the arguments and sets the resulting value as the value of the atom.

```
λ user> (def a (atom 11))
(atom 11)

λ user> (swap! a (fn (a) (* 10 a)))
110
```

#### keyword/1

Create a keyword from the given string.

```
λ user> (keyword "circle")
:circle
```

#### keyword?/1

A predicate which checks if the given element is a `keyword`.

```
λ user> (keyword? "zero")
false

λ user> (keyword? :zero)
true
```

#### symbol/1

Creates a symbol from the given string. If qualified, the symbol is associated with that module name as shown in the second example, else, it's not associated with any module, hence the `*` as in the first example.

```
λ user> (symbol "pi")
*:pi

λ user> (symbol "user:a")
user:a
```

#### symbol?/1

A predicate function which checks if the given element is a symbol.

```
λ user> (def z 0)
0

λ user> (symbol? 'z)
true

λ user> (symbol? 123)
false
```

#### apply/n

Takes a function and a list of arguments, invokes the function with the elements in the list as its arguments.

```
λ user> (apply str/1 ["d" "r" "e" "a" "m"])
"dream"

λ user> (apply (fn (& more) (doall (map odd?/1 more))) [0 3 13 31])
(false true true true)
```

#### throw/1

Throws an exception with the given data as its value.

```
λ user> (try (throw {:err "oops"}) (catch e e))
{:err "oops"}

λ user> (throw {:msg "oops"})
[error] Error: {:msg "oops"}  ; Here it's the console adding the [error] tag.
```

#### gensym/n

Generates a unique symbol across the runtime, which is used mainly in macros to avoid shadowing.

```
λ user> (gensym)
user:G__1

λ user> (gensym "p")
user:p__2
```

#### time-ms/0

Returns the current timestamp in milliseconds.

```
λ user> 1576624545772
```

#### type/1

Returns the type of the given element.

```
λ user> (type 'q)
"symbol"

λ user> (type 0)
"number"

λ user> (type '())
"list"
```

#### info/1

Returns details of the given element.

```
λ user> (info {:x 1})
{:type "hash-map" :module "*" :position 0}

λ user> (info +/n)
{:type "function" :module "*" :position 0 :arity -1 :macro? false :imported? false :name "+/n" :arguments []}

λ user> (info or/n)
{:type "function" :module "core" :position 0 :arity -1 :macro? true :imported? false :name "core:or/n" :arguments [core:x core:& core:more]}
```

### Lazy sequence functions

Below are the functions that creates, works on lazy sequences.

#### lazy-seq/1

Creates a lazy sequence from the given sequence.

```
λ user> (lazy-seq [1 2 3])
#<lazy-seq [1 2 3]>
```

#### lazy-seq?/1

A predicate function which checks if the given sequence is lazy.

```
λ user> (lazy-seq? (lazy-seq [1 2 3]))
true

λ user> (lazy-seq? [1 2 3])
false
```

#### has-next?/1

Returns whether the lazy sequence has elements which are not realised.

```
λ user> (def lseq (lazy-seq [1 2]))
#<lazy-seq [1 2]>

λ user> (has-next? lseq)
true
```

#### next/1

Returns the next element in the lazy sequence if present, else throws an exception.

Following from the above example,

```
λ user> (next lseq)
1

λ user> (next lseq)
2

λ user> (next lseq)
[error] Index 2 is out of bound for length 2

λ user> (has-next? lseq)
false
```

#### doall/1

Applies the function associated with the lazy sequence to the next element if present as required to realise the elements in the sequence.

```
λ user> (doall (map (fn (x) (+ x 3)) [1 2 3]))
[4 5 6]
```

