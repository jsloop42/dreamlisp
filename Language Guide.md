# DreamLisp Programming Language Guide

v1.0

### Data Types

All data types in DreamLisp are immutable in nature.

#### List

Lists are the building blocks for any Lisp program. Lists unlike vector, which we will see later, has special semantics in general. The first element of the list is almost always considered as a function and the rest as the argument to that function.

```
λ user> (+ 1 2 3 4 5 6)
21
```

Here, `+` is a function and `1`, `2`, `3` are arguments. A list always evaluates the elements. To suppress evaluation, we can quote the list.

```
λ user> (quote (list 1 2 3))
(1 2 3)

λ user> '(1 2 3)
(1 2 3)
```

Here, both expressions are the same. The second expression is a short version which uses `'` quote symbol which has this special semantics, which being suppress expression evaluation.

```
λ user> (first (list 1 2 3))
1

λ user> (first '(1 2 3))
1

λ user> (first (1 2 3))
[error] Expected 'function' but obtained 'number'
```

Here, the `(list 1 2 3)` produces a list. The first argument `list` is a function which creates a list out of the given elements. We then apply the result to the `first` function which returns the first element from the list.
Note that, the inner expression is evaluated and the result is passed to the outer `first` function.

The second expression uses the list notation form. When using the list as notation, the first argument is the number `1`, which is not a function. So if we did not quote the list, the list will be evaluated with `1` assumed as function with arguments `2`, `3`, `4`, which will fail, as in the third sample expression. So we suppress the evaluation passing the list as such.

#### Vector

Vectors are similar to lists, except that vectors do not evaluate the values like list does. So we can intuit it like a quoted list.

```
λ user> [1 2 3]
[1 2 3]

λ user> (first [1 2 3])
1

λ user> (rest (vector 1 2 3))
[2 3]
```

The vector notation is `[]` square braces. We can use the function `vector` to create a vector as well. The `rest` is a function that operates on sequences (`list`, `vector`, `string`) returning every element except the first. This is same as `head`, `tail`, or `car`, `cdr` functions one will encounter in other Lisp dialects.

#### Hash Map

Hash maps are dictionary collection which can have key-value pair. The short notation for hash-map is `{}`.

```
λ user> {"name" "Jane", "age" 42, "location" "Planet Earth"}
{"name" "Jane" "age" 42}

λ user> (def person {:first-name "Jane" :last-name "Doe" :location "Planet Earth"})
{:first-name "Jane" :last-name "Doe" :location "Planet Earth"}

λ user> (:first-name person)
"Jane"

λ user> (get :last-name person)
"Doe"
```

Hash map does not guarantee ordering of elements. Here the first expression uses strings as keys, whereas the second one uses a keyword. Keywords have the same semantics as that of Clojure keyword. We use `def` to bind a value to a variable. Here we bind the hash-map to the variable `person`. 

When using keyword in the function position of a list with the argument as a hash-map, we get the value associated with that key in the hash-map if present, else `nil`. If we are using a different key type, we can use the `get` function to get the value associated with the key as in the last expression.

#### Keyword

Keywords are denoted using an atom with `:` prefix. 

```
λ user> :sunday
:sunday

λ user> (def weekend [:saturday :sunday])
```
The last example shows a weekend variable which is a vector containing keywords.

#### String

A string is anything within double-quotes. Most of the list functions also works on strings. 

```
λ user> (first "Alice")
"A"

λ user> (concat "Jane" " " "Doe")
"Jane Doe"
```

#### Atom

Atoms are a container holding another value type where operations on the atom take place atomically. At present DreamLisp is only single threaded from user's perspective, so this distinction does not hold much.

```
λ user> (atom {:count 2 :displayed? false})
(atom {:displayed? false :count 2})
```

#### nil

All functions return a value, the default being `nil`.

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

Here `inc/1` and `dec/1` are two functions which take an argument `n` and increments or decrements by one. The string given after the argument is the docstring.

Note: When using the REPL, on a line break the expression will be evaluated. We can define the function without line breaks or we can load from a file.

```
λ user> (inc 41)
42
```

### let

`let` is used for local binding within an expression. The variables defined within `let` is only visible within its lexical scope and also available within a closure.

```
(let ((name-xs ["Alice" "Bob" "Jane"])
      (greet (fn (n)
               (doall (map (fn (x) (println "Hello" x)) n)))))
  (greet name-xs)
  nil)
    
; Hello Alice
; Hello Bob
; Hello Jane
; nil
```

`let` binding can take vector form like in Clojure. The same example with vector bindings can written like below. The traditional style of binding with parenthesis is preferred.

```
(let [name-xs ["Alice" "Bob" "Jane"] 
      greet (fn (n)
              (doall (map (fn (x) (println "Hello" x)) n)))]
  (greet name-xs) 
  nil)
    
; Hello Alice
; Hello Bob
; Hello Jane
; nil
```
Here we define variables `name-xs` which is a list of names and an anonymous function `greet` that takes a list of names and prints a message to the console. 

### Macros

Now to the best part, macros! Macros, as the Lisp lore says are programs that write programs. Macros are compile time only in the sense that when we apply a macro within some function, and later change the macro, the existing defined function does not change, because that code was already produced from the previous macro expansion when the function was defined. To make variable names to not have conflicts as the macros are unhygienic, we can make use of `gensym` which produces unique symbols throughout.

```
(defmacro when (x & form)
  "Evaluates the first expression and if `true`, evaluates the body."
  `(if ~x (do ~@form)))
```

Here we require a macro because we require conditional evaluation of arguments. If we use a function instead, all the arguments passed to the function will be first evaluated which is not what we want. The `when` macro evaluates the expression first and if it is `true`, it then executes the body. We use special reader macros that are available only within a macro definition. These have the standard Lisp semantics.

The backtick, which is unquote \`, is used to write code as is. Within the unquote, using tilde `~`, which is unquote evaluates the variable that follows and replaces it with its value. If the variable is a list, we use splice-unquote `~@`. Invoking macro will then replace the call site with the code, which is called the macro expansion. Macros can be considered like templating in a sense, but it's not mere string substitution.

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

Say, this is the `utils.dlisp` file, which contains `collatz` function. The filename and module name needs to be the same as a convention. Here we define a module using `defmodule` followed by the module name and then the module exports. The exports are function name followed by arity. Similarly, if we are importing from another module, we can use the `import` special form. Only exported functions and macros are visible to the other modules.

```
(defmodule utils-test 
  (export all)
  (import (from test (deftest n) (testing n) (is 2) (is 3) (run 1))))
```

Here we are importing `deftest` which takes a variable number of arguments which is represented by `n`. `is` takes only two arguments, so its arity is `2`.

The default module is called `user` which is the module that the REPL initializes into. REPL also loads one other module which is the `core`.

## Core Module

DreamLisp has a set of built-in core functions which are natively implemented and helpers which are written in DreamLisp itself and loaded into the REPL during initialization. These core functions are in the module `core` and are available to all other modules without having to explicitly import them.

### Sequence Functions
Sequence functions works on `list`, `vector` and `string` types.

#### list/n

Create a new list.

```
λ user> (list "Common Lisp" "Clojure" "LFE")
("Common Lisp" "Clojure" "LFE")
```

#### list?/1

`list?` is a predicate function which checks if the given element is a list type.

```
λ user> (list? [1 2 3])
false

λ user> (list? '("mojave" "catalina" "big sur" "monterey"))
true
```

#### vector/n

Create a new vector.

```
λ user> (vector "Jan" "Feb" "Mar")  ;; create a vector
["Jan" "Feb" "Mar"]
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
λ user> (def name "Swift")
"Swift"

λ user> (rest ["Objective-C" name "C" "C++"])
["Swift" "C" "C++"]

λ user> (rest "amnesia")
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

λ user> (drop -2 ["Paris" "Tokyo", "California", "Chicago"])
["Paris" "Tokyo"]
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

λ user> (count ["Apple" "Orange" "Banana"])
3

λ user> (count "hello world")
11
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

λ user> (concat "Eternal" " " "September")
"Eternal September"

λ user> (concat "After" [1 2 "B"] "C")
"After12BC"
```

#### conj/n

Takes a vector and n elements, appends the elements to the vector and return resulting new vector. If a list is given the elements are appended to the head of the list giving a reversed list.

```
λ user> (conj '(1) 2 3 4)
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

Takes a filter predicate function and a collection, and returns a sequence which applies the function to each element in the collection and returns the resulting filtered collection.

```
λ user> (doall (filter (fn (x) (< x 0)) '(-1 4 0 -4 -5 2)))
(-1 -4 -5)
```

#### flatten/1

Takes any nested collection and returns a sequence with its contents as a single collection.

```
λ user> (doall (flatten [[1] [2 [3]] [[[4]]] [5]]))
[1 2 3 4 5]
```

#### foldl/3

`foldl` takes a function, an initial value and a sequence, initializes the accumulator with the initial value, applies the function to each value in the collection with the result in the accumulator, returning the accumulator when done. The function application takes place from left to right.

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

`map` is a higher order function which takes a function and a sequence and applies the function to each element in the sequence.

```
λ user> (map (fn (a) (* 2 a)) [1 2 3])
[2 4 6]
```

`map` takes variadic arguments and invokes the functions with arguments interleaved.

```
λ user> (map (fn (a b) [a b]) [1 2 3] [4 5 6])
[[1 4] [2 5] [3 6]]
```

#### partition/2

Takes a predicate function and a collection, applies the function to each element in the collection and returns the resulting collection partitioned into two, where the first one satisfies the predicate and the second does not.

```
λ user> (partition (fn (x) (> x 4)) [4 2 5 3 1 7])
[[5 7] [4 2 3 1]]
```

#### take/2

Takes `n` elements from the given sequence.

```
λ user> (take 2 (map (fn (a) a) [1 2 3 4]))

λ user> (take 3 [1 2 3 4])
[1 2 3]
```

#### reverse/1

Returns a sequence with elements reversed.

```
λ user> (doall (reverse "apple"))
elppa

λ user> (doall (reverse [11 121 1221]))
[1221 121 11]
```

#### sort/2

Returns the collection sorted by the given criteria. The first argument depends on the data structure provided. It takes a keyword `:asc`, `:desc` for `list`, `vector` and `string` elements. For `hash-map`, it takes these in a `vector` with first element preferably the sort indicator `:key` or `:value`. The first argument can also take a comparison function which returns an integer value in the case of the former data types. For `hash-map`, the function is passed as the second element in the `vector`.

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

Hash maps are dictionary-based data structure.

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

Takes a hash map and key-value pairs, add them to the hash map and return a resulting new hash map.

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

### Special forms

#### if

A conditional which evaluates the first expression if the condition expression is `true`, else evaluates the second expression if present.

```
λ user> (if (> 1 0) ">" "<")
">"

λ user> (if (> 1 0) ">")
">"

λ user> (if (< 1 0) ">")
nil

λ user> (if (< 1 0) ">" "<")
"<"
```

#### do

`do` is used to group multiple expression within a single expression.

```
λ user> (if 1 (do (println "1") (println "2")) (println "-1"))
1
2
nil
```

#### fn

`fn` form is used for defining anonymous functions.

```
λ user> ((fn [x] (* x 3.14)) 3.14)
9.8596
```

#### macroexpand

Expands the given macro expression.

```
λ user> (macroexpand (or (= 1 1) (= 2 2)))
(core:if (user:= 1 1) (user:= 1 1) (core:or (user:= 2 2)))
```

#### quote

Suppresses evaluation of a list form.

```
λ user> (quote (+ 1 2))
(user:+ 1 2)

λ user> '(+ 1 2)
(user:+ 1 2)
```

### Misc

Miscellaneous functions

#### atom/1

Create an atom with the given element as its value. Basically, it acts as a container and has an atomic notion to it. However, DreamLisp is single-threaded from the user's perspective.

```
λ user> (atom {:success 0})
(atom {:success 0})
```

#### atom?/1

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

Throws an exception with the given data as its value. A `try` `catch` is used to handle exceptions.

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

### rand/1

Returns a random number within the specified upper bound inclusive.

```
λ user> (rand 31)
24
```

### shuffle/1

Shuffles the given sequence using Fisher–Yates algorithm with uniform random distribution.

```
λ user> (shuffle [1 2 3 4 5])
[5 3 2 4 1]
```

### String functions

Below are functions that work with strings.

#### string?/1

This is a predicate function which checks if the given element is a string.

```
λ user> (string? :info)
false

λ user> (string? "strange")
true
```

#### uppercase/1

Returns the string in uppercase format.

```
λ user> (uppercase "lisp")
"LISP"
```

#### lowercase/1

Returns the string in lowercase format.

```
λ user> (lowercase "LISP")
"lisp"
```

#### substring/3

Returns the substring within the given indices inclusive.

```
λ user> (substring 0 7 "Run free and dive into the sky")
"Run free"
```

#### regex/1

Returns a compiled regular expression.

```
λ user> (def pattern (regex "\d+"))
<regex <NSRegularExpression: 0x604000063f50> \d+ 0x0>
```

#### match/2

Takes a string and a compiled regular expression or a pattern in string format and matches the occurrences of the pattern within the string globally.
Following the example above:

```
λ user> (match "p3141i" pattern)
[["3141"]]

λ user> (match "p31i41p" "\d+")
[["31"] ["41"]]
```

#### split/2

Split the given string by the path component.

```
λ user> (split "lisp-is-life" "-")
["lisp" "is" "life"]
```

#### trim/1

Removes spaces from the start and end of the given string.

```
λ user> (trim " life is lisp ")
"life is lisp"
```

### Predicates

Below are predicates which are conditional functions.

#### nil?/1

Checks if the given value is `nil`.

```
λ user> (nil? nil)
true

λ user> (nil? 42)
false
```

#### true?/1

Checks if the given element evaluates to `true`.

```
λ user> (true? true)
true

λ user> (true? (> 3 3.1))
false
```

#### false?/1

Checks if the given element evaluates to `false`.

```
λ user> (false? (> 3.1 3))
false

λ user> (false? (> 3 3.1))
true
```

#### number?/1

Checks if the given element is a number.

```
λ user> (number? 42.24)
true

λ user> (number? "12.13")
false
```

#### fn?/1

Checks if the given element is a function.

```
λ user> (fn? core:inc/1)
true

λ user> (fn? core:apply/n)
true
```

#### macro?/1

Checks if the given element is a macro.

```
λ user> (macro? core:cond/n)
true

λ user> (macro? core:or/n)
true

λ user> (macro? core:macro?/1)
false
```

#### zero?/1

Checks if the given element is zero.

```
λ user> (zero? 0.0)
true

λ user> (zero? (/ 3.14 3.14))
false
```

#### coll?/1

Checks if the given element is a collection. Sequences except `string` and hash-maps are collections.

```
λ user> (coll? {:planet "earth"})
true

λ user> (coll? [2 4])
true

λ user> (coll? "prime")
false
```

#### even?/1

Checks if the given number is even.

```
λ user> (even? 84)
true

λ user> (even? 13)
false
```

#### odd?/1

Checks if the given number is odd.

```
λ user> (odd? 13)
true

λ user> (odd? 26)
false
```

### Meta

Below are metadata related functions.

#### with-meta/2

Associates metadata with the given element.

```
λ user> (def kwd (with-meta :apple 1)) ; The keyword :apple has meta as 1
:apple

```

#### meta/1

Returns the metadata associated with the given element if present.
Following the example above:

```
λ user> (meta kwd)
1
```

### Eval

Methods that are related to the runtime evaluation of code are given below.

#### eval/1

Evaluates the given expression

```
λ user> (def z "zero")
"zero"

λ user> (eval (symbol "user:z"))
"zero"
```

#### read-string/1

Takes an expression in string form, tokenizes it returning the expression.

```
λ user> (read-string "(+ 2 3)")
(user:+ 2 3)
```

#### slurp/1

Read the content of a file as a string. This should be used for smaller files only.

```
% cat inp.dlisp
(println "Greetings!")


```

```
λ user> (slurp "inp.dlisp")
"(println \"Greetings!\")\n\n"
```

#### load-file/1

Loads a dlisp file and evaluates the code. Path can be relative to the current interpreter location or can be absolute.

```
λ user> (load-file "DreamLispTests/dlisp/collatz.dlisp")
[100000000 ... 13 40 20 10 5 16 8 4 2 1]
[:ok "collatz.dlisp"]
```

### IO

Below are input-output related functions.

#### readline/1

Reads a line from the stdin with the given prompt displayed.

```
λ user> (readline "> ")
> There there
"There there\n"
```

#### write-file/2

Writes the given string to the given file. If the file does not exist, a new file will be created. If the file exists, its contents will be overwritten.

```
λ user> (write-file "string data" "/tmp/mytext.txt")
true
```

#### cwd/0

Get the current working directory.

```
λ user> (cwd)
"/Users/jsloop/bin/dreamlisp"
```

### Module

Below are module related functions.

#### defmodule/3

This is used to define a module, which can have `export` and `import` expressions with the function name, arity tuples. Using `(export all)` exports all functions in the current module.

```
λ user> (defmodule tree (export (create-tree 0) (right-node 1) (left-node 1)))
tree
```

```
λ user> (defmodule stage (export (greet 0)) (export all) (import (from player (sum 1))))
stage
```

#### in-module/1

Changes the current module to the given module.

```
λ user> (in-module "core")
"core"

λ core> 
```

#### remove-module/1

Removes a loaded module from the module table.

```
λ user> (defmodule tree (export all))
tree

λ tree> (remove-module "tree")
nil

λ user>
```

#### current-module-name/0

Returns the current module name.

```
λ user> (current-module-name)
"user"
```

#### module-info/1

Returns the module info which contains the imports, exports, and other metadata.

```
λ user> (module-info "test")
{:description "A module to work with unit tests." :imports [] :internal [] :name "test" :exports [test:get-test-info/0 test:dec/1 test:inc/1 test:inc-test-count/0 test:is/3 test:update-test-info/1 test:deftest/n test:testing/n test:is/2 test:run/1]}
```

#### module-exist?/1

Checks if the given module is loaded.

```
λ user> (module-exist? "test")
true

λ user> (module-exist? "spaceship")
false
```

#### all-modules/0

Returns all the loaded modules.

```
λ user> (all-modules)
["core" "test" "user" "network"]
```

### JSON

Below are functions to work with JSON data.

#### encode-json/1

Encodes the given hash-map to json string.

```
λ user> (encode-json {:first-name "Jane" :last-name "Doe"})
"{\"first-name\":\"Jane\",\"last-name\":\"Doe\"}"
```

#### decode-json/1

Decodes the JSON string to a hash-map.

```
λ user> (decode-json "{\"first-name\":\"Jane\",\"last-name\":\"Doe\"}")
{"last-name" "Doe" "first-name" "Jane"}
```

### Functions, macros implemented in DreamLisp

Below are functions, macros that are implemented in DreamLisp itself and loaded during bootstrap.

#### defun/n

Is a macro which is used for defining functions. Functions with same name and different arity are treated as different functions.

```
λ user> (defun prn-args () "zero")
user:prn-args/0

λ user> (defun prn-args (x) "one")
user:prn-args/1

λ user> (defun prn-args (x y) "two")
user:prn-args/2

λ user> (prn-args)
"zero"

λ user> (prn-args 1)
"one"

λ user> (prn-args 1 2)
"two"
```

Functions can have LFE, the traditional Lisp style parenthesis for arguments or Clojure style which uses vectors. The traditional style is preferred.

```
λ user> (defun greet [name] (println "Hello" name))
```

#### not/1

Negates the given expression that evaluates to a boolean value.

```
λ user> (not true)
false

λ user> (not (> 2 3))
true
```

#### cond/n

Evaluates the expression and if `true`, evaluates the corresponding form, else continues with the next expression.

```
λ user> (let ((a 2) (b 4)) (cond (= a b) 0 (> a b) 1 (< a b) -1))
-1

λ user> (let ((a 2) (b 2)) (cond (= a b) 0 (> a b) 1 (< a b) -1))
0

λ user> (let ((a 4) (b 2)) (cond (= a b) 0 (> a b) 1 (< a b) -1))
1
```

#### or/n

Evaluates expressions one at a time. If any expression returns `true`, the function returns with the result, else continues evaluation until the last expression is evaluated.

```
λ user> (or (= 1 2) (= 3 4))
false

λ user> (or (= 1 2) (< 3 4))
true
```

#### and/n

Evaluates expressions one at a time. If any expression returns `false`, the function returns with the result, else continues evaluation until the last expression is evaluated.

```
λ user> (and (= [1] [1]) (= (+ 1 1) 2))
true

λ user> (and (= [1] [1]) (= (+ 1 2) 2))
false
```

#### when/n

Evaluates the first expression and if `true`, evaluates the body.

```
λ user> (when (= 2 (+ 1 1)) "match")
"match"

λ user> (when (= 2 (- 1 1)) "match")
nil
```

#### when-not/n

Evaluates the first expression and if `false`, evaluates the body.

```
λ user> (when-not (= 2 (- 1 1)) "not-match")
"not-match"
```

#### not=/n

Negates the result of applying `=` on the given form.

```
λ user> (not= [1] [3])
true

λ user> (not= 1.5 1.5 1.5)
false
```

#### second/1

Returns the seconds element from the list.

```
λ user> (second [0 1 2 3])
1
```

#### sort/1

Sort the given collection in ascending order. If the collection is `hash-map`, the keys are sorted.

```
λ user> (sort [10 -3 6 3 7])
[-3 3 6 7 10]

λ user> (sort {10 3 -3 7 4 9})
[-3 4 10]
```

#### ->/n

Thread first macro, which takes an expression, evaluates it and inserts the result as the second element of the second expression and so on.

```
λ user> (-> [1 2 3] (count))
3
```

#### ->>/n

Thread last macro, which takes an expression, evaluates it and inserts the result as the last element of the second expression and so on.

```
λ user> (->> [1 4] (concat [2 3]))
[2 3 1 4]
```

#### inc/1

Increments the given number by one.

```
λ user> (inc 41)
42
```

#### dec/1

Decrements the given number by one.

```
λ user> (dec 14)
13
```

#### identity/1

Takes an element and returns the same element.

```
λ user> (identity -3.1415)
-3.1415
```

#### exit/0

Exits the REPL.

```
λ user> (exit)
Bye
```

## Run script from command line

The `dlisp` interpreter can be used to execute a `dlisp` file from the Terminal directly.

```
% dlisp DreamLispTests/dlisp/i.test.dlisp

Testing test-1 test case
test-1 case 1

Testing groups
test c - case 1

test c - case 2

Testing a success case
success

{:fail 2 :count 3 :success 1}
```

## Test Module

This module contains functions and macros for writing unit tests.

A test module can be written like any other module with test functions defined using `deftest`. To run all test functions, we use the macro `(run "test-module-name")`.

Check the test cases under [DreamLispTests/dlisp](https://github.com/jsloop42/dreamlisp/tree/master/DreamLispTests/dlisp).

Furthermore, there are extensive tests for the language implementation itself under [DreamLispTests](https://github.com/jsloop42/dreamlisp/tree/master/DreamLispTests).
