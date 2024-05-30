## DreamLisp Programming Language Interpreter

DreamLisp is a Lisp dialect with immutable data structures, modules, lazy collections and is a Lisp-2 with 
an unhygienic macro system. DreamLisp interpreter is implemented in Objective-C
and as such can run on macOS and iOS. It is tail recursive, uses ARC instead of
garbage collection, has asynchronous communication using notifications taking 
advantage of the underlying Objective-C runtime and Foundation library.  

#### Version

```
DreamLisp v1.0 (13) [Objective-C 2.0]
Shell v1.2 (5)
```
## Language Guide

[DreamLisp Programming Language Guide](Language%20Guide.md)

## Xcode Configurations

Use the `DreamLispAll` scheme to build all the targets which includes the framework, shell and test cases. This scheme is used for making release builds.  

Use the `DreamLispAll-Debug` scheme for development. Since the shell uses readline library the Xcode console won't work properly. Instead this scheme opens the shell in Terminal app. Make sure to enable `DevToolsSecurity` so that the debugger can attach to the running process.

```
% sudo DevToolsSecurity -enable
```

---

In the experimental version [objc-rt-1 branch](https://github.com/jsloop42/dreamlisp/tree/objc-rt-1), 
we can define classes and create objects at runtime from the REPL. At present, 
the language runs in interpreted mode.  

## References

[Lisp Flavoured Erlang (LFE)](https://github.com/rvirding/lfe)  
[Clojure](https://clojure.org)  
[MAL (Make a Lisp)](https://github.com/kanaka/mal/)  
