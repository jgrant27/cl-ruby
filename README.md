The implementers of existing Ruby VMs have gone the way of C, C++ and Java.
There is another possibility. Why not implement Ruby in Common Lisp ?

Ok, let's take the first shot and implement a subset of the Ruby language. Just enough to run some simple contrived numeric benchmarks. The following pseudo BNF describing a minimal subset of Ruby should do (taken from the rubydocs <a href="http://www.ruby-doc.org/docs/ruby-doc-bundle/Manual/man-1.4/yacc.html">here</a>).

```
PROGRAM         : COMPSTMT

COMPSTMT        : STMT (TERM EXPR)* [TERM]

STMT            : CALL
| EXPR

EXPR            | ARG

CALL            : FUNCTION

FUNCTION        : OPERATION [`(' [CALL_ARGS] `)']

ARG             : LHS `=' ARG
| ARG `..' ARG
| ARG `+' ARG
| ARG `-' ARG
| ARG `*' ARG
| PRIMARY

PRIMARY         : LITERAL
| VARIABLE
| for BLOCK_VAR in EXPR DO
COMPSTMT
end
| def FNAME ARGDECL
COMPSTMT
end

LHS             : VARIABLE

CALL_ARGS       : ARGS

ARGS            : ARG (`,' ARG)*

ARGDECL         : `(' ARGLIST `)'

ARGLIST         : IDENTIFIER(`,'IDENTIFIER)*

VARIABLE        : VARNAME
| nil

LITERAL         : numeric
| STRING

TERM            : `;'
| `n'

FNAME           : IDENTIFIER

OPERATION       : IDENTIFIER

VARNAME         : IDENTIFIER

STRING          : `"' any_char* `"'

IDENTIFIER is the sequence of characters in the pattern of /[a-zA-Z_][a-zA-Z0-9_]*/.
```

Let's write two functions in our new Ruby subset language. First a function to calculate fibonacci numbers.

```
def fib(n)
  curr = 0
  succ = 1
  presucc = nil

  for i in 1..n do
    presucc = succ
    succ = curr + succ
    curr = presucc
  end

  curr
end

fib(100000)
```

Then run it on ruby1.9 :

```
jgrant@pluto:~/cl-ruby$ ruby1.9 fib_it.rb
195328212870775773163201494759...<208928 digits>...719893411568996526838242546875
50.352559375 seconds of real time
```

Next a function to calculate factorials

```
def fact (n)
  tot = 1

  for i in 1..n do
    tot = tot * i
  end

  tot
end

res = fact(100000)
```

Then run it on ruby1.9 :

```
jgrant@pluto:~/cl-ruby$ ruby1.9 fact.rb
282422940796034787429342157802...<456514 digits>...000000000000000000000000000000
20.71802105 seconds of real time
```

Now let's compare by running the same code on our Ruby subset implemented in Lisp

```
(defun test-fib()
  (let* ((fun (compile-ruby
               "def fib (n)
                  curr = 0
                  succ = 1
                  presucc = nil


                  for i in 1..n do
                    presucc = succ
                    succ = curr + succ
                    curr = presucc
                  end

                  curr
                end"))
         (funcall (compile-ruby
                   "fib(1000000)"))
         (res (progn (run-ruby fun)
                     (format t "~A~%~A" fun funcall)
                     (time (run-ruby funcall))))
         (resstr (format nil "~A" res)))
    (format t "~A...<~A digits>...~A"
            (subseq resstr 0 30)
            (- (length resstr) 60)
            (subseq resstr (- (length resstr) 30))))
  nil)

* (test-fib)
Evaluation took:
  31.734 seconds of real time
  31.810000 seconds of total run time (30.070000 user, 1.740000 system)
  [ Run times consist of 1.260 seconds GC time, and 30.550 seconds non-GC time. ]
  100.24% CPU
  3 forms interpreted
  50,639,978,358 processor cycles
  43,412,830,352 bytes consed

195328212870775773163201494759...<208928 digits>...719893411568996526838242546875
```

And factorial

```
(defun test-fact()
  (let* ((fun (compile-ruby
               "def fact (n)
                  tot = 1

                  for i in 1..n do
                    tot = tot * i
                  end

                  tot
                end"))
         (funcall (compile-ruby
                   "fact(100000)"))
         (res (progn (run-ruby fun)
                     (format t "~A~%~A" fun funcall)
                     (time (run-ruby funcall))))
         (resstr (format nil "~A" res)))
    (format t "~A...<~A digits>...~A"
            (subseq resstr 0 30)
            (- (length resstr) 60)
            (subseq resstr (- (length resstr) 30))))
  nil)

*  (test-fact)
Evaluation took:
  5.772 seconds of real time
  5.790000 seconds of total run time (5.470000 user, 0.320000 system)
  [ Run times consist of 0.230 seconds GC time, and 5.560 seconds non-GC time. ]
  100.31% CPU
  3 forms interpreted
  9,210,473,724 processor cycles
  9,030,853,008 bytes consed

282422940796034787429342157802...<456514 digits>...000000000000000000000000000000
```

We're seeing  ~ 1.5 - 3.5 X improvement with our toy CL Ruby 'compiler' without even trying to generate performant code.

<a href="https://github.com/jgrant27/cl-ruby">Code is here</a>.
