import os
import re
from dotenv import load_dotenv

load_dotenv()

from interact import Interaction

from openai import OpenAI

from llmapi import Claude

# API KEY:
os.getenv("OPENAI_KEY")

client = OpenAI()

system_prompt = """
You are a helpful assistant that is helping to compute a constraint logic programming language called miniKanren, embedded in the Racket programming language.
One of the relations you are given is `==` which equates two logic variables.
You are also given the relation `evalo` which takes: `expr` and `value`. Evalo will take a program expression and evaluate it to a value.
Here is the definition of `evalo-expro`, `lookupo`, `evalo-list`, and `evalo` in miniKanren:
```racket
;; This is an interpreter for a simple Lisp.  Variables in this language are
;; represented namelessly, using De Bruijn indices.
;; Because it is implemented as a relation, we can run this interpreter with
;; unknowns in any argument position.  If we place unknowns in the `expr`
;; position, we can synthesize programs.
(define-relation (eval-expo expr env value)
  (conde
    ((fresh (body)
       (== `(lambda ,body) expr)      ;; expr is a procedure definition
       (== `(closure ,body ,env) value)))
    ((== `(quote ,value) expr))       ;; expr is a literal constant
    ((fresh (a*)
       (== `(list . ,a*) expr)        ;; expr is a list operation
       (eval-listo a* env value)))
    ((fresh (a d va vd)
       (== `(cons ,a ,d) expr)        ;; expr is a cons operation
       (== `(,va . ,vd) value)
       (eval-expo a env va)
       (eval-expo d env vd)))
    ((fresh (index)
       (== `(var ,index) expr)        ;; expr is a variable
       (lookupo index env value)))
    ((fresh (c va vd)
       (== `(car ,c) expr)            ;; expr is a car operation
       (== va value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (c va vd)
       (== `(cdr ,c) expr)            ;; expr is a cdr operation
       (== vd value)
       (eval-expo c env `(,va . ,vd))))
    ((fresh (rator rand arg env^ body)
       (== `(app ,rator ,rand) expr)  ;; expr is a procedure application
       (eval-expo rator env `(closure ,body ,env^))
       (eval-expo rand env arg)
       (eval-expo body `(,arg . ,env^) value)))))

;; Lookup the value a variable is bound to.
;; Variables are represented namelessly using relative De Bruijn indices.
;; These indices are encoded as peano numerals: (), (s), (s s), etc.
(define-relation (lookupo index env value)
  (fresh (arg e*)
    (== `(,arg . ,e*) env)
    (conde
      ((== '() index) (== arg value))
      ((fresh (i* a d)
         (== `(s . ,i*) index)
         (== `(,a . ,d) e*)
         (lookupo i* e* value))))))

;; This helper evaluates arguments to a list construction.
(define-relation (eval-listo e* env value)
  (conde
    ((== '() e*) (== '() value))
    ((fresh (ea ed va vd)
       (== `(,ea . ,ed) e*)
       (== `(,va . ,vd) value)
       (eval-expo ea env va)
       (eval-listo ed env vd)))))

;; Evaluation of a program expression with an empty environment.
(define (evalo expr value) (eval-expo expr '() value))
```
Given a query and logic variables, you will solve the query you are prompted with for each logic variable in the following form:
```
[logic variable] = [possible value]
```
"""

base_message = {"role": "system", "content": system_prompt}

query = """
(query (q)
    (evalo `(app (lambda . ,q) (quote (1 () a b x))) (quote ((1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x) (1 () a b x))))
    (evalo `(app (lambda . ,q) (quote (2 (3) 4 5 6))) (quote ((2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6) (2 (3) 4 5 6)))))
""".strip()


def make_user_prompt(content):
    # return {"role": "user", "content": content}
    return {"role": "system", "name": "example_user", "content": content}


def make_assistant_prompt(content):
    # return {"role": "assistant", "content": content}
    return {"role": "system", "name": "example_assistant", "content": content}


ex_q1 = "Q: (query (q) (evalo `(cons (quote 1) ,q) '(1 2)))"
ex_a1 = """A: `cons` will construct a list, and the first value in the list is already given to us. This means that the second variable required for `cons` must evaluate to `2`. Therefore, `q` must be `(quote (2))`.
```
q = (quote (2))
```
"""
ex_q2 = """Q: (query (q)
    (evalo `(app (lambda . ,q) (quote (11 . 22))) (quote (22 . 11)))
    (evalo `(app (lambda . ,q) (quote (33 . 44))) (quote (44 . 33))))
"""
ex_a2 = """A: We must solve for `q` which represents the function body. Given the two examples, we see that the function body represented by `q` swaps the variables of the given argumnt.
Step 1: Since `q` must return a list, we know that it will take the form `(cons a d)`.
Step 2: We must solve for the logic variable `a`. In this case, since `q` reverses the elements, we should retrieve the `cdr` of the arguments using the `var` variable lookup. By De Brujin indices, the variable reference will be: `(var ())`. So `a` will be: `(cdr (var ()))`.
Step 3: We now solve for `d` in the `cons`. Following from step 2, we can reason that `d` should be: `(car (var ()))`.
Therefore we arrive at the conclusion:
```
q = (cons (cdr (var ())) (car (var ())))
```
"""

claude = Claude(system_prompt.strip())

if __name__ == "__main__":
    messages = [
        # base_message,
        make_user_prompt(ex_q1),
        make_assistant_prompt(ex_a1),
        make_user_prompt(ex_q2),
        make_assistant_prompt(ex_a2),
        make_user_prompt(f"Q: {query}"),
        make_assistant_prompt("A: Let's think step by step."),
    ]
    # messages.append({"role": "user", "content": query})
    response = client.chat.completions.create(
        model="gpt-4-turbo-preview",
        temperature=0.0,
        messages=messages,
    )
    # response = claude.get_response(messages)
    print(f"Query: {query}\n")
    # print(f"Model Response: {response.content}")
    for i in range(len(response.choices)):
        model_message = response.choices[i].message
        print(f"Model Response {i+1}:")
        print(model_message.content)
