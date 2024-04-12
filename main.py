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

fname = "claude_test.txt"

# You are also given the 'appendo' relation which takes 3 parameters: l, s, and ls. The relation is satisfied when l and s are appended, equate to ls.
# This is the definition of appendo in miniKanren:
# (define-relation (appendo ab c abc)
#   (conde
#    ((== '() ab) (== c abc))
#    ((fresh (a b bc)
#            (== `(,a . ,b) ab)
#            (== `(,a . ,bc) abc)
#            (appendo b c bc)))))

model_prompt = """
You are a helpful assistant that is helping to guide the search process of a constraint logic programming language called miniKanren, embedded in the Racket programming language.
Information about the current step and current relational constraints are provided at each step.
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
When prompted with the text "[u]ndo or choices (period-separated)>", you will make a decision, responding with a `u` to undo or period-separated positive integers (for example: 1, 2, 2.1.2, etc.).
Note that the choices are 1-indexed.
First, explain the problem the query is trying to solve.
Then, think step by step about how to solve it.
You will respond with the decision(s) in a new line.
"""
# You should first explain the problem you are trying to solve, then explain the decision you want to make and the reasoning behind it.

# Your goal is to construct a quine: a program that evaluates to itself.
# ```racket
# '(((app (lambda (list (quote app) (var ())
#                     (list (quote quote) (var ()))))
#         (quote (lambda (list (quote app) (var ())
#                             (list (quote quote) (var ())))))))))
# ```

steps_matcher = re.compile(r"Steps: (\d+)")
valid_input = re.compile(r"([u\d.]+)")

# response = client.chat.completions.create(
#     model="gpt-3.5-turbo",
#     # consider outputing to JSON object?
#     messages=[
#         {"role": "system", "content": prompt.strip() },
#         {"role": "user", "content": "Evaluate this prompt..." }
#     ]
# )
#
# print(response.choices[0].message.content)


def base_message():
    # return [{"role": "system", "content": model_prompt.strip()}]
    return []


claude = Claude(model_prompt.strip())

if __name__ == "__main__":
    # TODO: produce a query
    print("Starting interaction\n============================\n\n")
    running = True
    messages = base_message()
    steps_taken = []
    with open(os.path.join("logs", fname), "w") as f:
        with Interaction() as env:
            #          while running:
            #              print(env.read_prompt())
            #              response = input("Response: ")
            #              if response.strip() == 'q':
            #                  running = False
            #             env.send(response)
            i = 0
            while i < 100:
                prompt = env.read_prompt()
                print(prompt)
                f.write(prompt)
                if "Goodbye" in prompt:
                    break
                elif "Number of results" in prompt:
                    match = steps_matcher.search(prompt)
                    if match:
                        steps = int(match.group(1))
                        print(f"\nSteps taken so far: {steps_taken}")
                        print(f"Took {steps} steps\n############################\n\n")
                        steps_taken.append(steps)
                    messages.clear()
                    messages.extend(base_message())
                    continue
                messages.append({"role": "user", "content": prompt})
                # response = client.chat.completions.create(
                #     model="gpt-4-turbo-preview",
                #     # model="gpt-4",
                #     # consider outputing to JSON object?
                #     messages=messages,
                # )
                # model_message = response.choices[0].message
                model_message = claude.get_response(messages)
                print(f"Model responds:\n> {model_message.content}\n=====\n")
                f.write("\n================================\n")
                f.write(model_message.content)
                f.write("\n================================\n")
                messages.append(model_message.get_message())
                to_send = (model_message.content or "u").split("\n")
                while len(to_send) > 0 and valid_input.match(to_send[-1]) is None:
                    to_send.pop(-1)
                if len(to_send) == 0:
                    to_send.append("u")
                env.send(to_send[-1])
                # messages quite long, let's trim some (keep the initial prompt)
                # if len(messages) > 8:
                #     messages.pop(1)
                #     messages.pop(1)
                i += 1

        print("\n\nEnd of interaction. Goodbye.")
        print("steps taken:")
        print(steps_taken)
        f.write(f"steps taken: {steps_taken}")
