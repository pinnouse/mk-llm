import os
import re
from dotenv import load_dotenv
load_dotenv()

from interact import Interaction

from openai import OpenAI

# API KEY:
os.getenv('OPENAI_KEY')

client = OpenAI()

model_prompt = """
You are a helpful assistant that is helping to guide the search process of a constraint logic programming language called miniKanren, embedded in the Racket programming language.
Information about the current step is provided at each step.
One of the relations you are given is '==' which equates two logic variables.
You are also given the 'appendo' relation which takes 3 parameters: l, s, and ls. The relation is satisfied when l and s are appended, equate to ls.
When prompted with the text "[h]elp [u]ndo or choice number>", you will either respond with an h, u, or positive integer (for example: 1, 2, 3, 4, etc.).
"""

steps_matcher = re.compile(r'Finished in steps: (\d+)')

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
    return [
        { "role": "system", "content": model_prompt.strip() }
    ]

if __name__ == "__main__":
    # TODO: produce a query
    print("Starting interaction\n============================\n\n")
    running = True
    messages = base_message()
    steps_taken = []
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
            if 'Goodbye' in prompt:
                break
            elif 'Number of results' in prompt:
                match = steps_matcher.search(prompt)
                if match:
                    steps = int(match.group(1))
                    print(f'Took {steps} steps\n############################\n\n')
                    steps_taken.append(steps)
                messages = base_message()
                continue
            messages.append({ "role": "user", "content": prompt })
            response = client.chat.completions.create(
                model="gpt-4-1106-preview",
                # model="gpt-4",
                # consider outputing to JSON object?
                messages=messages
            )
            model_message = response.choices[0].message
            print(f'Model responds:\n> {model_message.content}\n=====\n')
            messages.append(model_message)
            env.send(model_message.content)
            i += 1

    print("\n\nEnd of interaction. Goodbye.")
    print(f"steps taken:")
    print(steps_taken)

