import os 
from dotenv import load_dotenv
load_dotenv()

from interact import Interaction

from openai import OpenAI

# API KEY:
os.getenv('OPENAI_KEY')

client = OpenAI()

model_prompt = """
You are a helpful assistant that will respond to the prompt you are given.
When prompted with the text "[h]elp [u]ndo or choice >", you will either respond with an h, u, or positive integer (for example: 1, 2, 3, 4, etc.).
"""

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

if __name__ == "__main__":
    # TODO: produce a query
    query = "blank query"
    print("Starting interaction\n============================\n\n")
    running = True
    messages = [
        { "role": "system", "content": model_prompt.strip() }
    ]
    with Interaction(query) as env:
#          while running:
#              print(env.read_prompt())
#              response = input("Response: ")
#              if response.strip() == 'q':
#                  running = False
#             env.send(response)
        i = 0
        while i < 3:
            prompt = env.read_prompt()
            print(prompt)
            messages.append({ "role": "user", "content": prompt })
            response = client.chat.completions.create(
                model="gpt-3.5-turbo",
                # consider outputing to JSON object?
                messages=messages
            )
            model_message = response.choices[0].message
            print(f'Model responds: {model_message.content}')
            messages.append(model_message)
            env.send(model_message.content)
            i += 1

    print("\n\nEnd of interaction. Goodbye.")

