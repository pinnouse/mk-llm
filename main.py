import os 
from dotenv import load_dotenv
load_dotenv()

from interact import Interaction

# from openai import OpenAI

# API KEY:
# os.getenv('OPENAI_KEY')

# client = OpenAI()
#
# prompt = """
# You are a helpful assistant.
# When prompted with the text "[h]elp [u]ndo or choice >", you will either respond with an h, u, or positive integer (for example: 1, 2, 3, 4, etc.).
# """
#
# response = client.chat.completions.create(
#     model="gpt-3.5-turbo",
#     # consider outputing to JSON object?
#     # response.
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
    with Interaction(query) as env:
        print(env.read_prompt())
    print("\n\nEnd of interaction. Goodbye.")

