import os 
from dotenv import load_dotenv
load_dotenv()

from openai import OpenAI

# API KEY:
# os.getenv('OPENAI_KEY')

client = OpenAI()

prompt = """
You are a helpful assistant.
"""

response = client.chat.completions.create(
    model="gpt-3.5-turbo",
    # consider outputing to JSON object?
    # response.
    messages=[
        {"role": "system", "content": prompt.strip() },
        {"role": "user", "content": "Evaluate this prompt..." }
    ]
)

print(response.choices[0].message.content)
