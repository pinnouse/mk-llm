from anthropic import Anthropic
from openai import OpenAI

import os
from typing import Optional

from dotenv import load_dotenv

load_dotenv()


class ChatResponse:
    def __init__(self, role, content):
        self.role = role
        self.content = content

    def get_message(self):
        return {"role": self.role, "content": self.content}


class LLM:
    def __init__(self, client, system_prompt):
        pass

    def get_response(self, messages) -> Optional[ChatResponse]:
        pass


class Claude(LLM):
    client: Anthropic

    def __init__(self, system_prompt):
        self.client = Anthropic(api_key=os.getenv("ANTHROPIC_API_KEY"))
        self.system_prompt = system_prompt

    def get_response(self, messages):
        message = self.client.messages.create(
            system=self.system_prompt,
            model="claude-3-opus-20240229",
            max_tokens=1024,
            messages=messages,
            temperature=0.0,
        )
        return ChatResponse(message.role, message.content[0].text)


class GPT(LLM):
    client: OpenAI

    def _make_messages(self, messages):
        return [{"role": "system", "content": self.system_prompt}] + messages

    def __init__(self, system_prompt) -> None:
        self.client = OpenAI(api_key=os.getenv("OPENAI_KEY"))
        self.system_prompt = system_prompt

    def get_response(self, messages):
        response = self.client.chat.completions.create(
            model="gpt-4-turbo-preview",
            temperature=0.0,
            messages=self._make_messages(messages),
        )
        return ChatResponse(
            response.choices[0].message.role, response.choices[0].message.content
        )
