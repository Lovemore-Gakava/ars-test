import openai
import requests
from typing import List, Dict, Union

class LLMSearch:
    def __init__(self, provider: str, api_key: str):
        self.provider = provider.lower()
        self.api_key = api_key

    def search(self, query: str, model: str = None) -> Union[List[str], str]:
        if self.provider == "openai":
            return self._search_openai(query, model or "gpt-4")
        elif self.provider == "huggingface":
            return self._search_huggingface(query, model or "bigscience/bloom")
        else:
            raise ValueError("Unsupported provider. Choose 'openai' or 'huggingface'.")

    def _search_openai(self, query: str, model: str) -> List[str]:
        response = openai.ChatCompletion.create(
            model=model,
            messages=[{"role": "system", "content": "You are a helpful search assistant."},
                      {"role": "user", "content": query}],
            api_key=self.api_key
        )
        return [choice["message"]["content"] for choice in response["choices"]]

    def _search_huggingface(self, query: str, model: str) -> str:
        headers = {"Authorization": f"Bearer {self.api_key}"}
        response = requests.post(
            f"https://api-inference.huggingface.co/models/{model}",
            headers=headers,
            json={"inputs": query}
        )
        return response.json()

# Example Usage
if __name__ == "__main__":
    api_key = Sys.getenv("OPEN_API_KEY")  # Replace with your API key
    search_engine = LLMSearch(provider="openai", api_key=api_key)
    results = search_engine.search("What are the latest advancements in AI research?")
    print(results)
