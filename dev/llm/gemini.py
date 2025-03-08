# from google import genai
import google.generativeai as genai
from dotenv import load_dotenv
import os

load_dotenv()  # Load variables from .env file

api_key = os.getenv("GEMINI_API_KEY")
print(api_key)

genai.configure(api_key=api_key)

model = genai.GenerativeModel(model_name='gemini-1.5-flash')
response = model.generate_content('Teach me about how an LLM works')

print(response.text)


# Foray into embeddings
# https://developers.googleblog.com/en/gemini-embedding-text-model-now-available-gemini-api/
api_key = os.getenv("GEMINI_API_KEY")
import google.generativeai as genai
genai.configure(api_key=api_key)
embeddings = genai.embed_content(model="models/gemini-embedding-exp-03-07", content="How does alphafold work?")

print(embeddings)