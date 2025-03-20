import json
import openai
from markitdown import MarkItDown

md = MarkItDown(enable_plugins=True) # Set to True to enable plugins
result = md.convert("./workfiles/mapping/cdisc_pilot1_docs/cdiscpilot01_SAP.pdf")
print(result.text_content)

# Load the schema and example data
with open('./workfiles/mapping/schema/ars_ldm.json', 'r') as f:
    schema = json.load(f)

with open('./workfiles/mapping/arsdata/examples/excel/csd_reimagined.json', 'r') as f:
    example_data = json.load(f)

# Load the new source document
# with open('new_source_document.txt', 'r') as f:
#     new_source = f.read()

new_source = result.text_content

# Prepare the prompt
prompt = f"""
Given the following schema and example data, generate similar data entries based on the new source document.

Schema:
{json.dumps(schema, indent=2)}

Example Data:
{json.dumps(example_data, indent=2)}

New Source Document:
{new_source}

Please generate 5 new data entries that follow the schema and are similar in structure to the example data, but based on the information in the new source document.
"""

# Call the OpenAI API
response = openai.ChatCompletion.create(
    model="gpt-4",
    messages=[
        {"role": "system", "content": "You are a data generation assistant."},
        {"role": "user", "content": prompt}
    ]
)

# Extract and parse the generated data
generated_data = json.loads(response['choices'][0]['message']['content'])

# Validate the generated data (implement your validation logic here)
def validate_data(data, schema):
    # Implement validation logic
    pass

validated_data = [entry for entry in generated_data if validate_data(entry, schema)]

# Save the validated data
with open('generated_data.json', 'w') as f:
    json.dump(validated_data, f, indent=2)

print(f"Generated and validated {len(validated_data)} data entries.")