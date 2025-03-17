# Convert to yaml
python utilities/python/excel2yaml.py -x 'workfiles/mapping/arsdata/examples/excel/csd_reimagined.xlsx'

# Validate the data against the schema
linkml-validate -s workfiles/mapping/schema/ars_ldm.yaml workfiles/mapping/arsdata/examples/excel/csd_reimagined.yaml

# Convert to json
linkml-convert  -s workfiles/mapping/schema/ars_ldm.yaml workfiles/mapping/arsdata/examples/excel/csd_reimagined.yaml -o workfiles/mapping/arsdata/examples/excel/csd_reimagined.json
