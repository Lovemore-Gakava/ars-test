python excel2yaml.py .\Common\ Safety\ Displays.xlsx

python excel2yaml.py -x './dev/excel2yaml/csd_sklg.xlsx'

linkml-convert -s dev/ars_ldm_reimagined.yaml dev/excel2yaml/csd_sklg.yaml -o dev/excel2yaml/csd_sklg.json

linkml-convert -s dev/ars_ldm_reimagined.yaml dev/excel2yaml/ars_data_sklg.yaml -o dev/excel2yaml/ars_data_sklg.json


