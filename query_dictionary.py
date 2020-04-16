#!usr/bin/python 

import sys
import os
os.system("pip install --upgrade --force-reinstall git+https://github.com/hms-dbmi/pic-sure-python-adapter-hpds.git")
os.system("pip install --upgrade --force-reinstall git+https://github.com/hms-dbmi/pic-sure-python-client.git")

PICSURE_network_URL = "https://picsure.biodatacatalyst.nhlbi.nih.gov/picsure"
resource_id = "02e23f52-f354-4e8b-992c-d37c8b9ba140"
token_file = "token.txt"

with open(token_file, "r") as f:
    my_token = f.read()

from python_lib.wrappers import get_whole_dic
whole_data = get_whole_dic()

import pathlib
work_path=str(pathlib.Path().absolute())
whole_data.head()
whole_data.to_csv(path_or_buf=work_path+"/multiIndex_variablesDict.csv", sep=',',header=True)
