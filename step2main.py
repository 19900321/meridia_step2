#
# !/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 18.3.2018 9.26
# @Author  : YINYIN
# @Site    : 
# @File    : step2main.py
# @Software: PyCharm
import os
import sys
print(os.getcwd())

os.chdir('C:\\Users\\yinyin\\Desktop\\herbpair\\2. propertyarrangement\\compound finger\\runitself\\used for finger generation')
print(os.getcwd())

import pandas as pd
import numpy as np

hecomppair=pd.read_csv('input5-herbcompoundpair.csv')
hecomppair=pd.DataFrame(hecomppair)

comallfea1=pd.read_csv('result5-allingredentfinggercombin.csv')
comallfea2=pd.DataFrame(comallfea1)
coltem=comallfea2.columns[0]
comallfea2=comallfea2.rename(columns = {coltem:'Ingredientid'})
comallfea=comallfea2.set_index(comallfea2.columns[0])

herballfea1=pd.read_csv('result20-herbfingerfinal.csv')
herballfea2=pd.DataFrame(herballfea1)
herballfea=herballfea2.set_index(herballfea2.columns[0])

admetexten1=pd.read_csv('ADMEextenfin.csv')
admetexten2=pd.DataFrame(admetexten1)
coltem2=admetexten2.columns[0]
admetexten2=admetexten2.rename(columns = {coltem2:'Ingredientid'})
admetexten=admetexten2.set_index(admetexten2.columns[0])

def organfilter(seachterm,herballfea):
    organframe=herballfea[herballfea[seachterm]==1]
    herblist=organframe.index.values
    organpair=pd.DataFrame()
    for oneherb in herblist:
        organpairone=hecomppair[hecomppair.iloc[:,0]==oneherb]
        organpair=organpair.append(organpairone,ignore_index=True)
    print(organpair.index)
    group1=organpair.groupby('Ingredientid',as_index=False).count()
    group2=pd.DataFrame(group1).set_index('Ingredientid')
    group2.columns=[seachterm]
    return group2

fluquencedf=pd.DataFrame()
needorlist=['Lung','Spleen','Stomach','Bladder','Cardiovascular','Gallbladder','Heart','Kidney','LargeIntestine','Liver']
import codecs
from functools import reduce
fluquencelist = list(map(lambda x:organfilter(x,herballfea),needorlist))
fluquencedf= reduce(lambda x, y: pd.concat([x, y], axis=1, ignore_index=False),fluquencelist)

fluquencedf.to_csv('C:\\Users\\yinyin\\Desktop\\herbpair\\step2\\fluquency3.csv')


feafru739= pd.concat([fluquencedf,admetexten,comallfea], axis=1, join='inner')
feafru739.to_csv('C:\\Users\\yinyin\\Desktop\\herbpair\\step2\\feafru700-2.csv')

