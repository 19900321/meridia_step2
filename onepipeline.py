#!/usr/bin/env python
# -*- coding: utf-8 -*-
# @Time    : 17.3.2018 21.53
# @Author  : YINYIN
# @Site    : 
# @File    : onepipeline.py
# @Software: PyCharm
import os
import sys
import numpy as np
import pandas as pd
import pickle
import codecs
from functools import reduce

os.getcwd()
os.chdir('C:\\Users\\yinyin\\Desktop\\herbpair\\2. propertyarrangement\\compound finger\\runitself')
os.getcwd()

# Process1:add ingredient id to fingerprint file
# input file1
datasmiles1 = pd.read_csv('input1-maccs-add.csv')
df = pd.DataFrame(datasmiles1)
combin1 = df.set_index('Ingredientid')

# Process2: finger of target and related drug disease,it takes about 2-3 hour

# 2.1 input file3python

datacompoundinfor = pd.read_csv('input3-compoundinfor3.csv')
compoundinfor2 = pd.DataFrame(datacompoundinfor)

# 2.2 extract target-disease-drug pairs
list1 = []
list2 = []
dic1 = {}
dataframeorins = pd.DataFrame()
counttar = 0
for i in compoundinfor2.index:
    print('i=', i)
    m = compoundinfor2.loc[i]
    n = m[3]
    n1 = m[0]
    n2 = m[1]
    if n not in list1:
        counttar = counttar + 1
        print('counttar=', counttar)
        list1 = list1 + [n]
        s = m[5]
        s2 = m[6]
        k = str(s).replace('nan', 'no data').split(';')
        k2 = str(s2).replace('nan', 'no data2').split(';')
        dataframe1 = pd.DataFrame(1, index=[n], columns=k + k2)
        dataframe2 = dataframe1.copy()
        dataframeorins = dataframeorins.append(dataframe2, ignore_index=False)

    # 2.3 extract compound target pairs
    if n != 'NA':
        if n1 in dic1:
            if n not in dic1[n1]:
                dic1[n1] = dic1[n1] + [n]
        else:
            dic1[n1] = [n]

    # 2.4 write target-disease-drug finger  into files
dataframeorins.to_csv('result2-targetinforcombin.csv')

# 2.4 write compound-target pairs into files
df = pd.DataFrame()
df['Ingredientid'] = dic1.keys()
df['targets'] = dic1.values()
df.to_csv('result3-compoundtargetdiction.csv')

# Process3: generate ingredientfinger

# 3.1 use gernnerated file called 'targetinforcombin.csv',and generated diction called'dic1'

dataframeorins2 = dataframeorins.fillna(value=0)
counttar2 = 0
df2 = pd.DataFrame()
for j in dic1:
    counttar2 = counttar2 + 1
    print('counttar2=', counttar2)
    listall = []
    sd = dic1[j]
    list34 = list(map(lambda x: dataframeorins2.loc[x], sd))
    listall = list(reduce(lambda x, y: x + y, list34))
    df24 = pd.DataFrame(listall, columns=[j], index=dataframeorins2.columns)
    df222 = df24.copy()
    df2 = df2.append(df222.T, ignore_index=False)
    print('there are', j, 'compound which have target information')

# 3.2 input feature of compound and related target and their disease drug into  file
df2.to_csv('result4-compoundtargetfeature.csv')

# process4: add target to compound and add to all conpoundinger,use generated compound target pair  dataframe df

# 4.1 generate target feature finger

dataframe5 = pd.DataFrame()
for i in dic1:
    print(i)
    pairsw = dic1[i]
    pairsw2 = [x for n, x in enumerate(pairsw) if x not in pairsw[:n]]
    if dic1[i] != dic1[1367]:
        dataframe1 = pd.DataFrame(1, index=[i], columns=pairsw2)
        print(dataframe1.columns)
        dataframe5 = dataframe5.append(dataframe1, ignore_index=False)
dataframe6 = dataframe5.fillna(value=0)

# 4.2 write target finger to  to file

dataframe6.to_csv('result6-ingredienttargetfinger.csv')

# Process5: add Maccs fingerprint and basic compound information to ingredentfinger including target related disease ,drug(df2), and combin1(maccs),target(dataframe6)and basic information

# 5.1 input file 4 :compoundbasicinfor.csv
datacompoundbasicindor = pd.read_csv('input4-compoundbasicinfor.csv')
datacompoundbasicindor1 = pd.DataFrame(datacompoundbasicindor)
datacompoundbasicindor2 = datacompoundbasicindor1.set_index('Ingredientid')

# 5.2 add all feature to all information

combin8 = pd.concat([datacompoundbasicindor2, combin1, df2, dataframe6], axis=1)

# 5.3 write all information to file
combin8.to_csv('result5-allingredentfinggercombin.csv')

# Process6: herb ingredient pair

# 6.1 generate herb ingredient pair diction

dataherb = pd.read_csv('input5-herbcompoundpair.csv')
dataherb2 = pd.DataFrame(dataherb)
dataherb1 = dataherb2.dropna(axis=0, how='any')

dic2 = {}
for i in dataherb1.index:
    print('i=', i)
    m = dataherb1.iloc[i]
    n = m[1]
    n1 = m[0]
    if n != 0 and (int(n) in combin8.index):
        print('true')
        if n1 in dic2:
            dic2[n1] = dic2[n1] + [n]
        else:
            dic2[n1] = [n]

# 6.2 write herb ingredient pair to file
df5 = pd.DataFrame()
df5['herbs'] = dic2.keys()
df5['Ingredientid'] = dic2.values()
df5.to_csv('result8-herbingredientpair.csv')

# Process7: all herb feature finger,use generated  dic2 and  all compound feature dataframe combin8

dataallcompoundfinger3 = combin8.iloc[:, 4:]
print(dataallcompoundfinger3.columns)
dataallcompoundfinger4 = dataallcompoundfinger3.fillna(value=0)
df7 = pd.DataFrame()
for s in dic2:
    print('s=', s)
    list34 = list(map(lambda x: dataallcompoundfinger4.loc[x], dic2[s]))
    listall = list(reduce(lambda x, y: x + y, list34))
    df24 = pd.DataFrame(listall, index=dataallcompoundfinger4.columns, columns=[s])
    df222 = df24.copy()
    df7 = df7.append(df222.T, ignore_index=False)

df7.to_csv('result9-herbfingerprocess.csv')

# Process8-2: herb-compounds pairs feature
datacompoundpair = pd.read_csv('input5-herbcompoundpair.csv')
compoundpair2 = pd.DataFrame(datacompoundpair)
dic4 = {}
for i in compoundpair2.index:
    print('i=', i)
    m = compoundpair2.loc[i]
    n = m[1]
    n1 = m[0]
    if n != 'NA':
        if n1 in dic4:
            if n not in dic4[n1]:
                dic4[n1] = dic4[n1] + [n]
        else:
            dic4[n1] = [n]

    # 8.2.2 write compound-target pairs into files
df4 = pd.DataFrame()
df4['Ingredientid'] = dic4.keys()
df4['compounds'] = dic4.values()
df4.to_csv('result16-compoundtargetdiction.csv')

dataframe16 = pd.DataFrame()
for x2 in dic4:
    print(x2)
    pairsw12 = dic4[x2]
    # pairsw22= [x for n, x in enumerate(pairsw12) if x not in pairsw12[:n]]
    dataframe26 = pd.DataFrame(1, index=[x2], columns=pairsw12)
    dataframe16 = dataframe16.append(dataframe26, ignore_index=False)
dataframe17 = dataframe16.fillna(value=0)

# 8.2.3 write herb-compounds to  to file

dataframe17.to_csv('result15-ingredienttargetfinger.csv')

# Process8: add property and compound/herb pairs to  finger

finger2 = pd.read_csv('input6-herbpropertyfinget.csv')
finger22 = pd.DataFrame(finger2)
finger23 = finger22.set_index('herb-id')
combin9 = pd.concat([finger23, dataframe17, df7], axis=1)
combin9.to_csv('result10-herbfingerfinal.csv')

# process9:filter related 848 herbs,use generated dataframe combin9

# 9.1: read input file7

list848 = pd.read_csv('input7-848.csv')
s1 = list848['herb-id']

# 9.2: generate 848 herb feature finger

listdataframe = list(map(lambda x: combin9.loc[x], s1))
listall = reduce(lambda x, y: pd.concat([x, y], axis=1, ignore_index=False), listdataframe)
listall2 = listall.T
listall2.to_csv('result11-herbfinger848.csv')

# process 10 turn those >1 to 1
import copy

listall3 = copy.deepcopy(listall2)
for i in listall3.columns:
    listall3[i][listall3[i] > 1] = 1
listall3.to_csv('result12-herbfinger848-1.csv')

# process 11 turn those >1 to 1
listall4 = listall2.fillna(value=0)
listall4.to_csv('result14-herbfinger848-fill0.csv')

# process 12 turn those >1 to 1



