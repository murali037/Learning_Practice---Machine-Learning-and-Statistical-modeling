#%%

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

#%%
import scipy
from scipy import stats


#%% raw data - 1. Remove unnecessary rows and columns

data = pd.read_excel("2020-01-16_forams.xls", sheet_name=1)

data = data[data.Run_ID != '2% nitric wash']
data = data[data.Run_ID != 'GS Blank']
data = data.iloc[:,[0,1,2,3,5,6,7,10,11,27,28,30,37]] #keeping only the important column
data = data.replace('Blank(old)','Blank')


#%% excel o/p check

data.to_excel('test1.xlsx') #checking excel output

#%% 2. separate data into blanks and not blanks

data_blanks = data.loc[data['Run_ID'] == 'Blank']
data_Not_Blanks = data.loc[data['Run_ID'] != 'Blank']

#%% 3. Remove Outliers in blanks

sns.boxplot(x=data_blanks['7Li (STD)']) # boxplot to check outlier
plt.show()


#%% usinf z score with threshold of 3 to remove outliers

test = data_blanks.drop(['Run_ID'], axis = 1) # Remove Run_ID column

test = test.round(0).astype(int) # change to int type for computing z score

from scipy import stats
test1 = test[(np.abs(stats.zscore(test)) < 3).all(axis=1)]

test.info()
test1.info()

# Here, Run_Num is a unique identifier

#%% use Run_Num of test1 to remove outliers from data_blanks

data_blanks = pd.merge(data_blanks, test1['Run_Num'], how='inner', indicator=True)

# Now, data_blanks has values without outliers


#%% remove first row which dosnt have data

data_Not_Blanks = data_Not_Blanks.drop(data_Not_Blanks.index[[0]]) #drop row
#dont rerun this line if already removed
data_Not_Blanks = data_Not_Blanks.reset_index()

#%%
# combine data_blanks and data_not_blanks and sort by Run_Num

combined = data_Not_Blanks.append(data_blanks, sort = False)
combined = combined.reset_index()
combined = combined.drop(combined.columns[[0]], axis=1) # drop col


#sort by Run_Num

combined = combined.sort_values('Run_Num')

combined = combined.reset_index()

data.to_excel('combined_test1.xlsx') #checking excel output

#%% 4. Subtract Samples by nearest blocks - Find the blanks

test_sub = combined

blank_index = []

j=0
while j < len(test_sub.Run_ID):
    for i in range(j,len(test_sub.Run_ID)):
        if test_sub.Run_ID[i] == 'Blank':
            if i+1 == len(test_sub.Run_ID):
                print("Blank found: ", test_sub.Run_Num[i], " ", i)
                blank_1 = i
                blank_index.append(blank_1)
                print(blank_index)
                break
            if test_sub.Run_ID[i+1] != 'Blank':
                print("Blank found: ", test_sub.Run_Num[i]," ", i)
                blank_1 = i
                blank_index.append(blank_1)
                print(blank_index)
                break

    j = i+1


#%% 4. Subtract Samples by nearest blocks - weighted average - approach 1

only_values = combined.drop(['level_0','index','Run_Num','Run_ID','_merge'], axis=1)

#%%
op = only_values

# [5, 19, 59, 73, 87, 101, 115, 123]

for k in range(len(blank_index)):
    if k == len(blank_index)-1:
        break
    l = blank_index[k+1] - blank_index[k] - 1    # 13
    first_index = blank_index[k]+1               # 6
    last_index = blank_index[k+1]-1              # 18
    w1 = 1
    for z in range(first_index,last_index+1):
        w1 = w1-(100/l)/100                     # 0.9230
        if w1 < 0.01:
            w1 = 0.01
        wn = 1-w1                               # 0.0769
        op.loc[z] = op.loc[z].subtract(((w1 * only_values.loc[blank_index[k]])+
                                        (wn * only_values.loc[blank_index[k+1]])))

#%% 4. for second approach

values_no_blanks = data_Not_Blanks.drop(['index','Run_Num','Run_ID'], axis=1)
values_blanks = data_blanks.drop(['Run_Num','Run_ID','_merge'], axis=1)

#%% 4. Subtract Samples by nearest blocks - weighted average - approach 2 - using Run_Num as reference

# j = [5, 6, 7, 8, 9, 10, 11]

k=0 

for i in range(len(data_Not_Blanks.Run_Num)):
    for j in range(k, len(data_blanks.Run_Num)):
        if data_Not_Blanks.Run_Num[i] > data_blanks.Run_Num[j] and data_Not_Blanks.Run_Num[i] < data_blanks.Run_Num[j+1]:     #j is required

            k = j                                                   #updates blank index if the first blank is found
            l = data_blanks.Run_Num[j+1] - data_blanks.Run_Num[j]-1

            if (data_Not_Blanks.Run_Num[i] - data_blanks.Run_Num[j]) <= (data_blanks.Run_Num[j+1] - data_Not_Blanks.Run_Num[i]):
                w1 = 1-((data_Not_Blanks.Run_Num[i] - data_blanks.Run_Num[j])/l)
                wn = 1 - w1
            else:
                w1 = 1 - ((data_blanks.Run_Num[j] - data_Not_Blanks.Run_Num[i]) / l)
                wn = 1 - w1

            values_no_blanks.loc[i] = values_no_blanks.loc[i].subtract((w1 * values_blanks.loc[j]) +
                                            (wn * values_blanks.loc[j+1]))


#%% # blanks subtracted samples - Add back Run_Num and Run_ID

std_samples = data_Not_Blanks.iloc[:,[1,2]]
std_samples = pd.concat([std_samples, values_no_blanks.reindex(std_samples.index)], axis=1)

#%%
std_samples.to_excel('std_samples.xlsx') #checking excel output


#%%







