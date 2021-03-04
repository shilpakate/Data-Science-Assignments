#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from apyori import apriori
from mlxtend.frequent_patterns import apriori,association_rules


# In[7]:



groceries = []
# As the file is in transaction data we will be reading data directly 
with open("D:\\Assignment__Rstudio\\Association_Rules\\groceries.csv") as f:
    groceries = f.read()


# In[8]:


groceries


# In[10]:


# splitting the data into separate transactions using separator as "\n"
groceries = groceries.split("\n")
groceries_list = []
for i in groceries:
    groceries_list.append(i.split(","))


# In[11]:


groceries_list


# In[13]:


all_groceries_list = [i for item in groceries_list for i in item]
from collections import Counter

item_frequencies = Counter(all_groceries_list)

item_frequencies = sorted(item_frequencies.items(),key = lambda x:x[1])

# Storing frequencies and items in separate variables 
frequencies = list(reversed([i[1] for i in item_frequencies]))
items = list(reversed([i[0] for i in item_frequencies]))

groceries_series  = pd.DataFrame(pd.Series(groceries_list))
groceries_series = groceries_series.iloc[:9835,:]

groceries_series.columns = ["transactions"]

# creating a dummy columns for the each item in each transactions ... Using column names as item name
X = groceries_series['transactions'].str.join(sep='*').str.get_dummies(sep='*')

frequent_itemsets = apriori(X, min_support=0.023, max_len=4,use_colnames = True)

# Most Frequent item sets based on support 
frequent_itemsets.sort_values('support',ascending = False,inplace=True)

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=2)
rules.sort_values('lift',ascending = False,inplace=True)

rules.head(10)


# In[14]:


#Support vs Confidence

ax = rules["support"],rules["confidence"].value_counts().plot(kind="pie")
plt.xlabel('support')
plt.ylabel('confidence')
plt.title('Support vs Confidence')
plt.show()


# In[15]:



#confidence vs lift

bx = rules["confidence"],rules["lift"].value_counts().plot(kind="bar")
plt.xlabel('confidence')
plt.ylabel('lift')
plt.title('confidence vs lift')
plt.show()


# In[16]:


#Support vs lift

bx = rules["support"],rules["lift"].value_counts().plot(kind="hist")
plt.xlabel('Support')
plt.ylabel('Lift')
plt.title('support vs lift')
plt.show()


# In[ ]:




