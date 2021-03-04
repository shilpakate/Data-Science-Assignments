#!/usr/bin/env python
# coding: utf-8

# In[1]:



import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from apyori import apriori
from mlxtend.frequent_patterns import apriori,association_rules


# In[2]:



movies = []
# As the file is in transaction data we will be reading data directly 
with open("D:\\Assignment__Rstudio\\Association_Rules\\my_movies.csv") as f:
    movies = f.read()


# In[3]:


movies


# In[4]:


# splitting the data into separate transactions using separator as "\n"
movies = movies.split("\n")
movies_list = []
for i in movies:
    movies_list.append(i.split(","))


# In[5]:


all_movies_list = [i for item in movies_list for i in item]
from collections import Counter

item_frequencies = Counter(all_movies_list)

item_frequencies = sorted(item_frequencies.items(),key = lambda x:x[1])

# Storing frequencies and items in separate variables 
frequencies = list(reversed([i[1] for i in item_frequencies]))
items = list(reversed([i[0] for i in item_frequencies]))

# Purpose of converting all list into Series object Coz to treat each list element as entire element not to separate 
movies_series  = pd.DataFrame(pd.Series(movies_list))

movies_series


# In[6]:


movies_series.columns = ["transactions"]

# creating a dummy columns for the each item in each transactions ... Using column names as item name
X = movies_series['transactions'].str.join(sep='*').str.get_dummies(sep='*')

X


# In[7]:


X = X.iloc[:,0:9]

X


# In[8]:


frequent_itemsets = apriori(X, min_support=0.015, max_len=4,use_colnames = True)

# Most Frequent item sets based on support 
frequent_itemsets.sort_values('support',ascending = False,inplace=True)

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1)
rules.sort_values('lift',ascending = False,inplace=True)

rules.head(10)


# In[9]:


#Support vs Lift

plt.scatter(rules['support'],rules['lift'],alpha=0.5)
plt.xlabel('support')
plt.ylabel('lift')
plt.title('Support vs Lift')
plt.show()


# In[10]:


#Support vs Confidence

plt.scatter(rules['support'],rules['confidence'],alpha=0.5)
plt.xlabel('support')
plt.ylabel('confidence')
plt.title('Support vs Confidence')
plt.show()


# In[11]:


#Confidence vs Lift

plt.scatter(rules['confidence'],rules['lift'],alpha=0.5)
plt.xlabel('confidence')
plt.ylabel('lift')
plt.title('Confidence vs Lift')
plt.show()


# In[12]:



fit = np.polyfit(rules['lift'],rules['confidence'],1)
fit_fn = np.poly1d(fit)
plt.plot(rules['lift'],
rules['confidence'],'yo',
rules['lift'],
         fit_fn(rules['lift']))


# In[13]:


frequent_itemsets = apriori(X, min_support=0.027, max_len=3,use_colnames = True)

# Most Frequent item sets based on support 
frequent_itemsets.sort_values('support',ascending = False,inplace=True)

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1)
rules.sort_values('lift',ascending = False,inplace=True)

rules.head()


# In[14]:


#Support vs Confidence

ax = rules["support"],rules["confidence"].value_counts().plot(kind="bar")
plt.xlabel('support')
plt.ylabel('confidence')
plt.title('Support vs Confidence')
plt.show()


# In[15]:


#Confidence vs Lift

bx = rules["confidence"],rules["lift"].value_counts().plot(kind="bar")
plt.xlabel('confidence')
plt.ylabel('lift')
plt.title('confidence vs lift')
plt.show()


# In[16]:


#Support vs Lift

bx = rules["support"],rules["lift"].value_counts().plot(kind="bar")
plt.xlabel('Support')
plt.ylabel('Lift')
plt.title('support vs lift')
plt.show()


# In[ ]:




