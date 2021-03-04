#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from apyori import apriori
from mlxtend.frequent_patterns import apriori,association_rules


# In[2]:


book = pd.read_csv("D:\\Assignment__Rstudio\\Association_Rules\\book.csv")


# In[3]:



book


# In[4]:


frequent_itemsets = apriori(book, min_support=0.015, max_len=2,use_colnames = True)

# Most Frequent item sets based on support 
frequent_itemsets.sort_values('support',ascending = False,inplace=True)

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1)
rules.sort_values('lift',ascending = False,inplace=True)

rules.head(10)


# In[5]:


## Using diff support and confidence

frequent_itemsets = apriori(book, min_support=0.035, max_len=3,use_colnames = True)

# Most Frequent item sets based on support 
frequent_itemsets.sort_values('support',ascending = False,inplace=True)

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1)
rules.sort_values('lift',ascending = False,inplace=True)

rules.head(10)


# In[6]:


#Support Vs Lift

plt.scatter(rules['support'],rules['lift'],alpha=0.5)
plt.xlabel('support')
plt.ylabel('lift')
plt.title('Support vs Lift')
plt.show()


# In[7]:


#Support vs Confidence

plt.scatter(rules['support'],rules['confidence'],alpha=0.5)
plt.xlabel('support')
plt.ylabel('confidence')
plt.title('Support vs Confidence')
plt.show()


# In[8]:


#Confidence vs Lift

plt.scatter(rules['confidence'],rules['lift'],alpha=0.5)
plt.xlabel('confidence')
plt.ylabel('lift')
plt.title('Confidence vs Lift')
plt.show()


# In[9]:


fit = np.polyfit(rules['lift'],rules['confidence'],1)
fit_fn = np.poly1d(fit)
plt.plot(rules['lift'],
rules['confidence'],'yo',
rules['lift'],
         fit_fn(rules['lift']))


# In[10]:


frequent_itemsets = apriori(book, min_support=0.047, max_len=3,use_colnames = True)

# Most Frequent item sets based on support 
frequent_itemsets.sort_values('support',ascending = False,inplace=True)

rules = association_rules(frequent_itemsets, metric="lift", min_threshold=1)
rules.sort_values('lift',ascending = False,inplace=True)

rules.head(10)


# In[11]:


#Support VS Lift

ax = rules["support"],rules["confidence"].value_counts().plot(kind="hist")
plt.xlabel('support')
plt.ylabel('confidence')
plt.title('Support vs Confidence')
plt.show()


# In[12]:


#Confidence Vs Lift

bx = rules["confidence"],rules["lift"].value_counts().plot(kind="hist")
plt.xlabel('confidence')
plt.ylabel('lift')
plt.title('confidence vs lift')
plt.show()


# In[13]:


#Support vs Lift

bx = rules["support"],rules["lift"].value_counts().plot(kind="hist")
plt.xlabel('Support')
plt.ylabel('Lift')
plt.title('support vs lift')
plt.show()


# In[ ]:




