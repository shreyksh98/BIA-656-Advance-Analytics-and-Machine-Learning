#!/usr/bin/env python
# coding: utf-8

# # BIA 656 
# ## Advance Data Analytics and Machine Learning
# ## Assignment : Lab 4
# ## Shrey Kshatriya

# # Q1. Load the CSV data into a pandas data frame. Print some high-level statistical info about the data frame's columns.

# In[7]:


import pandas as pd
import numpy as np

dx = pd.read_csv('Concrete_Data.csv')
dx.head()


# You can use 'describe' for additional information as it gives the mean, std and IQR values.

# In[8]:


dx.describe()


# To get high level information, you can use 'info'

# In[9]:


dx.info()


# # Q2. How many rows have a compressive strength > 40 MPa?

# In[10]:


len(dx[dx['Concrete_Compressive_Strength']>40])


# # Q3. Plot the histogram of Coarse Aggregate and Fine Aggregate values

# In[12]:


dx.hist(column='Coarse_Aggregate')


# In[13]:


dx.hist(column='Fine_Aggregate')


# # Q4. Make a plot comparing compressive strength to age

# In[24]:


dx.plot('Concrete_Compressive_Strength', 'Age', 'scatter')


# # Q5. Make a plot comparing compressive strength to age for only those rows with < 750 fine aggregate.

# To do that we create a data frame of rows with < 750 fine aggregate.

# In[29]:


temp = dx['Fine_Aggregate'] < 750
dy = dx[temp]
dy.plot('Concrete_Compressive_Strength', 'Age', 'scatter', label = '< 750 fine aggregate')


# # Q6. Try to build a linear model that predicts compressive strength given the other available fields.

# In[30]:


# First we'll need to import the predictive model we'll use
from sklearn import linear_model 

# Choose a particular sort of linear regression model
# Get set up to train one of those
my_linear = linear_model.Lasso(alpha=0.01)

# Assemble the training data
# Let's use these columns as features and the target variable
features = ["Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", "Superplasticizer", "Fine_Aggregate", "Age"]
target = "Concrete_Compressive_Strength"

# Eliminate (drop) any instances with missing values (NaNs) for now
cleaned_dx = dx.dropna()

# Train the model you set up on the data
#   a.k.a. Fit the model to the data!
my_linear.fit(cleaned_dx[features], cleaned_dx[target])

# Show the coefficients of the linear model
pd.DataFrame([dict(zip(features, my_linear.coef_))])


# # Q7. Generate predictions for all the observations and a scatterplot comparing the predicted compressive strengths to the actual values.

# In[35]:


preds = my_linear.predict(cleaned_dx[features])
predictions_dx = cleaned_dx.assign(predictions=preds)
predictions_dx[["Concrete_Compressive_Strength", "predictions"]]


# In[37]:


predictions_dx.plot('Concrete_Compressive_Strength', 'predictions', 'scatter')

