# Contraception choice
Contraception method choice among women varies across the world. Among the 1.9 billion women of reproductive age (15-49 years) living in the world in 2019, 1.1 billion have a need for family planning. 
The experience, or awareness, of side effects and inconveniences of using specific contraceptive methods and their effectiveness at preventing pregnancy, play a role in the choice of the method used.  
At the population level, several factors play a role in the heterogeneous distribution, including access to different contraceptive methods to the level of knowledge on contraceptives.

## Objectives
Due to the nature of ethics surrounding clinical data, there is limited access to specific data on factors influencing contraceptive use among women. The Contraceptive Method Choice Dataset compiles data collected from the 1987 National Indonesia Contraceptive Prevalence Survey. 
This article demonstrates how one would use Decision Trees as a data modeling method to predict a woman’s contraceptive usage based on socio-economic factors.
- OneHotEncoding
- Decision Trees

#Loading and Setting up the data
Import libraries from panda. Loaded the file as a .data file. Then reference the columns.


```python
#set up
import pandas as pd
import numpy as np
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import make_column_transformer
```


```python
df = pd.read_csv('cmc.data')
df.head()
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>24</th>
      <th>2</th>
      <th>3</th>
      <th>3.1</th>
      <th>1</th>
      <th>1.1</th>
      <th>2.1</th>
      <th>3.2</th>
      <th>0</th>
      <th>1.2</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>45</td>
      <td>1</td>
      <td>3</td>
      <td>10</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>4</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1</th>
      <td>43</td>
      <td>2</td>
      <td>3</td>
      <td>7</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>4</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>42</td>
      <td>3</td>
      <td>2</td>
      <td>9</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>3</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>3</th>
      <td>36</td>
      <td>3</td>
      <td>3</td>
      <td>8</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>2</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>4</th>
      <td>19</td>
      <td>4</td>
      <td>4</td>
      <td>0</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>3</td>
      <td>0</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>




```python
#Assigning column names to the dataset. 
df.columns=['age','wife_education','husband_education','no#_children',
            'religion', 'working', 'husband_occupation','std_index','media_exposure',
           'contraceptive_method']
df.head()
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>age</th>
      <th>wife_education</th>
      <th>husband_education</th>
      <th>no#_children</th>
      <th>religion</th>
      <th>working</th>
      <th>husband_occupation</th>
      <th>std_index</th>
      <th>media_exposure</th>
      <th>contraceptive_method</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>45</td>
      <td>1</td>
      <td>3</td>
      <td>10</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>4</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1</th>
      <td>43</td>
      <td>2</td>
      <td>3</td>
      <td>7</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>4</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>42</td>
      <td>3</td>
      <td>2</td>
      <td>9</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>3</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>3</th>
      <td>36</td>
      <td>3</td>
      <td>3</td>
      <td>8</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>2</td>
      <td>0</td>
      <td>1</td>
    </tr>
    <tr>
      <th>4</th>
      <td>19</td>
      <td>4</td>
      <td>4</td>
      <td>0</td>
      <td>1</td>
      <td>1</td>
      <td>3</td>
      <td>3</td>
      <td>0</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>




```python
#Mapping values to the coded dataset, and storing in a new dataframe
wife_education_mapping ={1:"low", 2:"medium-low", 3:"medium-high", 4:"high"}
husband_education_mapping = {1:"low", 2:"medium-low", 3:"medium-high", 4:"high"}
religion_mapping ={0:"Non-Islam", 1:"Islam"}
working_mapping={0:"Yes", 1:"No"}
husband_occupation_mapping ={1:"low", 2:"medium-low", 3:"medium-high", 4:"high"}
std_index_mapping ={1:"low", 2:"medium-low", 3:"medium-high", 4:"high"}
media_exposure_mapping = {0:"Good", 1:"Not good"}
#contraceptive_method_mapping ={1:"No use", 2:"Long term", 3:"Short term"}

df['wife_education_label'] = df['wife_education'].map(wife_education_mapping)
df['husband_education_label'] = df['husband_education'].map(husband_education_mapping)
df['religion_label'] = df['religion'].map(religion_mapping)
df['working_label'] = df['working'].map(working_mapping)
df['husband_occupation_label'] = df['husband_occupation'].map(husband_occupation_mapping)
df['std_index_label'] = df['std_index'].map(std_index_mapping)
df['media_exposure_label'] = df['media_exposure'].map(media_exposure_mapping)

new_df = df[['age','wife_education_label','husband_education_label','no#_children','religion_label','working_label',
             'husband_occupation_label','std_index_label','media_exposure_label','contraceptive_method']]
new_df.head()
```




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>age</th>
      <th>wife_education_label</th>
      <th>husband_education_label</th>
      <th>no#_children</th>
      <th>religion_label</th>
      <th>working_label</th>
      <th>husband_occupation_label</th>
      <th>std_index_label</th>
      <th>media_exposure_label</th>
      <th>contraceptive_method</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>45</td>
      <td>low</td>
      <td>medium-high</td>
      <td>10</td>
      <td>Islam</td>
      <td>No</td>
      <td>medium-high</td>
      <td>high</td>
      <td>Good</td>
      <td>1</td>
    </tr>
    <tr>
      <th>1</th>
      <td>43</td>
      <td>medium-low</td>
      <td>medium-high</td>
      <td>7</td>
      <td>Islam</td>
      <td>No</td>
      <td>medium-high</td>
      <td>high</td>
      <td>Good</td>
      <td>1</td>
    </tr>
    <tr>
      <th>2</th>
      <td>42</td>
      <td>medium-high</td>
      <td>medium-low</td>
      <td>9</td>
      <td>Islam</td>
      <td>No</td>
      <td>medium-high</td>
      <td>medium-high</td>
      <td>Good</td>
      <td>1</td>
    </tr>
    <tr>
      <th>3</th>
      <td>36</td>
      <td>medium-high</td>
      <td>medium-high</td>
      <td>8</td>
      <td>Islam</td>
      <td>No</td>
      <td>medium-high</td>
      <td>medium-low</td>
      <td>Good</td>
      <td>1</td>
    </tr>
    <tr>
      <th>4</th>
      <td>19</td>
      <td>high</td>
      <td>high</td>
      <td>0</td>
      <td>Islam</td>
      <td>No</td>
      <td>medium-high</td>
      <td>medium-high</td>
      <td>Good</td>
      <td>1</td>
    </tr>
  </tbody>
</table>
</div>




```python
##Viewing the data types
new_df.dtypes
```




    age                          int64
    wife_education_label        object
    husband_education_label     object
    no#_children                 int64
    religion_label              object
    working_label               object
    husband_occupation_label    object
    std_index_label             object
    media_exposure_label        object
    contraceptive_method         int64
    dtype: object




```python
#Data Transformation
##OneHotEncode the dataset
transformer = make_column_transformer((OneHotEncoder(),['wife_education_label','husband_education_label','religion_label','working_label',
                      'husband_occupation_label','std_index_label','media_exposure_label']), remainder = 'passthrough')

transformed = transformer.fit_transform(new_df)

transformed_new_df = pd.DataFrame(transformed, columns = transformer.get_feature_names())

#print(transformed_new_df.head())
transformed_new_df.dtypes
```




    onehotencoder__x0_high           float64
    onehotencoder__x0_low            float64
    onehotencoder__x0_medium-high    float64
    onehotencoder__x0_medium-low     float64
    onehotencoder__x1_high           float64
    onehotencoder__x1_low            float64
    onehotencoder__x1_medium-high    float64
    onehotencoder__x1_medium-low     float64
    onehotencoder__x2_Islam          float64
    onehotencoder__x2_Non-Islam      float64
    onehotencoder__x3_No             float64
    onehotencoder__x3_Yes            float64
    onehotencoder__x4_high           float64
    onehotencoder__x4_low            float64
    onehotencoder__x4_medium-high    float64
    onehotencoder__x4_medium-low     float64
    onehotencoder__x5_high           float64
    onehotencoder__x5_low            float64
    onehotencoder__x5_medium-high    float64
    onehotencoder__x5_medium-low     float64
    onehotencoder__x6_Good           float64
    onehotencoder__x6_Not good       float64
    age                              float64
    no#_children                     float64
    contraceptive_method             float64
    dtype: object




```python
## Convert contraceptive method from a float to a category data type for classification purpose
s= transformed_new_df.loc[:,('contraceptive_method')].astype('category')
transformed_new_df.insert(len(new_df.columns),'contraceptive_method_label',s.values)
transformed_new_df = transformed_new_df.drop('contraceptive_method', axis=1)
## Convert age, no#_children to interger data type
transformed_new_df[['age','no#_children']]= transformed_new_df[['age','no#_children']].astype('int')
transformed_new_df.dtypes
```




    onehotencoder__x0_high            float64
    onehotencoder__x0_low             float64
    onehotencoder__x0_medium-high     float64
    onehotencoder__x0_medium-low      float64
    onehotencoder__x1_high            float64
    onehotencoder__x1_low             float64
    onehotencoder__x1_medium-high     float64
    onehotencoder__x1_medium-low      float64
    onehotencoder__x2_Islam           float64
    onehotencoder__x2_Non-Islam       float64
    contraceptive_method_label       category
    onehotencoder__x3_No              float64
    onehotencoder__x3_Yes             float64
    onehotencoder__x4_high            float64
    onehotencoder__x4_low             float64
    onehotencoder__x4_medium-high     float64
    onehotencoder__x4_medium-low      float64
    onehotencoder__x5_high            float64
    onehotencoder__x5_low             float64
    onehotencoder__x5_medium-high     float64
    onehotencoder__x5_medium-low      float64
    onehotencoder__x6_Good            float64
    onehotencoder__x6_Not good        float64
    age                                 int32
    no#_children                        int32
    dtype: object




```python
#Implementing Decision Trees
#Prepairing data for modeling
inputs = transformed_new_df.drop('contraceptive_method_label', axis='columns')
target = transformed_new_df['contraceptive_method_label']

from sklearn.model_selection import train_test_split
X = inputs
y = target
X_train, X_test, y_train, y_test = train_test_split(X,y, test_size=0.33)

```


```python
#Decision Tree Classifier
from sklearn.tree import DecisionTreeClassifier
clf = DecisionTreeClassifier()
clf = clf.fit(X_train, y_train)
```


```python
predictions = clf.predict(X_test)

```


```python
#Prediction Test of the Decision Tree Classifier on the Test data set
from sklearn.metrics import accuracy_score
from sklearn import tree
a = accuracy_score(clf.predict(X_train),y_train)
b = accuracy_score(y_test, predictions)
print("Training accuracy of the model was at:",a)
print("Validation accuracy of the model was at:",b)
#print("This", accuracy_score)
#print("That", accuracy_vscore)
```

    Training accuracy of the model was at: 0.9665314401622718
    Validation accuracy of the model was at: 0.5061728395061729
    

## In conclusion
One-hot-encoding is a powerful technique to treat categorical data, but it can lead to increased dimensionality, sparsity, and overfitting. It is essential to use it cautiously. Our decision tree model had a low accuracy score of 50.62%, while the training score was high at 96.65%. Overfitting occurs when a model fits too closely to the training data and may become less accurate when encountering new data or predicting future outcomes. Methods such as pruning were not applied in this case. Many factors must be considered when faced with a task, such as predicting contraceptive methods. More variables must be factored in, and the correct context must be considered. 
