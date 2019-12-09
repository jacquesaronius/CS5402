import pandas as pd
import numpy
from sklearn.model_selection import train_test_split
from sklearn.ensemble import BaggingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import metrics
from sklearn.metrics import confusion_matrix

df = pd.read_csv("hw8_data.csv")
x = df.iloc[:, 1:37]
y = df.iloc[:, 0]

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.33, random_state=0)

model = BaggingClassifier(DecisionTreeClassifier(max_depth=None), n_estimators=100, random_state=0)
model.fit(x_train, y_train)
y_pred = model.predict(x_test)
print('Mean Absolute Error:', metrics.mean_absolute_error(y_test, y_pred)) 
print(confusion_matrix(y_test, y_pred))