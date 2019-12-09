import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from sklearn.ensemble import BaggingClassifier
from sklearn.tree import DecisionTreeRegressor

df = pd.read_csv("hw8_data.csv")

x = df.iloc[:, 1:37].values
y = df.iloc[:, 0].values

cv = KFold(n_splits=10, random_state=42, shuffle=False)
scores = []

for train_index, test_index in cv.split(x):
    print("Train Index: ", train_index, "\n")
    print("Test Index: ", test_index)

    x_train, x_test = x[train_index], x[test_index]
    y_train, y_test = y[train_index], y[test_index] 

    r = DecisionTreeRegressor() 
    r.fit(x_train, y_train)

    scores.append(r.score(x_test, y_test))

    print(np.mean(scores))  
