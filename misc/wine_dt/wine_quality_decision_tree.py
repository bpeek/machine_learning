import pandas as pd
import requests
from io import StringIO
from sklearn.tree import DecisionTreeClassifier, export_graphviz


data_link = "http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
data_response_obj = requests.get(data_link)
data = StringIO(data_response_obj.text)

wine_df = pd.read_csv(data, sep=";")

print("* df.head()", wine_df.head(), sep="\n", end="\n\n")
print("* df.tail()", wine_df.tail(), sep="\n", end="\n\n")

features = list(wine_df.columns[:11])

print("* Features", features, sep = '\n')

y = wine_df["quality"]
x = wine_df[features]

decision_tree = DecisionTreeClassifier(criterion = 'entropy', max_depth = 3, random_state = 0)

decision_tree.fit(x, y)

f = open("wine_dt.dot", "w")
export_graphviz(decision_tree, out_file=f, feature_names = features)
