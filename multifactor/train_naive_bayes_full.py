import pandas as pd
# from sklearn.naive_bayes import CategoricalNB
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import confusion_matrix

data = pd.read_csv("./naive_bayes_model_training_data.tsv", sep="\t")

# train_data.fillna(None)

labeled_data = data.dropna(subset=['SP.Outperforms.GS10'])

predictors = ['SP.Momentum.1Mo.Negative','SP.Momentum.6Mo.Negative','SP.Momentum.12Mo.Negative',
  'Low.Risk.Premium', 'GS10.Tbill.Spread.Positive', 'Bull.Bear.Spread.Positive']

# drop NA columns in training data
# TODO train model on different predictors independently to include more data
nonna_data = labeled_data.dropna(subset=predictors)

X = nonna_data[predictors]

# inspect
X.iloc[:,:3].tail()
X.iloc[:,:3].describe()
X.iloc[:,3:].tail()
X.iloc[:,3:].describe()

y = nonna_data['SP.Outperforms.GS10']

import pdb; pdb.set_trace()

clf = MultinomialNB()

clf.fit(X, y)

# try predicting the training data
y_pred = clf.predict(X)

confusion_matrix(y, y_pred)