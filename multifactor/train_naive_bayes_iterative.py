# train naive bayes iteratively and test performance at each month

import pandas as pd
# from sklearn.naive_bayes import CategoricalNB
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.metrics import confusion_matrix
from scipy.stats import fisher_exact
import numpy as np

data = pd.read_csv("./naive_bayes_model_training_data.tsv", sep="\t")

# train_data.fillna(None)

labeled_data = data.dropna(subset=['SP.Outperforms.GS10'])

predictors = ['SP.Momentum.1Mo.Negative','SP.Momentum.6Mo.Negative','SP.Momentum.12Mo.Negative',
  'Low.Risk.Premium', 'GS10.Tbill.Spread.Positive']
y_col = 'SP.Outperforms.GS10'

# drop NA columns in training data
# TODO train model on different predictors independently to include more data
nonna_data = labeled_data.dropna(subset=predictors)

X = nonna_data[predictors]

# inspect
X.iloc[:,:3].tail()
X.iloc[:,:3].describe()
X.iloc[:,3:].tail()
X.iloc[:,3:].describe()

y = nonna_data[y_col]

# clf_full = MultinomialNB()
clf_full = BernoulliNB()

clf_full.fit(X, y)

# try predicting the training data
y_pred = clf_full.predict(X)

confusion_matrix(y, y_pred)
clf_full.score(X, y)

# import pdb; pdb.set_trace()

nonna_data['Date.Raw'].describe()

# train model each month, predict each month
clf_iter = BernoulliNB()

# start with 20 years
initial_idx = 240

y_preds_iterative = []
keep_columns = predictors + [y_col] + ['Date']
test_df_iterative = nonna_data[keep_columns].iloc[initial_idx:]

for idx in range(initial_idx,nonna_data.shape[0]):
  print("index {}".format(idx))
  # train on data up to idx
  X_train_current = nonna_data[predictors].iloc[0:idx]
  y_train_current = nonna_data[y_col].iloc[0:idx]
  clf_iter.fit(X_train_current, y_train_current)
  # predict data point at idx
  X_current = nonna_data[predictors].iloc[idx:idx+1]
  y_pred_current_array = clf_iter.predict(X_current)
  y_pred_current = y_pred_current_array[0]
  y_preds_iterative.append(y_pred_current)

import pdb; pdb.set_trace()

test_df_iterative['y_pred'] = y_preds_iterative

test_df_iterative.to_csv("./test_df_iterative.tsv",sep="\t")

iterative_confusion_matrix = confusion_matrix(test_df_iterative[y_col], test_df_iterative['y_pred'])
print(iterative_confusion_matrix)
