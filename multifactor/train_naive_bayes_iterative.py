# train naive bayes iteratively and test performance at each month

import pandas as pd
# from sklearn.naive_bayes import CategoricalNB
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.metrics import confusion_matrix, accuracy_score
from scipy.stats import fisher_exact
import numpy as np

data = pd.read_csv("./naive_bayes_model_training_data.tsv", sep="\t")

# train_data.fillna(None)

labeled_data = data.dropna(subset=['SP.Outperforms.GS10'])

test_df_output_filename = "./test_df_output_filename1.tsv"

predictors = ['SP.Momentum.1Mo.Negative','SP.Momentum.6Mo.Negative','SP.Momentum.12Mo.Negative',
  'Low.Risk.Premium', 'GS10.Tbill.Spread.Positive']

y_col = 'SP.Outperforms.GS10'

metadata_columns = ['Date', 'Log.SP.Return.Forward', 'Log.GS10.Return.Forward'] 
  # 'Log.SP.Return.Forward.Cumulative', 'Log.GS10.Return.Forward.Cumulative']

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

## train model iteratively
## each month, train on history to that point and predict that month
clf_iter = BernoulliNB()

# start with 20 years
initial_idx = 240

y_preds_iterative = []
keep_columns = predictors + [y_col] + metadata_columns
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

# import pdb; pdb.set_trace()

test_df_iterative['y_pred'] = y_preds_iterative

test_df_iterative.to_csv(test_df_output_filename,sep="\t")

iterative_confusion_matrix = confusion_matrix(test_df_iterative[y_col], test_df_iterative['y_pred'])
print(iterative_confusion_matrix)

## calculate performance
sp_return_log = np.sum(test_df_iterative['Log.SP.Return.Forward'])
gs10_return_log = np.sum(test_df_iterative['Log.GS10.Return.Forward'])
model_return_sp_log = np.dot(test_df_iterative['Log.SP.Return.Forward'], test_df_iterative['y_pred'])
model_return_gs10_log = np.dot(test_df_iterative['Log.GS10.Return.Forward'], np.subtract(1, test_df_iterative['y_pred']))
model_return = np.exp(model_return_sp_log + model_return_gs10_log)
sp_return = np.exp(sp_return_log)
gs10_return = np.exp(gs10_return_log)

import pdb; pdb.set_trace()

accuracy = accuracy_score(test_df_iterative[y_col], test_df_iterative['y_pred'])
print("classification accuracy: {}".format(accuracy))

print("confusion matrix:")
print(iterative_confusion_matrix)

print("Test Period Returns:")
print("SP 500: {0:0.3f}".format(sp_return))
print("10-year Treasuries: {0:0.3f}".format(gs10_return))
print("Model: {0:0.3f}".format(model_return))


