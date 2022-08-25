import pandas as pd
# from sklearn.naive_bayes import CategoricalNB
from sklearn.naive_bayes import MultinomialNB

data = pd.read_csv("./sp_vs_gs10_training_data.tsv", sep="\t")

# train_data.fillna(None)

train_data = data.dropna(subset=['SP.Outperforms.GS10'])

X_cols = train_data.columns
X_cols = X_cols.drop('Date')
X_cols = X_cols.drop('SP.Outperforms.GS10')
# X_cols = X_cols.drop('Bullish.High')

X = train_data[X_cols]
# X = X.dropna()

y = train_data['SP.Outperforms.GS10']


clf = MultinomialNB()

clf.fit(X, y)


