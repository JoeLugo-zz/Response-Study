import numpy as np
import pandas as pd
import datetime
import sklearn as sk
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.linear_model import LogisticRegression
import nltk
from nltk.corpus import stopwords
from patsy import dmatrices
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn import preprocessing
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.metrics import mean_squared_error, confusion_matrix, classification_report
from sklearn.feature_selection import SelectFromModel
from sklearn import cross_validation
from sklearn.ensemble import RandomForestRegressor
from sklearn import svm
from sklearn.ensemble import GradientBoostingClassifier
from IPython.core.debugger import Tracer

le = preprocessing.LabelEncoder()
sno = nltk.stem.SnowballStemmer('english')
vectorizer = CountVectorizer(min_df=1, stop_words= "english")
analyze = vectorizer.build_analyzer()
transformer = TfidfTransformer(smooth_idf=False)

CHRISTELLE_FILE = "ResponseTimeStudyCHRISTELLE_Sheet1.tsv"
JOE_FILE = "ResponseTimeStudyJOE_Sheet1.tsv"

def stemmed_words(doc):
    return " ".join(sno.stem(w) for w in analyze(doc))

def date_count(dataframe):

	s_list = []
	n_list = []
	d_list = []

	for index in range(0,len(dataframe.Text)):
	    # getting the stemmed words
	    s_list.append(stemmed_words(dataframe.Text[index]))
	    # getting the number of words 
	    n_list.append(len(dataframe.Text[index].split()))
	    # getting the dates formatted correctly
	    d_list.append(datetime.datetime.strptime(dataframe.Date[index], "%B %d, %Y at %I:%M%p"))	

	dataframe.Text = s_list
	dataframe["nwords"] = n_list
	dataframe.Date = d_list

	return dataframe

def split_df(dataframe):

	df = pd.DataFrame((vectorizer.fit_transform(dataframe.Text)).toarray())
	df.columns = vectorizer.get_feature_names()

	for i in df.columns:
	    if sum(df[i]) <= 5:
	        df = df.drop(i, 1)

	tfidf = transformer.fit_transform(df).toarray()
	df = pd.DataFrame(data=tfidf, columns=df.columns)

	dataframe = pd.concat([dataframe.reset_index(drop=True), df], axis=1)

	return dataframe

def cleanup():

	# Getting rid of the Number column
	christelle_text = pd.read_csv(filepath_or_buffer=CHRISTELLE_FILE, sep="\t", usecols=[0,1,3])
	joe_text = pd.read_csv(filepath_or_buffer=JOE_FILE, sep="\t", usecols=[0,1,3])

	christelle_text = split_df(date_count(christelle_text))
	joe_text = split_df(date_count(joe_text))

	combined = joe_text.append(pd.DataFrame(data=christelle_text), ignore_index=True)
	combined = combined.fillna(0)
	combined = combined.sort_values("Date", ascending=True)

	keep = []
	curr = ""
	for item in combined.Name:
	    if item == curr:
	        keep.append(0)
	    else:
	        keep.append(1)
	    curr = item

	combined["Keep"] = keep
	combined = combined[combined.Keep == 1]
	combined = combined.sort_values("Date", ascending=False)

	delt = []
	curr = ""
	for item in combined.Date:
	    if curr == "":
	        delt.append(np.nan)
	    else:
	    	# Getting the minutes from the datetime
	        delt.append(((curr-item).seconds)%3600/60.0)
	    curr = item
	    
	del delt[0]
	delt.append(np.nan)
	combined["response_time"] = delt

	combined["Month"] = [x.strftime('%m') for x in combined.Date]
	combined["Weekday"] = [x.strftime('%w') for x in combined.Date]
	combined["Hour"] = [x.strftime('%H') for x in combined.Date]

	joe_mean = (combined[combined.Name == "Joseph Lugo"].response_time).mean()
	christelle_mean = (combined[combined.Name == "Christelle Mamitag"].response_time).mean()
	total_mean = (combined.response_time).mean()
	combined["response_cat"] = combined.response_time > total_mean
	combined = combined.dropna()
	combined = combined.reset_index()
	prev_response = combined.response_time
	prev_response = list(combined.response_time[1:])
	prev_response.append(np.nan)
	combined["prev_response_time"] = prev_response
	combined_clean = combined[combined.response_time <= 180]
	del combined_clean["index"]
	del combined_clean["Keep"]
	del combined_clean["Text"]
	del combined_clean["Date"]
	combined_clean = combined_clean.dropna()

	combined_clean.Name = le.fit_transform(combined_clean.Name)

	return combined_clean

def reduce(dataframe, response="response_cat"):

	if "response_time" in dataframe.columns:
		dataframe_drop = dataframe.drop("response_time", 1)

	X = dataframe_drop.drop(response, 1)
	y = dataframe_drop[response]

	clf = ExtraTreesClassifier()
	clf = clf.fit(X, y)
	important = (clf.feature_importances_).tolist()

	importance_table = pd.DataFrame()
	importance_table["colname"] = X.columns
	importance_table["importance"] = important
	importance_table = importance_table.sort_values("importance", ascending=False)
	reduced = importance_table[importance_table.importance >= importance_table.importance.mean()]
	keep = (reduced.colname.tolist())
	keep.extend(["response_cat"])

	reduced_data = dataframe[keep]

	return reduced_data

def pred(dataframe, algorithm, response="response_cat", confusion_matrix=TRUE):

	if "response_time" in dataframe.columns:
		dataframe = dataframe.drop("response_time", 1)

	X = dataframe.drop(response, 1)
	y = dataframe[response]

	# subsetting the data to a 70-30 split
	X_train, X_test, y_train, y_test = cross_validation.train_test_split(X, y, test_size=0.3, random_state=4)

	clf = algorithm
		
	clf.fit(X_train,y_train)

	if confusion_matrix:
		expected = y_train
		predicted = clf.predict(X_train) >= 0.5

		print("Classification report")
		print(classification_report(expected, predicted))

	print("Train R^2: {}".format(clf.score(X_train, y_train)))
	print("Test R^2: {}".format(clf.score(X_test, y_test)))

	y_pred_train = clf.predict(X_train)
	y_pred_validation = clf.predict(X_test)

	print("Train mse: {}".format(mean_squared_error(y_train, y_pred_train)))
	print("Test mse: {}".format(mean_squared_error(y_test, y_pred_validation)))

	mse = cross_validation.cross_val_score(clf, X, y, cv=10, scoring='mean_squared_error')
	print("MSE 10 fold CV: %0.4f (+/- %0.4f)" % (mse.mean(), mse.std() * 2))
	rs = cross_validation.cross_val_score(clf, X, y, cv=10, scoring='r2')
	print("R^2 10 fold CV: %0.4f (+/- %0.4f)" % (rs.mean(), rs.std() * 2))

def main():
	cleanup()

if __name__ == '__main__':
	main()

