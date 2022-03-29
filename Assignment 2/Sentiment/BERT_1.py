# This is a sample Python script.

# Press ⌃R to execute it or replace it with your code.
# Press Double ⇧ to search everywhere for classes, files, tool windows, actions, and settings.


# Press the green button in the gutter to run the script.

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import pandas as pd
from keras.layers import Dense
from keras.utils.np_utils import to_categorical
from transformers import BertTokenizer, TFBertForSequenceClassification
from transformers import InputExample, InputFeatures


import pandas as pd
tweets = pd.read_csv("/Users/xavierverbrugge/Documents/School/Master/Sem 2/Social Media and Web Analytics/Groupwork/Tweets_And_Labels_2.csv")

from sklearn.model_selection import train_test_split

df_train, df_test = train_test_split(tweets, test_size=0.4)
df_val, df_test = train_test_split(df_test, test_size=0.5)

df_train = df_train[["text","Sentiment_label"]]
df_train["Sentiment_label"] = df_train["Sentiment_label"] + 2

df_val = df_val[["text","Sentiment_label"]]
df_val["Sentiment_label"] = df_val["Sentiment_label"] + 2

df_train["Sentiment_label"] = df_train["Sentiment_label"].fillna(0)
df_val["Sentiment_label"] = df_val["Sentiment_label"].fillna(0)
df_test["Sentiment_label"] = df_test["Sentiment_label"].fillna(0)

print(df_train.Sentiment_label)
y_train = to_categorical(df_train["Sentiment_label"], num_classes= 5)
y_test = to_categorical(df_test["Sentiment_label"], num_classes= 5)


# Transformers
import transformers

from transformers import AutoTokenizer,TFBertModel
tokenizer = AutoTokenizer.from_pretrained("bert-base-cased")
bert = TFBertModel.from_pretrained("bert-base-cased")


## Tokenize the input (takes some time)
# here tokenizer using from bert-base-cased
# convert the input textual data into BERT’s input data format using a tokenizer.
x_train = tokenizer(
    text=df_train.text.tolist(),
    add_special_tokens=True,
    max_length=70,
    truncation=True,
    padding=True,
    return_tensors='tf',
    return_token_type_ids = False,
    return_attention_mask = True,
    verbose = True)
x_test = tokenizer(
    text=df_test.text.tolist(),
    add_special_tokens=True,
    max_length=70,
    truncation=True,
    padding=True,
    return_tensors='tf',
    return_token_type_ids = False,
    return_attention_mask = True,
    verbose = True)

input_ids = x_train["input_ids"]
attention_mask = x_train['attention_mask']


#### Model building

import tensorflow as tf
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras.initializers import TruncatedNormal
from tensorflow.keras.losses import CategoricalCrossentropy
from tensorflow.keras.metrics import CategoricalAccuracy
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.layers import Input, Dense
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from keras.models import Sequential
from keras.layers import Activation, Dense, Dropout
from keras import optimizers
from sklearn.metrics import confusion_matrix,accuracy_score, roc_curve, auc

max_len = 70
input_ids = Input(shape=(max_len,), dtype=tf.int32, name="input_ids")
input_mask = Input(shape=(max_len,), dtype=tf.int32, name="attention_mask")
embeddings = bert(input_ids,attention_mask = input_mask)[0]
out = tf.keras.layers.GlobalMaxPool1D()(embeddings)
out = Dense(128, activation='relu')(out)
out = tf.keras.layers.Dropout(0.1)(out)
out = Dense(32,activation = 'relu')(out)
y = Dense(5,activation = 'sigmoid')(out)
model = tf.keras.Model(inputs=[input_ids, input_mask], outputs=y)
model.layers[2].trainable = True

## Compile
optimizer = Adam(
    learning_rate=5e-05, # this learning rate is for bert model , taken from huggingface website
    epsilon=1e-08,
    decay=0.01,
    clipnorm=1.0)
# Set loss and metrics
loss =CategoricalCrossentropy(from_logits = True)
metric = CategoricalAccuracy('balanced_accuracy')
# Compile the model
model.compile(
    optimizer = optimizer,
    loss = loss,
    metrics = metric)

train_history = model.fit(
    x ={'input_ids':x_train['input_ids'],'attention_mask':x_train['attention_mask']} ,
    y = y_train,
    validation_data = (
        {'input_ids':x_test['input_ids'],'attention_mask':x_test['attention_mask']}, y_test
    ),
    epochs=2,
    batch_size=36
)

predicted_raw = model.predict({'input_ids':x_test['input_ids'],'attention_mask':x_test['attention_mask']})

y_predicted = np.argmax(predicted_raw, axis = 1)
y_true = df_test.Sentiment_label

from sklearn.metrics import classification_report, accuracy_score, roc_curve

print(classification_report(y_true, y_predicted))

texts = input(str('input the text'))
x_val = tokenizer(
    text=texts,
    add_special_tokens=True,
    max_length=70,
    truncation=True,
    padding='max_length',
    return_tensors='tf',
    return_token_type_ids = False,
    return_attention_mask = True,
    verbose = True)
validation = model.predict({'input_ids':x_val['input_ids'],'attention_mask':x_val['attention_mask']})*100

