﻿https://github.com/CSE6242-Team-102/NLP


# Suggestive Title Maker
CSE6242- Team 102 (Spring 2023)


# Data sourcing
https://www.kaggle.com/datasets/rsrishav/youtube-trending-video-dataset <br>
Only download US and CA and GB csv and json files and save under folder 'youtube-trending-video-dataset'


# Description
In TensorFlow and Keras for Long Short-Term Memory (LSTM), happens through the tf.keras.layers.LSTM class. LSTM based neural networks have played an important role in the field of Natural Language Processing. In addition, they have been used widely for sequence modeling. The reason why LSTMs have been used widely for this is because the model connects back to itself during a forward pass of your samples, and thus benefits from context generated by previous predictions when prediction for any new sample.


# Installation/Execution Instructions
1. Create virtual environment of your choice. Select Python version 3.7 for your kernel.
2. Install the following libraries in your environment. Refer to Requirements.txt file.
3. If running model for the first time:


**Main Python Script:** youtube-title-suggestion-model-Copy1.ipynb <br>
For each Video Content Category, the following outputs will be generated: <br>
* keras_next_word_model.h5: Saved LSTM model
* history.p: Checkpoints
* tokenzier.pkl: Tokernizer
* nextword1.h5: Callbacks <br>
* FOLDER??


**Prediction Script:** Predictions.ipynb


4. For prior saved model outputs that can be reused refer to folder 'Prior Runs'