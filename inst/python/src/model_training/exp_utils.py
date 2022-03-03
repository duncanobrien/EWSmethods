import os
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import random
import warnings
import tensorflow as tf
import seaborn as sns
from sklearn.metrics import  confusion_matrix
from itertools import cycle
from sklearn.preprocessing import LabelEncoder
from tensorflow.keras.models import Model
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.utils import to_categorical
from tensorflow.keras.callbacks import ModelCheckpoint, ReduceLROnPlateau, LearningRateScheduler
from tensorflow.keras import backend as K
from src.utils.generic_utils import load_dataset_at, plot_roc

def train_model(model: Model, dataset_id, dataset_prefix, epochs=50, batch_size=128, val_subset=5000,
                cutoff=None, normalize_timeseries=False, learning_rate=1e-5,prediction=False):
    """
    Trains a provided Model, given a dataset id.

    Args:
        model: A Keras Model.
        dataset_id: Integer id representing the dataset index containd in `utils/constants.py`.
        dataset_prefix: Name of the dataset. Used for weight saving.
        epochs: Number of epochs to train.
        batch_size: Size of each batch for training.
        val_subset: Optional integer id to subset the test set. To be used if the test set evaluation time significantly surpasses training time per epoch.
        cutoff: Optional integer which slices of the first `cutoff` timesteps from the input signal.
        normalize_timeseries: Bool / Integer. Determines whether to normalize the timeseries.
            If False, does not normalize the time series.
            If True / int not equal to 2, performs standard sample-wise z-normalization.
            If 2: Performs full dataset z-normalization.
        learning_rate: Initial learning rate.
    
    Returns:
        The trained model.
        
    """
    X_train, y_train, X_test, y_test, is_timeseries = load_dataset_at(dataset_id,normalize_timeseries=normalize_timeseries)
    classes = np.unique(y_train)
    le    = LabelEncoder()
    y_ind = le.fit_transform(y_train.ravel())
    recip_freq = len(y_train) / (len(le.classes_) *
                                 np.bincount(y_ind).astype(np.float64))
    class_weight = recip_freq[le.transform(classes)]
    print("Class weights : ", class_weight)

    y_train = to_categorical(y_train, len(np.unique(y_train)))
    y_test = to_categorical(y_test, len(np.unique(y_test)))
    
    if is_timeseries:
        factor = 1. / np.cbrt(2)
    else:
        factor = 1. / np.sqrt(2)
    path_splits = os.path.split(dataset_prefix)
    if len(path_splits) > 1:
        base_path = os.path.join('weights', *path_splits)
        if not os.path.exists(base_path):
            os.makedirs(base_path)
        base_path = os.path.join(base_path, path_splits[-1])
    else:
        all_weights_path = os.path.join('weights', dataset_prefix)
        if not os.path.exists(all_weights_path):
            os.makedirs(all_weights_path)

    model_checkpoint = ModelCheckpoint("./weights/%s_weights.h5" % dataset_prefix, verbose=1,monitor='loss', save_best_only=True, save_weights_only=True)
    reduce_lr = ReduceLROnPlateau(monitor='loss', patience=10, mode='auto',factor=factor, cooldown=0, min_lr=1e-6, verbose=1)
    es = tf.keras.callbacks.EarlyStopping(monitor='val_accuracy', patience=50)
    callback_list = [model_checkpoint, reduce_lr,es]
    optm = Adam(lr=learning_rate)
    model.compile(optimizer=optm, loss='categorical_crossentropy', metrics=['accuracy'])

    if val_subset is not None:
        X_test = X_test[:val_subset]
        y_test = y_test[:val_subset]

    seqModel = model.fit(X_train, y_train, batch_size=batch_size, epochs=epochs, callbacks=callback_list, verbose=1, validation_data=(X_test, y_test))    
    return model,seqModel.history


def evaluate_model(model: Model, dataset_id, dataset_prefix, batch_size=128, test_data_subset=None,
                   cutoff=None, normalize_timeseries=False,error_analysis=False):
    """
    Evaluates a given Keras Model on the provided dataset.

    Args:
        model: A Keras Model.
        dataset_id: Integer id representing the dataset index containd in
            `utils/constants.py`.
        dataset_prefix: Name of the dataset. Used for weight saving.
        batch_size: Size of each batch for evaluation.
        test_data_subset: Optional integer id to subset the test set. To be used if
            the test set evaluation time is significantly.
        cutoff: Optional integer which slices of the first `cutoff` timesteps
            from the input signal.
        normalize_timeseries: Bool / Integer. Determines whether to normalize
            the timeseries.
            If False, does not normalize the time series.
            If True / int not equal to 2, performs standard sample-wise z-normalization.
            If 2: Performs full dataset z-normalization.

    Returns:
        The test set accuracy of the model.
        
    """
    _, _, X_test, y_test, is_timeseries = load_dataset_at(dataset_id,normalize_timeseries=normalize_timeseries)

    y_test = to_categorical(y_test, len(np.unique(y_test)))
    optm   = Adam()

    model.compile(optimizer=optm, loss='categorical_crossentropy', metrics=['accuracy',tf.keras.metrics.TruePositives(),tf.keras.metrics.FalsePositives(),tf.keras.metrics.TrueNegatives(),tf.keras.metrics.FalseNegatives()])
    model.load_weights("./weights/%s_weights.h5" % dataset_prefix)

    print("Weights loaded from ", "./weights/%s_weights.h5" % dataset_prefix)

    if test_data_subset is not None:
        X_test = X_test[test_data_subset]
        y_test = y_test[test_data_subset]
    loss, accuracy,TP,FP,TN,FN = model.evaluate(X_test, y_test, batch_size=batch_size)
    print("Final Accuracy : ", accuracy)

    Y_pred = model.predict(X_test)
    plot_roc(y_test,Y_pred,dataset_prefix)    
    y_pred = np.argmax(Y_pred, axis=1)
    y_test = np.argmax(y_test, axis=1)

    cm=confusion_matrix(y_test,y_pred,normalize='true')    
    plt.rcParams["axes.edgecolor"] = "0.15"
    plt.rcParams["axes.linewidth"]  = 1.50
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    fig,ax= plt.subplots(1,1,figsize=(5,4))
    sns.heatmap(cm, annot=True, ax = ax,cmap='RdPu_r',square=True,linecolor='k',linewidth=1.25,annot_kws={"fontsize":12, "weight": "bold"}); 
    ax.set_xlabel('Predicted labels',fontsize=15)
    ax.set_ylabel('True labels',fontsize=15)
    ax.xaxis.tick_top() 
    ax.xaxis.set_label_position('top')
    ax.xaxis.set_ticklabels(['N.T', 'S.T','C.T'])
    ax.yaxis.set_ticklabels(['N.T', 'S.T','C.T'])
    cbar = ax.collections[0].colorbar
    cbar.ax.tick_params(labelsize=12)   
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.show()

    return accuracy

