import os
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import random
import warnings
import tensorflow as tf
import seaborn as sns
import pickle
from itertools import cycle
from scipy import interp
from sklearn.metrics import roc_curve, auc, confusion_matrix
from tensorflow.keras import backend as K
from src.utils.constants import TRAIN_FILES, TEST_FILES, NB_CLASSES_LIST


def plot_roc(y_test,y_score,figname="none",n_classes=3): 
    """
    
    Plots the ROC Curve given the target and the prediction probabilities 

    Args:
        y_test: The target class labels
        y_score: The models output prediction probabilities
        figname: Name of the figure for saving.
        n_classes: Number of classes.
        

    """
    sns.set_style('ticks')
    lw = 8.0
    colors = cycle(['purple','crimson','lightpink',])
    # Compute ROC curve and ROC area for each class
    fpr = dict()
    tpr = dict()
    roc_auc = dict()

    for i in range(n_classes):
        fpr[i], tpr[i], _ = roc_curve(y_test[:, i], y_score[:, i])
        roc_auc[i] = auc(fpr[i], tpr[i])
    # Compute micro-average ROC curve and ROC area
    fpr["micro"], tpr["micro"], _ = roc_curve(y_test.ravel(), y_score.ravel())
    roc_auc["micro"] = auc(fpr["micro"], tpr["micro"])
    # First aggregate all false positive rates
    all_fpr = np.unique(np.concatenate([fpr[i] for i in range(n_classes)]))
    # Then interpolate all ROC curves at this points
    mean_tpr = np.zeros_like(all_fpr)
    for i in range(n_classes):
        mean_tpr += interp(all_fpr, fpr[i], tpr[i])
    # Finally average it and compute AUC
    mean_tpr    /= n_classes
    fpr["macro"] = all_fpr
    tpr["macro"] = mean_tpr
    roc_auc["macro"] = auc(fpr["macro"], tpr["macro"])
    # Plot all ROC curves
    plt.rcParams["axes.edgecolor"] = "0.15"
    plt.rcParams["axes.linewidth"]  = 1.50
    plt.figure(3,figsize=(5,5))
    plt.plot(fpr["micro"], tpr["micro"],label='Micro-Average ROC \n (area = {0:0.2f})'.format(roc_auc["micro"]),color='crimson', linestyle='-', linewidth=2.5) 
    plt.plot([0, 1], [0, 1], linestyle='--',color='purple', lw=2.0)
    plt.xlim([-0.1, 1.1])
    plt.ylim([-0.1, 1.1])
    plt.xticks(fontsize=14)
    plt.yticks(fontsize=14)
    plt.xlabel('False Positive Rate',fontsize=15)
    plt.ylabel('True Positive Rate',fontsize=15)
    plt.legend(fontsize=14,ncol=1)
    plt.show()

def load_dataset_at(index, normalize_timeseries=False, verbose=True,is_timeseries = True) -> (np.array, np.array):
    """
    Loads a Univaraite Dataset indexed by `utils.constants`. The dataset is loaded as a pandas DataFrame and preprocessed to replace missing values with zero.  
    
    .. note:: The dataset should be such that the first column corresponds to the target class label. i.e a dataset consisting of N time series of length T would be a dataframe of dimension Nx(T+1) where the first column corresponds to the class labels.
    
    Args:
        index: Integer index, set inside `utils.constants` that refers to the dataset.
        normalize_timeseries: Bool / Integer. Determines whether to normalize the timeseries.
                If False, does not normalize the time series.
                If True / int not equal to 2, performs standard sample-wise z-normalization.
                If 2: Performs full dataset z-normalization.
        verbose: Whether to describe the dataset being loaded.

    Returns:
        A tuple of shape (X_train, y_train, X_test, y_test, is_timeseries).
        For legacy reasons, is_timeseries is always True.

    """

    assert index < len(TRAIN_FILES), "Index invalid. Could not load dataset at %d" % index
    if verbose: print("Loading train / test dataset : ", TRAIN_FILES[index], TEST_FILES[index])
    if os.path.exists(TRAIN_FILES[index]):
        df = pd.read_csv(TRAIN_FILES[index], header=None, encoding='latin-1')
    elif os.path.exists(TRAIN_FILES[index][1:]):
        df = pd.read_csv(TRAIN_FILES[index][1:], header=None, encoding='latin-1')
    else:
        raise FileNotFoundError('File %s not found!' % (TRAIN_FILES[index]))

    # remove all columns which are completely empty
    df.dropna(axis=1, how='all', inplace=True)
    # fill all missing columns with 0
    df.fillna(0, inplace=True)
    y_train    = df[[0]].values
    nb_classes = NB_CLASSES_LIST[index]
    y_train    = (y_train - y_train.min()) / (y_train.max() - y_train.min()) * (nb_classes - 1)
    # drop labels column from train set X
    df.drop(df.columns[0], axis=1, inplace=True)
    X_train = df.values
    if is_timeseries:
        X_train = X_train[:, np.newaxis, :]
        # normalize the values
        if normalize_timeseries:
            normalize_timeseries = int(normalize_timeseries)
            if normalize_timeseries == 2:
                X_train_mean = X_train.mean()
                X_train_std  = X_train.std()
                X_train      = (X_train - X_train_mean) / (X_train_std + 1e-8)
            elif normalize_timeseries == 1:
                X_train_mean = X_train.mean(axis=-1, keepdims=True)
                X_train_std  = X_train.std(axis=-1, keepdims=True)
                X_train      = (X_train - X_train_mean) / (X_train_std + 1e-8)
            else:
                X_train_mean = X_train.mean(axis=-1, keepdims=True)
                X_train      = (X_train - X_train_mean) 

    if verbose: print("Finished loading train dataset..")
    if os.path.exists(TEST_FILES[index]):
        df = pd.read_csv(TEST_FILES[index], header=None, encoding='latin-1')
    elif os.path.exists(TEST_FILES[index][1:]):
        df = pd.read_csv(TEST_FILES[index][1:], header=None, encoding='latin-1')
    else:
        raise FileNotFoundError('File %s not found!' % (TEST_FILES[index]))
    # remove all columns which are completely empty
    df.dropna(axis=1, how='all', inplace=True)
    # fill all missing columns with 0
    df.fillna(0, inplace=True)
    y_test = df[[0]].values
    # extract labels Y and normalize to [0 - (MAX - 1)] range
    y_test = (y_test - y_test.min()) / (y_test.max() - y_test.min()) * (nb_classes - 1)
    # drop labels column from train set X
    df.drop(df.columns[0], axis=1, inplace=True)
    X_test = df.values
    if is_timeseries:
        X_test = X_test[:, np.newaxis, :]
        # normalize the values
        if normalize_timeseries:
            normalize_timeseries = int(normalize_timeseries)

            if normalize_timeseries == 2:
                X_test = (X_test - X_train_mean) / (X_train_std + 1e-8)
            elif normalize_timeseries ==1 :
                X_test_mean = X_test.mean(axis=-1, keepdims=True)
                X_test_std = X_test.std(axis=-1, keepdims=True)
                X_test = (X_test - X_test_mean) / (X_test_std + 1e-8)
            else:
                X_test_mean = X_test.mean(axis=-1, keepdims=True)
                X_test = (X_test - X_test_mean)
    if verbose:
        print("Finished loading test dataset..")
        print()
        print("Number of train samples : ", X_train.shape[0], "Number of test samples : ", X_test.shape[0])
        print("Sequence length : ", X_train.shape[-1])
    return X_train, y_train, X_test, y_test, is_timeseries
