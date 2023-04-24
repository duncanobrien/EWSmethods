import os
import tensorflow as tf 
from tensorflow.keras import backend as K
from tensorflow.keras.layers import Conv1D, BatchNormalization, GlobalAveragePooling1D, Permute, Dropout, Flatten
from tensorflow.keras.layers import Input, Dense, LSTM, concatenate, Activation, GRU, SimpleRNN
from tensorflow.keras.models import Model
from tensorflow.keras import regularizers
import numpy as np 
import random
from sklearn.model_selection import train_test_split

class VariableLenModel(Model):
    
    sample_random_length = True
    augment_iter = 10
    min_window_len = 15

    def random_window(self, total_len=400):
        window_len = self.min_window_len + np.random.randint(0,total_len-self.min_window_len)
        start_idx  = np.random.randint(0,total_len-window_len)
        end_idx    = start_idx + window_len
        return start_idx,end_idx,window_len

    def train_step(self, data):
        # Unpack the data. Its structure depends on your model and
        # on what you pass to `fit()`.
        x, y = data
        start_idx, end_idx, window_len = 0, x.shape[-1], x.shape[-1]
        # print("Before Random Sampling:",x.shape,window_len)
        
        def train_iter(x_,y):
          x = x_[:,:,start_idx:end_idx]
          # print("After Random Sampling:",x.shape,window_len)
          with tf.GradientTape() as tape:
              y_pred = self(x, training=True)  # Forward pass
              # Compute the loss value
              # (the loss function is configured in `compile()`)
              loss = self.compiled_loss(y, y_pred, regularization_losses=self.losses)

          # Compute gradients
          trainable_vars = self.trainable_variables
          gradients = tape.gradient(loss, trainable_vars)
          # Update weights
          self.optimizer.apply_gradients(zip(gradients, trainable_vars))
          # Update metrics (includes the metric that tracks the loss)
          self.compiled_metrics.update_state(y, y_pred)
        
        train_iter(x,y)
        if(self.sample_random_length):
            for aiter in range(self.augment_iter):
              start_idx, end_idx, window_len = self.random_window(x.shape[-1])
              train_iter(x,y)
        
        # Return a dict mapping metric names to current value
        return {m.name: m.result() for m in self.metrics}

class EWSNet():

    def __init__(self,ensemble=1, weight_dir=None, prefix="",suffix=".h5"):
        """
        This is a wrapper class to load pretrained EWSNet models and perform inference on custom data points.
        To instanatiate this inference wrapper, you need to have pretrained weights stored in a local directory.
        Supports ensembling multiple models for increased reliability and robustness.

        Initializing the EWSNet wrapper.

        :param ensemble: A variable indicating the no. of models to ensemble and average predictions over.
        :type ensemble: int, optional

        :param weight_dir: Path to the directory to load the weights from.
        :type weight_dir: str, optional

        :param prefix: Prefix for the weight filenames
        :type prefix: str, optional

        :param suffix: Suffix for the weight filenames
        :type suffix: str, optional

        `Attributes`
    
        - model
            A list of size `ensemble` holding the corresponding models, that are instances of type model class:`tf.keras.Model` 
        
        .. note:: Note that the model weights should be saved as $_PREFIX_$i$_SUFFIX_ where i corresponds to the index of the model in the ensemble.
        
        .. note:: Once loading of the weights is successfull, use the predict() function to test custom time series data using EWSNet.

        """
    
        self.ensemble = ensemble
        self.model    = [self.build_model() for _ in range(self.ensemble)]
        if weight_dir is not None:
            self.load_model(weight_dir,prefix,suffix)
        self.labels=["No Transition","Smooth Transition","Critical Transition"]

                
    def predict(self, x, ensemble_subset=None):
        """
        Function to make predictions using EWSNet. 

        :param x: The datapoint (univariate timeseries) to test for future transitions
        :type x: 1 dimensional np.array or list , required

        
        :param ensemble_subset: Whether to make predictions using a subset of ensembles
        :type ensemble_subset: A positive integer, should lie < self.ensemble

        Returns: 
            A tuple consisting of the predicted label and the predictoin probability for each class.

        """
        ensemble_models = range(self.ensemble) if ensemble_subset is None else random.sample(range(self.ensemble),ensemble_subset) 
        x = np.array(x)
        x = np.reshape(x,(1,1,x.shape[0]))
        predictions = np.array([self.model[i](x)[0] for i in ensemble_models])
        predictions = np.mean(predictions,axis=0)
        prediction_probability = {
            "No Transition"      :predictions[0],
            "Smooth Transition"  :predictions[1],
            "Critical Transition":predictions[2],
        }
        return self.labels[np.argmax(predictions)],prediction_probability

    def finetune(self,X,y, freeze_feature_extractor=True, learning_rate = 5e-5, batch_size = 512, tune_epochs = 5):
        """
        Function to finetune EWSNet on a custom dataset. By default finetunes all models in the ensemble based on the given data and set of parameters.

        :param X: The data points (univariate timeseries) to finetune EWSNet on. Dimension - (N x D) or (N x 1 x D) where `N` denotes the no. of samples and `D` denotes the no. of time steps.
        :type X:  np.array, required

        :param y: The target labels corresponding to the data points (X). Dimension - (N, ) or (N x 1) where `N` denotes the no. of samples.
        :type y:  np.array, required

        :param freeze_feature_extractor: A boolean flag that determines the part of the network to be finetuned. When set to False. the entire network is finetuned. When set to True, only the fully connected layers are finetuned and the feature extraction blocks are frozen.
        :type freeze_feature_extractor:  bool, optional

        :param learning_rate: The learning rate for finetuning the models.
        :type learning_rate:  float, optional

        :param batch_size: The batch size for finetuning the models.
        :type batch_size:  int, optional

        :param tune_epochs: The no. of epochs for finetuning the models.
        :type tune_epochs:  int, optional

        """
        
        if(len(X.shape)==2):
            X = np.expand_dims(X,axis=1)
        X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.05, random_state=1)
        train_dataset   = tf.data.Dataset.from_tensor_slices((X_train, y_train)).batch(batch_size)
        val_dataset     = tf.data.Dataset.from_tensor_slices((X_val, y_val)).batch(batch_size)
        for i in range(self.ensemble):
            print("==> Fine tuning {}-th model in the ensemble on the given data.".format(i+1))
            if(freeze_feature_extractor):
                trainable_layers = ["dense","dense_1","lstm","dropout","concatenate"]
                self.model[i].trainable = False
                for layer in self.model[i].layers:
                    if(layer.name in trainable_layers):
                        layer.trainable = True
            self.model[i].compile(optimizer=tf.keras.optimizers.Adam(learning_rate=learning_rate),loss=tf.keras.losses.sparse_categorical_crossentropy,metrics=['accuracy'])
            self.model[i].fit(train_dataset,epochs=tune_epochs,validation_data=val_dataset)
    
    def build_model(self):
        """
        Function to define and build the neural network architecture for EWSNet

        """
        
        ip = Input(shape=(1, None))
        x = Permute((2, 1))(ip)
        x = LSTM(128)(x)
        x = Dropout(0.2)(x)
        y = Permute((2, 1))(ip)
        y = Conv1D(128, 8, padding='same', kernel_initializer='he_uniform')(y)
        y = BatchNormalization()(y)
        y = Activation('relu')(y)
        y = Conv1D(256, 5, padding='same', kernel_initializer='he_uniform')(y)
        y = BatchNormalization()(y)
        y = Activation('relu')(y)
        y = Conv1D(128, 3, padding='same', kernel_initializer='he_uniform')(y)
        y = BatchNormalization()(y)
        y = Activation('relu')(y)
        y = GlobalAveragePooling1D()(y)
        x = concatenate([x, y])
        x = Dense(256, activation='relu',kernel_regularizer=regularizers.l2(0.01))(x)
        out = Dense(3, activation='softmax',kernel_regularizer=regularizers.l2(0.001))(x)
        #model = VariableLenModel(ip, out)
        model = VariableLenModel(ip,out)
        return model

    def load_model(self,weight_dir,prefix,suffix):
        """
        Function to load the model from the weights present in the given directory

        :param weight_dir: Path to the directory to load the weights from.
        :type weight_dir: str, 

        :param prefix: Prefix for the weight filenames
        :type prefix: str, 

        :param suffix: Suffix for the weight filenames
        :type suffix: str, 

        """
        
        for i in range(self.ensemble):
            if(os.path.exists("{}/{}{}{}".format(weight_dir,prefix,i,suffix))):
                self.model[i] = tf.keras.models.load_model("{}/{}{}{}".format(weight_dir,prefix,i,suffix))
    
    
if __name__ == '__main__':
    
    weight_dir = "./weights/Pretrained"
    dataset    = "W"
    prefix     = ""
    suffix     = ".h5"
    ensemble   = 25

    ewsnet     = EWSNet(ensemble=ensemble, weight_dir=os.path.join(weight_dir,"Dataset-{}".format(dataset)), prefix=prefix,suffix=suffix)
    x = np.random.randint(1,2,(20,))
    print(ewsnet.predict(x))
