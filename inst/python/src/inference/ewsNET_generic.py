import os
import numpy as np
from numpy.random import seed
seed(1)
from tensorflow.random import set_seed
set_seed(2)
#from python.src.inference.ewsnet import EWSNet
from python.src.inference.ewsnet_pred_rand import EWSNet
#from python.src.inference.ewsnet_copy2 import EWSNet
