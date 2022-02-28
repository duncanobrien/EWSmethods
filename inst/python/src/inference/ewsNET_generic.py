
import os
import numpy as np
from python.src.inference.ewsnet import EWSNet

dataset = noise_type
ensemble_fin = ensemble
weight_dir = ".python/weights/Pretrained"
weight_dir_fin = os.path.join(weight_dir,"Dataset-{}".format(dataset))

ewsnet_obj = EWSNet(ensemble=ensemble_fin, weight_dir=weight_dir_fin)
