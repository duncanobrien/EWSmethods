
import os
import numpy as np
from src.inference.ewsnet import EWSNet

dataset = "W"
ensemble25 = 25
weight_dir = "./weights/Pretrained"
weight_dirW = os.path.join(weight_dir,"Dataset-{}".format(dataset))

ewsnetW_25 = EWSNet(ensemble=ensemble25, weight_dir=weight_dir)
