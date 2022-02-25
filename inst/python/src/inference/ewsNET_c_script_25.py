
import os
import numpy as np
from inst.python.src.inference.ewsnet import EWSNet

dataset = "C"
ensemble25 = 25
weight_dir = ".inst/python/weights/Pretrained"
weight_dirW = os.path.join(weight_dir,"Dataset-{}".format(dataset))

ewsnetC_25 = EWSNet(ensemble=ensemble25, weight_dir=weight_dir)
