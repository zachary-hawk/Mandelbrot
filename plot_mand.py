#!/usr/bin/env python3
import numpy as np
from scipy.io import FortranFile
import matplotlib.pyplot as plt
import time
from PIL import Image
import matplotlib.cm as cm
import re


start  = time.process_time()
params = open("out.mand","r")
params =  params.readlines()
n=re.findall(r'\d+',params[16])
n=int(n[0])
f = FortranFile("data.mand", "r")

data = f.read_reals(np.float32).reshape((n,n))
f.close()



plt.imsave("img.png",data,origin = "centre",cmap = "cubehelix")#,extent = (minx,maxx,miny,maxy))


