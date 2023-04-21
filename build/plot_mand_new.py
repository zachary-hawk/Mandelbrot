#!/usr/bin/env python3
import numpy as np
from scipy.io import FortranFile
import matplotlib.pyplot as plt
import sys

import argparse

parser = argparse.ArgumentParser(description= "Plotting nice images.")
parser.add_argument("name",help="The final image name")
parser.add_argument("-c",help="The plotting color map")
parser.add_argument('-r',help='rotate',action='store_true')
parser.add_argument('--dpi',help='DPI',default=300,type=int)
parser.add_argument('-ty',help='trim y',default = 0,type=float)

args = parser.parse_args()
name=args.name
c=args.c
rotate=args.r
dpi=args.dpi
sy=args.ty






f = FortranFile("data.mand", "r",'>u4')
ny=int(f.read_ints('>f8')[0])
n=int(f.read_ints('>f8')[0])
print(n,ny)
data = f.read_reals('>f8')#.reshape((n,n))
#n=int(np.sqrt(len(data)))
data=data.reshape((n,ny))

#print(data)
data[data < -1e308] = 0

f.close()

# trim y
trim_n = int(ny*sy)
print(trim_n)

data = data[:,int(trim_n/2):int(ny-trim_n/2)]



if rotate:
    data=np.transpose(data)
    fix, ax = plt.subplots(figsize=(16.7*ny/n,16.7))
else:
    fix, ax = plt.subplots(figsize=(16.7*ny/n,16.7))






ax.imshow(data,origin='lower', cmap=c,aspect='auto')
#plt.imshow(data,origin = "centre",cmap = color)
#plt.imsave("img.png",data,origin = "centre",cmap = color)#,extent = (minx,maxx,miny,maxy))

ax.set_axis_off()
#if sy==0:
#    ax.set_aspect('equal')
plt.tight_layout()
plt.savefig(name, dpi=dpi,pad_inches=0,bbox_inches='tight')

#plt.show()
#print("test")
