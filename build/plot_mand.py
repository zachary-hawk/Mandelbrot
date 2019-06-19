#!/usr/bin/env python3
import numpy as np
from scipy.io import FortranFile
import matplotlib.pyplot as plt
import sys

def print_help():


    print("Mandelbrot image generator, Author: Z.Hawkhead")
    print()
    print("Change image colourmap \nTakes matplotlib colormap string, or intger in range 0-79")
    print()
    
cmaps = ['viridis', 'plasma', 'inferno', 'magma',
  
            'Greys', 'Purples', 'Blues', 'Greens', 'Oranges', 'Reds',
            'YlOrBr', 'YlOrRd', 'OrRd', 'PuRd', 'RdPu', 'BuPu',
            'GnBu', 'PuBu', 'YlGnBu', 'PuBuGn', 'BuGn', 'YlGn',
  
            'binary', 'gist_yarg', 'gist_gray', 'gray', 'bone', 'pink',
            'spring', 'summer', 'autumn', 'winter', 'cool', 'Wistia',
            'hot', 'afmhot', 'gist_heat', 'copper',
  
            'PiYG', 'PRGn', 'BrBG', 'PuOr', 'RdGy', 'RdBu',
            'RdYlBu', 'RdYlGn', 'Spectral', 'coolwarm', 'bwr', 'seismic',
  
            'Pastel1', 'Pastel2', 'Paired', 'Accent',
            'Dark2', 'Set1', 'Set2', 'Set3',
            'tab10', 'tab20', 'tab20b', 'tab20c',
  
            'flag', 'prism', 'ocean', 'gist_earth', 'terrain', 'gist_stern',
            'gnuplot', 'gnuplot2', 'CMRmap', 'cubehelix', 'brg', 'hsv',
            'gist_rainbow', 'rainbow', 'jet', 'nipy_spectral', 'gist_ncar']


if len(sys.argv)>1:
    
    c=sys.argv[1]

    if c=="-h":
        print_help()
        exit()


    if len(c)>2:
        i=cmaps.index(c)
        
        color=cmaps[i]
    else:
        i=np.int(c)

        color=cmaps[i]
else:
    color="cubehelix"

print("Cmap used: ",color,"\nindex:",cmaps.index(color))

f = FortranFile("data.mand", "r")

data = f.read_reals(np.float32)#.reshape((n,n))
n=int(np.sqrt(len(data)))
data=data.reshape((n,n))


#print(data)
f.close()

plt.imsave("img.png",data,origin = "centre",cmap = color)#,extent = (minx,maxx,miny,maxy))


#from PIL import Image

#img = Image.open('img.png')
#img = img.convert("RGBA")
#datas = img.getdata()

#newData = []
#for item in datas:
#    if item[0] == 0 and item[1] == 0 and item[2] == 0:
#        newData.append((255, 255, 255, 0))
#    else:
#        if item[0] > 150:
#            newData.append((0, 0, 0, 255))
#        else:
#            newData.append(item)
#            #print(item)3#3#


#img.putdata(newData)
#img.save("img_transparent.png", "PNG")
