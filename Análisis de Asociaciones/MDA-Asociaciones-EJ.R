
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ilustración Reglas de Asociación

Juan Esteban Mejía Velásquez

'''

if(!require(arules, quietly=TRUE))install.packages("arules")
if(!require(arulesViz, quietly=TRUE))install.packages("arulesViz")
if(!require(RColorBrewer, quietly=TRUE))install.packages("RColorBrewer")
if(!require(scatterplot3d, quietly=TRUE))install.packages("scatterplot3d")


library(arules)  # association rules
library(arulesViz)  # data visualization of association rules
library(RColorBrewer)  # color palettes for plots
library(cluster)  # cluster analysis for market segmentation

data(Groceries)  # grocery transcations object from arules package

# show the dimensions of the transactions object

print(dim(Groceries))

print(dim(Groceries)[1])  # 9835 market baskets for shopping trips
print(dim(Groceries)[2])  # 169 initial store items  