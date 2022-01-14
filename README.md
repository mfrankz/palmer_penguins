# Data tidying, visualization, and analysis with the Palmer Penguins Dataset 
### This tutorial uses a model comparison approach to analyze a publicly available dataset in R. Tidyverse tools are used for data cleaning, and ggplot2 and the performance library are used for data visualization. 

To begin, we will load packages and import a phyloseq object. ***This is necessary for all the following steps in this tutorial.***

1. Load packages. Note: you will also need an up-to-date version of R studio and the phyloseq package (installed through BioConductor): 
```
#install BioConductor
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.13") 
#install phyloseq
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("phyloseq")
#load additional packages
library(phyloseq)
library(ggplot2)
library(dplyr)
```
