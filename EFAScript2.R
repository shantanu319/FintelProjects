#install packages needed for EFA, only needs to be done once

#install.packages("psych")  #PCA/EFA amongst many other things!
#install.packages("REdaS")  ##produces KMO and Bartletts test
#install.packages("readxl")
#install.packages("readr")  #reads github
#install.packages('GPArotation')
#install.packages('ggplot2')

#pull packages out of the library
library(psych)
library(readxl)
library(REdaS)
library(readr)
library(ggplot2)

#read in the data set/access csv file
url <- "https://raw.githubusercontent.com/housecricket/data/main/efa/sample1.csv"
ATGC <- read.csv(url, sep = ",")
#ATGC <- read_excel("C:/Users/Shantanu Khaladkar/Downloads/ATGC_expfa.xlsx")
View(ATGC)        
attach(ATGC)

#KMO and sphericity test for factorability
bart_spher(ATGC) # produces Bartletts test of spherecity (you want this to be significant)
KMO(ATGC)       # Kaiser-Meyer-Olkin measure, you want to be above .6

#building the scree plot


#using Kaisers rule, Eigenvalues>1 represent valid factors
###set nfactors to n items, in this case there is 12 items so we state nfactors=12
#####oblimin is selected as the rotation although this is default for factor analysis (variamx is default for pca)
##orthagonal roatations availible ="none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" "bifactor" 
##oblique roatations availible "Promax", "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" "biquartimin" "cluster" 

fa(ATGC, nfactors = 4, rotate =  "oblimin" )  


#fa(ATGC, nfactors = 3, rotate =  "oblimin" )

###################you can produce a figure 

M1<-fa(ATGC, nfactors = 4, rotate =  "oblimin" ) ##save the analysis as the object m1
fa.diagram(M1,main="ATGC")                      ## produce a figure with the title "" note fa.diagram still works for PCA























