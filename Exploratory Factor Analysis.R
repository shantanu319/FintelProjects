#packages needed for EFA
install.packages(c('psych', 'corrplot', '"psych"', 'ggplot2', 'car'))
library(psych)
library(corrplot)
library("psych")
library(ggplot2)
library(car)
library(diagram)

#Accessing csv file
url <- "https://raw.githubusercontent.com/housecricket/data/main/efa/sample1.csv"
data_survey <- read.csv(url, sep = ",")
#For csv files, just set the file name inside read.csv()

#statistical description
describe(data_survey)
dim(data_survey)
dat <- data_survey[ , -1] 
head(dat)

#creating correlation matrix; c(-13) represents the number of columns that need to be made (13 columns)
datamatrix <- cor(dat[,c(-13)])
corrplot(datamatrix, method="number")
X <- dat[,-c(13)]
Y <- dat[,13]

#KMO and sphericity test
KMO(r=cor(X))
cortest.bartlett(X)
det(cor(X))

#Scree Plot to find the number of factors
fafitfree <- fa(dat,nfactors = ncol(X), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

#Parallel Analysis
parallel <- fa.parallel(X)
#Parallel analysis suggests that the number of factors =  4 and the number of components =  3

#The first 4 factors have an Eigenvalue >1 and which explains almost 69% of the variance. 
#We can effectively reduce dimensionality from 11 to 4 while only losing about 31% of the variance.
#Factor 1 accounts for 29.20% of the variance; Factor 2 accounts for 20.20% of the variance; 
#Factor 3 accounts for 13.60% of the variance; Factor 4 accounts for 6% of the variance. 
#All the 4 factors together explain for 69% of the variance in performance.
#varimax rotation assumes that the external factors are unrelated to each otheror show no real correlation (ie earnings and jobs reports)

#Performing Factor Analysis can be done with factanal() or fa() this procedure also returns a rotate component matrix of standardized loadings which tells
#us which variables are mostly explained by which of the latent factors
fa.none <- fa(r = X, nfactors = 4, rotate = "varimax", max.iter = 100, fm = "pa")
print(fa.none)

fa.diagram(fa.none)

#using factor loading to figure out correlations between latent factors (PA1, PA2, PA3, PA4) and given factors KM, CT, PC, QC 1-3
fa.none$loadings
fa.diagram(fa.none)

#cleaning data for linear regression of EFA results
#head(fa.none$scores)
#fa.none$loadings
#fa.diagram(fa.none)
#regdata <- cbind(“QD”), fa.none$scores)
#names(regdata) <- c(“QD”, “F1”, “F2”, “F3”, “F4”)
#head(regdata)