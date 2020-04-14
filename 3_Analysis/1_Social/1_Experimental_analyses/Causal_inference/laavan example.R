library(lavaan)
library(qgraph)
library(semPlot)

data(HolzingerSwineford1939)
HS.model <-   'visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed =~ x7 + x8 + x9 '
fit <- cfa(HS.model, data = HolzingerSwineford1939)
summary(fit, fit.measures = TRUE)


data(PoliticalDemocracy)
?PoliticalDemocracy
head(PoliticalDemocracy)
model <- 
'# measurement model
ind60 =~ x1 + x2 + x3  (indicators that attempt to measure ind)
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5     (these variables are somehow correlated with each other e.g.)
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8'

fit <- sem(model, data = PoliticalDemocracy)
summary(fit, standardized = TRUE, rsq=T)

#Plot it 
qgraph(fit,edge.label.cex=1.5,label.cex=1.5)
semPaths(fit, "std",edge.label.cex=1, curvePivot=T, layout ="tree")

# did we miss any obvious relationships? shows you potential correlations that you might hve missed
modindices(fit) # gives all possible paths given the data. Look for extremely large variables 



#fit your SEM
fit <- sem(model, data = PD)

#summarize results
summary(fit, standardized = TRUE, rsq = T)

##plot results using semPaths function in qgraph
semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")

##check to see if you missed anything. High mi values suggest that there is a path that you missed.

modindices(fit)

## looks good

##can also look at variance tables  
vartable(fit)


## sometimes you get warnings about the scale of your variables
#Warning message:
# In getDataFull(data = data, group = group, group.label = group.label, :
#                 lavaan WARNING: some observed variances are (at least) a factor 100 times larger than others; please rescale

# in this case, all you have to do to make this error go away is rescale variables

#model comparison

#you can compare alternative pathway models using AIC, BIC, etc:

#create second alternative model
names(PD)

model2 <- '
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ dem60
#took out ind60 from regression
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit2 <- sem(model2, data = PD)
summary(fit2)

AIC(fit, fit2)

## what about nonlinear data?

# Set your working directory
("~/Desktop/sem workshop")
#commands in bold

# Load data and name file ?k.dat?
k.dat<-read.csv("./Keeley_rawdata_select4.csv")

# Examine contents of keeley data file
names(k.dat)
head(k.dat)


# Write lavaan code for this single equation model
mod <- '
rich ~ cover
cover ~ firesev
'

k.dat$cov2<-k.dat$cover^2

mod2<- '
rich ~ cover + cov2
cover ~ firesev
cover ~~ cov2
cov2 ~~ firesev
'



# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod, data=k.dat)
mod2.fit<- sem(mod2, data=k.dat,fixed.x=FALSE)

#need to rescale data.
vartable(mod1.fit)
k.dat$rich<-k.dat$rich/100

# Output a summary of the computed results - summary of mod2 suggests that both cover and cover squared can impact 
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr
summary(mod2.fit, rsq=T)


semPaths(mod1.fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")
semPaths(mod2.fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")




