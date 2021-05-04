library(tidyverse)
library(nlme)
library(car)
library(lsmeans)
library(MuMIn)
library(plotrix)
library(lattice)
library(readxl)
library(dplyr)

path = getwd()

#setwd("C:/Users/bubbl/Desktop/Data Analysis 2019")
hemlock <- read.csv(paste0(path, "/" ,"Hemlock Data - R 1-6.csv"), 
                      na.strings = c("N/A", "n/a", "#VALUE!", "NA", "missing"), 
                      header = TRUE)

hemlock = select(hemlock, -X)

##### Exclude seedlings, create subsets ######################################################

# Exclude 8 vandalized (dug up) hemlocks
hemlock <- subset(hemlock, stem.L != "NA")

# Exclude 8 seedlings that were browsed when 4-Fence was breached in March 2018
hemlock <- subset(hemlock, seedling.number != "433" &
                  seedling.number != "438" &
                  seedling.number != "442" &
                  seedling.number != "445" &
                  seedling.number != "446" &
                  seedling.number != "469" &
                  seedling.number != "493" &
                  seedling.number != "499")

# Create Fence and Open subsets
hemlock.fence <- subset(hemlock, fence == "Fence")
hemlock.open <- subset(hemlock, fence == "Open")

# Exclude 64 seedlings that died by November 2018

hemlock$V.survival6 = tolower(hemlock$V.survival6) # Change Yes to lower case yes
hemlock.alive <- subset(hemlock, V.survival6 == "yes")

##### Setting variables to factor / numeric #######################################

# Set variables to factor and numeric
hemlock.alive$plot     <- factor(hemlock.alive$plot)
hemlock.alive$V.stem.L6 <- as.numeric(as.character(hemlock.alive$V.stem.L6))
hemlock.fence$plot     <- factor(hemlock.fence$plot)
hemlock.fence$V.stem.L6 <- as.numeric(as.character(hemlock.fence$V.stem.L6))
hemlock.open$plot     <- factor(hemlock.open$plot)
hemlock.open$V.stem.L6 <- as.numeric(as.character(hemlock.open$V.stem.L6))

# Set contrasts to "sum to zero" before making models
contrasts(hemlock.alive$fence)=contr.sum
contrasts(hemlock.alive$gm)=contr.sum
contrasts(hemlock.alive$plot)=contr.sum
contrasts(hemlock.alive$subplot)=contr.sum

hemlock.alive.fence <- subset(hemlock.alive, fence == "Fence")
hemlock.alive.open <- subset(hemlock.alive, fence == "Open")

#####

##### Stem length analysis - July 2017 to November 2018 ##############################################
hemlock.alive$V.stem.L6

model1 <- lme(V.stem.L6 ~ stem.L + ipc.gm * fence * gm, random = ~1|plot, data = hemlock.alive)
summary(model1)

# Check residuals and outliers, checking assumptions. 
# 1. Assumption of Normality (of errors).
# 2. Assumption of Constant Variance.
# 3. Assumption of observations are independant.

plot(model1)
qqnorm(model1, abline = c(0,1))


# Check for sig. curve -- use poly(stem.L, 2) to compare
model1b <- lme(V.stem.L6 ~ poly(stem.L, 2) + ipc.gm * fence * gm, data = hemlock.alive, random = ~1|plot)
summary(model1b)
# Curve is not significant


# Test for outliers

# Boxplot with whiskers at default 1.5 * Interquartile Range
# Identifies moderate outliers on the residuals of a model
boxplot(residuals(model1))

# Boxplot with whiskers at 3 * IQR for extreme outliers
Boxplot(residuals(model1),range=3, id=T)


# Get the outliers and print them
whosout<-boxplot(residuals(model1),range=3)
whosout$out

# No extreme outliers




summary(model1)
anova.lme(model1, type = "marginal")
r.squaredGLMM(model1)



# Effect of FENCE is significant

# Calculate the least-squares means
lsmeans(model1, ~ fence)



# Let's see what the effect of ipc.gm looks like

xyplot(V.stem.L6 ~ ipc.gm, data = hemlock.alive, type = c("p","r"), auto.key = TRUE, group = fence)
anova.lme(model1, type = "marginal")

# Let's look at the effect of ipc.gm in fenced and open plots

xyplot(V.stem.L6 ~ ipc.gm, data = hemlock.alive, type = c("p","r"), auto.key = TRUE, group = fence)

# This shows that V.stem.L6 increases with increasing ipc.gm, but really only in fence plots.
# This makes sense because the seedlings in open plots are already severly damaged by deer browsing.


##### Descriptive Statistics.

# Calculating mean size in fence/open treatments 

# July 2017
mean(hemlock.alive.fence$stem.L)
std.error(hemlock.alive.fence$stem.L)
mean(hemlock.alive.open$stem.L)
std.error(hemlock.alive.open$stem.L)


histogram(hemlock.alive.open$stem.L)
histogram(hemlock.alive.fence$stem.L)
