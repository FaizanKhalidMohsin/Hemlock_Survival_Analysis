
library(tidyverse)
library(nlme)
library(car)
library(lsmeans)
library(MuMIn)
library(plotrix)
library(lattice)

setwd("C:/Users/bubbl/Desktop/Data Analysis 2019")
hemlock <- read.csv("Hemlock Data - Updated November 2018.csv", 
                 na.strings = c("N/A", "n/a", "#VALUE!", "NA"), 
                 header = TRUE)

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
hemlock.alive <- subset(hemlock, survival4 == "yes")

##### Setting variables to factor / numeric #######################################

# Set variables to factor and numeric
hemlock.alive$plot     <- factor(hemlock.alive$plot)
hemlock.alive$stem.L4 <- as.numeric(as.character(hemlock.alive$stem.L4))
hemlock.fence$plot     <- factor(hemlock.fence$plot)
hemlock.fence$stem.L4 <- as.numeric(as.character(hemlock.fence$stem.L4))
hemlock.open$plot     <- factor(hemlock.open$plot)
hemlock.open$stem.L4 <- as.numeric(as.character(hemlock.open$stem.L4))

# Set contrasts to "sum to zero" before making models
contrasts(hemlock.alive$fence)=contr.sum
contrasts(hemlock.alive$gm)=contr.sum
contrasts(hemlock.alive$plot)=contr.sum
contrasts(hemlock.alive$subplot)=contr.sum

hemlock.alive.fence <- subset(hemlock.alive, fence == "Fence")
hemlock.alive.open <- subset(hemlock.alive, fence == "Open")

#####

##### Stem length analysis - July 2017 to November 2018 ##############################################


model1 <- lme(stem.L4 ~ stem.L + ipc.gm * fence * gm, random = ~1|plot, data = hemlock.alive)


# Check residuals and outliers
plot(model1)
qqnorm(model1, abline = c(0,1))


# Check for sig. curve -- use poly(stem.L, 2) to compare
# model1b <- lme(stem.L4 ~ poly(stem.L, 2) + ipc.gm * fence * gm, data = hemlock.alive, random = ~1|plot)
# summary(model1b)
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

xyplot(stem.L4 ~ ipc.gm, data = hemlock.alive, type = c("p","r"), auto.key = TRUE, group = fence)
anova.lme(model1, type = "marginal")

# Let's look at the effect of ipc.gm in fenced and open plots

xyplot(stem.L4 ~ ipc.gm, data = hemlock.alive, type = c("p","r"), auto.key = TRUE, group = fence)

# This shows that stem.L4 increases with increasing ipc.gm, but really only in fence plots.
# This makes sense because the seedlings in open plots are already severly damaged by deer browsing.


#####

# Calculating mean size in fence/open treatments 

# July 2017
mean(hemlock.alive.fence$stem.L)
std.error(hemlock.alive.fence$stem.L)
mean(hemlock.alive.open$stem.L)
std.error(hemlock.alive.open$stem.L)

