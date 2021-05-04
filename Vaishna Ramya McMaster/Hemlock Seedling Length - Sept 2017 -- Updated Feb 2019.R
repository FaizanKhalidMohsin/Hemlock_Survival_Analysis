

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

# Exclude 6 seedlings that died by September 2017
hemlock.alive <- subset(hemlock, survival2 == "yes")

##### Setting variables to factor / numeric #######################################

# Set variables to factor and numeric
hemlock.alive$plot     <- factor(hemlock.alive$plot)
hemlock.alive$stem.L2 <- as.numeric(as.character(hemlock.alive$stem.L2))
hemlock.fence$plot     <- factor(hemlock.fence$plot)
hemlock.fence$stem.L2 <- as.numeric(as.character(hemlock.fence$stem.L2))
hemlock.open$plot     <- factor(hemlock.open$plot)
hemlock.open$stem.L2 <- as.numeric(as.character(hemlock.open$stem.L2))

# Set contrasts to "sum to zero" before making models
contrasts(hemlock.alive$fence)=contr.sum
contrasts(hemlock.alive$gm)=contr.sum
contrasts(hemlock.alive$plot)=contr.sum
contrasts(hemlock.alive$subplot)=contr.sum

hemlock.alive.fence <- subset(hemlock.alive, fence == "Fence")
hemlock.alive.open <- subset(hemlock.alive, fence == "Open")

#####

##### Stem length analysis - July 2017 to September 2017 ##############################################


model1 <- lme(stem.L2 ~ stem.L + ipc.gm * fence * gm, data = hemlock.alive, random = ~1|plot)


# Check residuals and outliers
plot(model1)
qqnorm(model1, abline = c(0,1))


# Boxplot with whiskers at default 1.5 * Interquartile Range
# Identifies moderate outliers on the residuals of a model
boxplot(residuals(model1))

# Boxplot with whiskers at 3 * IQR for extreme outliers
boxplot(residuals(model1),range=3)

# Get the outliers and print them
whosout<-boxplot(residuals(model1),range=3)
whosout$out

# Seedling numbers 208 and 517
# One gre 10 cm, the other shrank 10 cm, while all other seedlings were pretty much the same



# Exclude seedlings

hemlock.alive.stem.L <- subset(hemlock.alive, seedling.number != "208" &
                                 seedling.number != "517")




model1 <- lme(stem.L2 ~  stem.L + ipc.gm * fence * gm, random = ~1|plot, data = hemlock.alive.stem.L)


# Check residuals and outliers
plot(model1)
qqnorm(model1, abline = c(0,1))


summary(model1)
anova.lme(model1, type = "marginal")
r.squaredGLMM(model1)

lsmeans(model1, ~ fence)

# Effect of IPC.GM is significant


# Let's see what the effect of IPC.GM looks like


xyplot((stem.L2-stem.L) ~ ipc.gm, data = hemlock.alive.stem.L, type = c("p","r"), auto.key = TRUE,
       xlab = "June % GM Cover",
       ylab = "Sep '17 SL - Jul '17 SL (cm)")




# Stem length growth increased with increasing gm



#####

# Calculating mean size in fence/open treatments 
hemlock.alive.stem.L.fence <- subset(hemlock.alive.stem.L, fence =="Fence")
hemlock.alive.stem.L.open <- subset(hemlock.alive.stem.L, fence =="Open")


# July 2017
mean(hemlock.alive.stem.L.fence$stem.L)
std.error(hemlock.alive.stem.L.fence$stem.L)
mean(hemlock.alive.stem.L.open$stem.L)
std.error(hemlock.alive.stem.L.open$stem.L)



