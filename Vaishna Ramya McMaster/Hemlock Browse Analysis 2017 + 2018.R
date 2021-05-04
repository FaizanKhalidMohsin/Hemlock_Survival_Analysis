

library(tidyverse)
library(car)
library(pscl)
library(lme4)
library(car)
library(MuMIn)

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

# Set plot as factor
hemlock$plot <- factor(hemlock$plot)

# Set contrasts to "sum to zero" before making models
contrasts(hemlock$fence)=contr.sum
contrasts(hemlock$gm)=contr.sum
contrasts(hemlock$plot)=contr.sum
contrasts(hemlock$subplot)=contr.sum


contrasts(hemlock$browse2)=contr.sum
contrasts(hemlock$browse3)=contr.sum
contrasts(hemlock$browse4)=contr.sum

# Create Fence and Open subsets
hemlock.fence <- subset(hemlock, fence == "Fence")
hemlock.open <- subset(hemlock, fence == "Open")

#####
##### Analysis of browse - July 2017 to September 2017 ############################


model1 <- glmer(browse2 ~ stem.L + ipc.gm + gm + (1|plot), data = hemlock.open, family = binomial)

Anova(model1, test = "Chisq", type = "III")
summary(model1)

# Singular fit
# No effects are significant

# Get open plot % browsed
hemlock.open %>% 
  group_by(browse2) %>%
  summarise(no_rows = length(browse2))

# 3 / 88
# 3.4% browsed


# Only 3 seedlings in open plots were browsed


# Get R2 (0.96)
r.squaredGLMM(model1)





##### Analysis of browse - July 2017 to April 2018 ############################


model2 <- glmer(browse3 ~ stem.L + ipc.gm + gm + (1|plot), data = hemlock.open, family = binomial)


Anova(model2, test = "Chisq", type = "III")
summary(model2)

# Singular fit
# No effects are significant


# Get open plot % browsed
hemlock.open %>% 
  group_by(browse3) %>%
  summarise(no_rows = length(browse3))

# 85 / 88
# 96.6% browsed

# 85 out of 88 seedlings in open plots were browsed.


# Get R2 (0.43)
r.squaredGLMM(model2)




##### Analysis of browse - July 2017 to November 2018 ############################


model3 <- glmer(browse4 ~ stem.L + ipc.gm + gm + (1|plot), data = hemlock.open, family = binomial)

Anova(model3, test = "Chisq", type = "III")

summary(model3)

# No effects are significant

# Get  R2
r.squaredGLMM(model3)

# Get open plot % browsed
hemlock.open %>% 
  group_by(browse4) %>%
  summarise(no_rows = length(browse4))

# 87 / 88
# 98.9% browsed















