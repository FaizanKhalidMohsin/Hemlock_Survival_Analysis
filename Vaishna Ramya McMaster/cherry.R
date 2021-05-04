#library(tidyverse)
library(dplyr)
library(dplyr)
library(car)
library(pscl)
library(lme4)
library(car)
library(MuMIn)

#setwd("C:/Users/Vaishna Kumaran/iCloud Drive/Desktop/")

cherry <- read.csv("Cherry Data - Updated November 23,2019.csv", 
                    na.strings = c("N/A", "n/a", "#VALUE!", "NA"), 
                    header = TRUE)

# Set plot as factor
cherry$plot <- factor(cherry$plot)

# Set contrasts to "sum to zero" before making models
contrasts(cherry$fence)=contr.sum
contrasts(cherry$gm)=contr.sum
contrasts(cherry$plot)=contr.sum
contrasts(cherry$subplot)=contr.sum


contrasts(cherry$browse2)=contr.sum
contrasts(cherry$browse3)=contr.sum
contrasts(cherry$browse4)=contr.sum

# Data cleaning


# Create Fence and Open subsets
cherry.fence <- subset(cherry, fence == "Fence")
cherry.open <- subset(cherry, fence == "Open")
cherry.alive <- subset(cherry, survival4 == "yes")
cherry.alive$highest4 = as.numeric(cherry.alive$highest4)
table(cherry.alive$highest4)
table(cherry.alive$highest)
class(cherry.alive$highest4)
summary (cherry)


# Data exploration 
table(cherry$fence, cherry$browse4)
table(cherry$browse4, cherry$survival4)
table(cherry$fence, cherry$survival4)

cherry.open$browse4
cherry.open$browse3

#####
##### Analysis of browse - July 2017 to September 2017 ############################


# Determing which outcome variable to use. 
outcome.var = survival4 
outcome.var = highest4  

# Survival Model Use survival analysis or glmer logistic regression. 
#model0 <- glmer( survival4 ~ browse3 + ipc.gm * fence * gm + (1|plot),   data = cherry, family = binomial)      

model1 <- lmer( highest4 ~ highest + ipc.gm * fence * gm + (1|plot), data = cherry.alive ) 

Anova(model1, test = "Chisq", type = "III")
summary(model1)

# Singular fit
# No effects are significant

# Get open plot % browsed
cherry.alive %>% 
  group_by(fence) %>%
  summarise(no_rows = length(fence))

# 3 / 88
# 3.4% browsed


# Only 3 seedlings in open plots were browsed


# Get R2 (0.96)
r.squaredGLMM(model1)





##### Analysis of browse - July 2017 to April 2018 ############################


model2 <- glmer(browse3 ~ browse3 + ipc.gm + gm + (1|plot), data = cherry.open, family = binomial)


Anova(model2, test = "Chisq", type = "III")
summary(model2)

# Singular fit
# No effects are significant


# Get open plot % browsed
cherry.open %>% 
  group_by(browse3) %>%
  summarise(no_rows = length(browse3))

# 85 / 88
# 96.6% browsed

# 85 out of 88 seedlings in open plots were browsed.


# Get R2 (0.43)
r.squaredGLMM(model2)

plot(browse4~survival4, data=cherry)
summary(cherry$browse4)

##### Analysis of browse - July 2017 to November 2018 ############################


model3 <- glmer(browse4 ~ browse3 + ipc.gm + gm + (1|plot), data = cherry.open, family = binomial)

Anova(model3, test = "Chisq", type = "III")

summary(model3)

# No effects are significant

# Get  R2
r.squaredGLMM(model3)

# Get open plot % browsed
cherry.open %>% 
  group_by(browse4) %>%
  summarise(no_rows = length(browse4))

# 87 / 88
# 98.9% browsed

