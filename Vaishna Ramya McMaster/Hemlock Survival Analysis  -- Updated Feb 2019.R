

library(tidyverse)
library(car)
library(pscl)
library(lme4)
library(lattice)
library(MuMIn)
library(emmeans)
library(jtools)

setwd("C:/Users/bubbl/Desktop/Data Analysis 2019")
hemlock <- read.csv("Hemlock Data - Updated November 2018.csv", 
                    na.strings = c("N/A", "n/a", "#VALUE!", "NA"), 
                    header = TRUE)

##### Exclude seedlings, create subsets ######################################################

# Recode survival to 1s and 0s
library(plyr)
hemlock$survival4 <- revalue(hemlock$survival4, c("yes"=1, "no"=0))
detach("package:plyr", unload=TRUE)
library(tidyverse)

# Set survival to numeric
hemlock$survival4 <- as.numeric(as.character(hemlock$survival4))

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

# Create Fence and Open subsets
hemlock.fence <- subset(hemlock, fence == "Fence")
hemlock.open <- subset(hemlock, fence == "Open")

#####
##### Analysis of survival - July 2017 to November 2018 ############################



# Get total % survival
hemlock %>% group_by(survival4) %>% summarise(no_rows = length(survival4))

# 112 / 176
# 63.63% survival


# Get % survival of open plot seedlings
hemlock.open %>% group_by(survival4) %>% summarise(no_rows = length(survival4))

# 30 / 88
# 34.1% survival

# Get % survival of fenced plot seedlings
hemlock.fence %>% group_by(survival4) %>% summarise(no_rows = length(survival4))

# 82 / 88
# 93.2%




# Generalized linear mixed model on survival

# All plots together

model1 <- glmer(survival4 ~ fence * gm + (1|plot), data = hemlock, family = binomial)

Anova(model1, test = "Chisq", type = "III")
summary(model1)


# Effect of FENCE is significant

lsmeans(model1, ~ fence, type = "response")

# Prob survival in fence = 94.0%
# Prob survival in open  = 32.9%


# Get McFadden R2 (0.47)
r.squaredGLMM(model1)



# Creating a bar graph for the effect of FENCE

df1 <- data.frame(lsmeans(model1, ~ fence, type = "response"))

p1 <- ggplot(df1, aes(x = fence, y = prob)) + 
  geom_col(width = 0.5) +
  scale_x_discrete(limits = c("Fence", "Open")) +  # reorder x axis
  theme_classic() +                                  # classic theme: white background, no grid lines, etc.
  geom_errorbar(aes(ymin=prob-SE, ymax=prob+SE), # SE bars
                width=.2,                            # width of error bars
                position=position_dodge(0.7)) +      # position of error bars
  labs(title = NULL,                                 # title
       x = " ",                                     # x axis label
       y = "Probability of Survival") +                     # y axis label
  scale_y_continuous(limits = c(0,1.0),   # Set y-axis limits
                     expand = c(0, 0)) +  # make y axis start at zero 
  guides(fill=FALSE)

p1





# Generalized linear model on survival in open plots -- including plot as a fixed effect.

# Open plots only

model2 <- glm(survival4 ~  stem.L + ipc.gm * gm + plot, data = hemlock.open, family = binomial)

Anova(model2, test = "Wald", type = "III")

# No effects are significant

summary(model2)

##
## Get r2, but which method?
##


# Number of survivors per plot pair (block)

# Block   Survivors
#     1           7
#     2          14
#     3           3
#     4           6

# With plot pair (block) as a random effect, ipc.gm is significant.
#
# Including plot as a fixed effect shows that the effect of ipc.gm is actually not significant.
















#######################################################################################
#######################################################################################


##### Analysis of survival - July 2017 to September 2017 ############################


# Get total % survival
hemlock %>% group_by(survival2) %>% summarise(no_rows = length(survival2))

# 170 / 176
# 96.6% survival

# Get open plot % survival
hemlock.open %>% group_by(survival2) %>% summarise(no_rows = length(survival2))

# 87 / 88
# 98.9% survival

# Get % survival of fenced plot seedlings
hemlock.fence %>% group_by(survival2) %>% summarise(no_rows = length(survival2))

# 83 / 88
# 94.3% survival



# All plots

model3 <- glmer(survival2 ~ fence * gm + (1|plot), data = hemlock, family = binomial)

# Warning: Hessian is numerically singular: parameters are not uniquely determined

Anova(model3, test = "Chisq", type = "III")
summary(model3)


# No effects are significant

# Get  R2
r.squaredGLMM(model3)





# Open plots only

model4 <- glm(survival2 ~  stem.L + ipc.gm * gm + plot, data = hemlock.open, family = binomial)

# glm.fit: fitted probabilities numerically 0 or 1 occurred

Anova(model4, test = "Wald", type = "III")

# No effects are significant

summary(model4)









##### Analysis of survival - July 2017 to April 2018 ############################




# Get total % survival
hemlock %>% 
  group_by(survival3) %>%
  summarise(no_rows = length(survival3))

# 167 / 176
# 94.9% survival

# Get open plot % survival
hemlock.open %>% 
  group_by(survival3) %>%
  summarise(no_rows = length(survival3))

# 84 / 88
# 95.5% survival

# Get fenced plot % survival
hemlock.fence %>% 
  group_by(survival3) %>%
  summarise(no_rows = length(survival3))

# 83 / 88
# 94.3% survival





# All plots

model5 <- glmer(survival3 ~ fence * gm + (1|plot), data = hemlock, family = binomial)

# Singular fit

Anova(model5, test = "Chisq", type = "III")
summary(model5)

# No effects are significant

# Get  R2 (0.01)
r.squaredGLMM(model5)





# Open plots only

model6 <- glm(survival3 ~  stem.L + ipc.gm * gm + plot, data = hemlock.open, family = binomial)


Anova(model6, test = "Wald", type = "III")

# No effects are significant

summary(model6)

# Get  R2 (0.93)
r.squaredGLMM(model6)














