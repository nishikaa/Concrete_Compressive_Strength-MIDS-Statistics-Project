
install.packages("bookdown")
install.packages("dcolumn")

library(ggplot2)
library(ggcorrplot)
library(GGally)
library(olsrr)
library(tidyverse)
library(stargazer) 
library(sandwich)
library(magrittr)
#library(car)
library(lmtest)
library(MASS)
#library(caTools)

library(readxl)



Concrete_Data<-read_excel("data/external/Concrete_Data.xls")

# Renamecolumns 
names(Concrete_Data) <- c("Cement","Slag","Ash","Water","Plasticizer","Coarse","Fine","Age","Strength")
head(Concrete_Data)

# Create water:cement ratio variable
dat <- Concrete_Data %>%
  mutate(wc_ratio = Water / Cement) %>%
  filter(Age >= 28)

ggcorr(`dat`, method = c("everything", "pearson")) 
cor(dat$Plasticizer,dat$Age)
cor(dat$Plasticizer,dat$Ash)
cor(dat$Strength,dat$Age)
cor(dat$Strength,dat$Ash)
cor(dat$Strength,dat$Slag)

# split data into exploration vs confirmation
sample_size <- floor(0.3 * nrow(dat))
set.seed(123)
train_ind <- sample(seq_len(nrow(dat)), size=sample_size)
train <- dat[train_ind, ]
test <- dat[-train_ind, ]

train
test

# target model
model_one <- lm(Strength ~ wc_ratio + Plasticizer, data=train)
coeftest(model_one,vcov=vcovHC(model_one))
# Check assumptions of LM model
# No issues with assumptions
par(mfrow=c(2,2))
plot(model_one)
bptest(model_one)

# Use best-subset regression to identify other variables
model <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Age + Ash + Coarse + Fine, 
            data = train)
ols_step_best_subset(model,details=TRUE)

# Explore other model specifications
model_zero <- lm(Strength ~ wc_ratio, data=train)
model_two <- lm(Strength ~ wc_ratio + Plasticizer + Slag, data=train)
model_three <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Age, data=train)
model_four <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Ash + Age, data=train)
model_five <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Age + Ash + Coarse + Fine, data=train)
stargazer(model_one,model_two,model_three,model_four,model_five,type="text")

# Use test data to generate star-gazer
model_1 <- lm(Strength ~ wc_ratio + Plasticizer, data=test)
model_2 <- lm(Strength ~ wc_ratio + Plasticizer + Slag, data=test)
model_3 <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Age, data=test)
model_4 <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Ash + Age, data=test)
model_5 <- lm(Strength ~ wc_ratio + Plasticizer + Slag + Age + Ash + Coarse + Fine, data=test)
stargazer(model_1,model_2,model_3,model_4,model_5,type="text")

# Compute effect size
sd_plasticizer = sd(test$Plasticizer)
raw_effect = sd_plasticizer * 0.546
raw_effect
sd_strength = sd(test$Strength)
sd_strength
raw_effect / sd_strength
0.282 * 32.2/sd_strength

range(dat$Plasticizer)

model_1_scaled <- lm(scale(Strength) ~ scale(wc_ratio) + scale(Plasticizer), data=test)
model_5_scaled <- lm(scale(Strength) ~ scale(wc_ratio) + scale(Plasticizer) + scale(Slag) + scale(Age) + scale(Ash) + scale(Coarse) + scale(Fine), data=test)
summary(model_5_scaled)
