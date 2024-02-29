# you may have to download C++ and cmake to get this to work
install.packages(c("dplyr", "ggplot2"))
if (!requireNamespace("lme4", quietly = TRUE)) install.packages("lme4")
install.packages("nlme", dependencies = TRUE)
install.packages("lmerTest")


## Load Libraries

library(lme4)
library(lmerTest)
library(nlme)
library(dplyr)
library(ggplot2)


##################################################
#### Example 1: Penicillin Data Hierarchical model
##################################################

data(Penicillin)
View(Penicillin)

#### Data Exploration
str(Penicillin)

ggplot(data = Penicillin, aes(sample = diameter)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot of Data against Normal Distribution")

# Fit a mixed-effects model

#########
# Model 1
#########

model1 <- lm(diameter ~ sample, data=Penicillin)
summary(model1)
AIC(model1)

df_residuals <- data.frame(
  Fitted = fitted(model1),
  Residuals = resid(model1)
)

# Plotting residuals vs fitted values using ggplot2
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at 0
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Add a LOESS smoothed line
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(legend.position = "none")  # Remove legend for color

#########
# Model 2
#########

model2 <- lme(diameter ~ 1, data=Penicillin, random=~1|plate)
summary(model2)

df_residuals <- data.frame(
  Fitted = fitted(model2),
  Residuals = resid(model2)
)

# Plotting residuals vs fitted values using ggplot2
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at 0
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Add a LOESS smoothed line
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(legend.position = "none")  # Remove legend for color

#########
# Model 3
#########

model3 <- lme(diameter ~ sample, data=Penicillin, random=~1|plate)
summary(model3)

df_residuals <- data.frame(
  Fitted = fitted(model3),
  Residuals = resid(model3)
)

# Plotting residuals vs fitted values using ggplot2
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at 0
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Add a LOESS smoothed line
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(legend.position = "none")  # Remove legend for color

model4 <- lme(diameter ~ sample, data=Penicillin, random=~1|plate/sample)
summary(model4)

df_residuals <- data.frame(
  Fitted = fitted(model4),
  Residuals = resid(model4)
)

# Plotting residuals vs fitted values using ggplot2
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at 0
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Add a LOESS smoothed line
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(legend.position = "none")  # Remove legend for color

##################################################
#### Example 2: Continue the school Example ######
##################################################



##### View Our Data

View(school_df)

ggplot(data = school_df, aes(sample = test_scores)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot of Data against Normal Distribution")

#### Build and evaluate an lm
# Model 1
linear_model_school <- lm(test_scores ~ intervention  + school + time + individual_id, data=school_df)
b <- summary(linear_model_school)
b
anova(linear_model_school)
AIC(linear_model_school)
BIC(linear_model_school)
b$r.squared

###
df_residuals <- data.frame(
  Fitted = fitted(linear_model_school),
  Residuals = resid(linear_model_school)
)

# Plotting residuals vs fitted values using ggplot2
ggplot(df_residuals, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at 0
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Add a LOESS smoothed line
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(legend.position = "none")  # Remove legend for color


# Model 2
lmm_school <- lmer(test_scores ~ intervention  + (1|school)+ (1 | school:individual_id), data = school_df) #
lmm_sum <- summary(lmm_school)
lmm_sum
anova(lmm_school)
AIC(lmm_school)
BIC(lmm_school)
lmm_sum$AICtab

###
df_residuals_lmm <- data.frame(
  Fitted = fitted(lmm_school),
  Residuals = resid(lmm_school)
)

# Plotting residuals vs fitted values using ggplot2
ggplot(df_residuals_lmm, aes(x = Fitted, y = Residuals)) +
  geom_point(aes(color = Residuals), alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Horizontal line at 0
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Add a LOESS smoothed line
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal() +
  scale_color_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme(legend.position = "none")  # Remove legend for color


######################################
## One more example: Longitudinal Data
######################################

# Load the sleepstudy dataset
data("sleepstudy", package = "lme4")

ggplot(data = sleepstudy, aes(sample = Reaction)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot of Data against Normal Distribution")


model1 <- lmer(Reaction ~ (1 | Subject), data = sleepstudy)

summary(model1)

model2 <- lmer(Reaction ~ Days+ (1 | Subject), data = sleepstudy)

summary(model2)

anova(model1, model2)

