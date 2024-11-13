library(tidyverse)
library(lme4)
library(lmerTest)

data <- data.frame(datasets::Orange)
data

data$Tree <- factor(data$Tree, levels = c("1", "2", "3", "4", "5"))

head(data)

# Just for exporting the dataset
# write_csv(data, "D:\\Downloads\\orange_tree_data.csv")

glimpse(data)

# Fitting fixed effects model
fmod <- lm(circumference ~ age, data = data)
fmod <- lm(circumference ~ age + Tree, data = data)
fmod <- lm(circumference ~ age*Tree, data = data)

# Checking the spread of residuals vs. fitted values
plot(fmod, 1)
plot(fmod, 2)
plot(x = fitted(fmod), residuals(fmod))

lmtest::bptest(fmod)

# Fitting mixed effects model with random intercepts for Tree
mmod_random_intercept <- lmer(circumference ~ age + (1 | Tree), REML = FALSE, data = data)

# Diagnostic plots
plot(x = fitted(mmod_random_intercept), residuals(mmod_random_intercept), main = "Residuals vs. Fitted Values", xlab = "Fitted", ylab = "Residuals"); abline(h = 0)
qqnorm(residuals(mmod_random_intercept)); qqline(residuals(mmod_random_intercept))
plot(mmod_random_intercept)

# Checking model comparisons
AIC(fmod, mmod_random_intercept)
BIC(fmod, mmod_random_intercept)
anova(fmod, mmod_random_intercept)



# Fitting mixed effects model with random intercepts and random slopes for Tree
mmod_random_int_slope <- lmer(circumference ~ age + (age | Tree), REML = FALSE, data = data)

# Checking AIC
AIC(fmod, mmod_random_intercept, mmod_random_int_slope)

# Adding the model predictions to the dataset so that they can be plotted
data$fmod_pred <- predict(fmod, newdata = data) # Fixed effects model
data$mmod_random_intercept_pred <- predict(mmod_random_intercept, newdata = data) # Mixed effects model with random intercepts
data$mmod_pred_random_int_slope <- predict(mmod_random_int_slope, newdata = data) # Mixed effects model with random intercepts and random slopes

# Visualizing the fixed effects model
data %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree), size = 2) +
  geom_line(aes(y = fmod_pred), linetype = "dashed", linewidth = 1) +
  ggtitle("Fixed effects model") +
  ylab("Circumference") +
  xlab("Tree Age")

data %>%
  ggplot(aes(x = age, y = circumference, group = Tree)) +
  geom_point(aes(colour = Tree), size = 2) +
  geom_line(aes(y = fmod_pred, colour = Tree), linetype = "dashed", linewidth = 1) +
  # ylim(c(0,NA)) +
  ggtitle("Fixed effects model") +
  ylab("Circumference") +
  xlab("Tree Age")

# Visualizing the mixed effects model with random intercepts (fixed slopes)
data %>%
  ggplot(aes(x = age, y = circumference, group = Tree)) +
  geom_point(aes(colour = Tree), size = 2) +
  geom_line(aes(y = mmod_random_intercept_pred, colour = Tree), linetype = "dashed", linewidth = 1) +
  # ylim(c(0,NA)) +
  ggtitle("Mixed effects model (random intercepts)") +
  ylab("Circumference") +
  xlab("Tree Age")

# Visualizing the mixed effects model with random intercepts and random slopes
data %>%
  ggplot(aes(x = age, y = circumference, group = Tree)) +
  geom_point(aes(colour = Tree), size = 2) +
  geom_line(aes(y = mmod_pred_random_int_slope, colour = Tree), , linetype = "dashed", linewidth = 1) +
  # ylim(c(0,NA)) +
  ggtitle("Mixed effects model (random intercepts and random slopes)") +
  ylab("Circumference") +
  xlab("Tree Age")

# Check out model summaries (will have to interpret these summaries in the presentation)

# Fixed effects model
summary(fmod)

# Mixed effects models
summary(mmod_random_intercept) # Mixed effects model with random intercepts
summary(mmod_random_intercept, ddf = "Kenward-Roger") # Mixed effects model with random intercepts; uses the Kenward-Roger approach to adjusting df
anova(mmod_random_intercept)
faraway::sumary(mmod_random_intercept)

summary(mmod_random_int_slope) # Mixed effects model with random intercepts and random slopes