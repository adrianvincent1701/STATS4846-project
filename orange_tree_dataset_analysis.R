library(tidyverse)
library(lmerTest)

data <- datasets::Orange
data

# Just for exporting the dataset
# write_csv(data, "D:\\Downloads\\orange_tree_data.csv")

glimpse(data)

# Fitting fixed effects model
fmod <- lm(circumference ~ age, data = data)

# Checking the spread of residuals vs. fitted values
# Looks like it gets larger
plot(fmod, 1)

# Fitting mixed effects model with random intercepts for Tree
mmod_random_intercept <- lmerTest::lmer(circumference ~ age + (1 | Tree), data = data)

# Fitting mixed effects model with random intercepts and random slopes for Tree
mmod_random_int_slope <- lmerTest::lmer(circumference ~ age + (age | Tree), data = data)

# Adding the model predictions to the dataset so that they can be plotted
data$fmod_pred <- predict(fmod, newdata = data) # Fixed effects model
data$mmod_random_intercept_pred <- predict(mmod_random_intercept, newdata = data) # Mixed effects model with random intercepts
data$mmod_pred_random_int_slope <- predict(mmod_random_int_slope, newdata = data) # Mixed effects model with random intercepts and random slopes

# Visualizing the fixed effects model
data %>%
  ggplot(aes(x = age, y = circumference)) +
  geom_point(aes(colour = Tree)) +
  geom_line(aes(y = fmod_pred), linetype = "dashed", size = 1) +
  ggtitle("Fixed effects model") +
  ylab("Circumference") +
  xlab("Tree Age")

# Visualizing the mixed effects model with random intercepts (fixed slopes)
data %>%
  ggplot(aes(x = age, y = circumference, group = Tree)) +
  geom_point(aes(colour = Tree)) +
  geom_line(aes(y = mmod_random_intercept_pred, colour = Tree), linetype = "dashed", size = 1) +
  ggtitle("Mixed effects model (random intercepts)") +
  ylab("Circumference") +
  xlab("Tree Age")

# Visualizing the mixed effects model with random intercepts and random slopes
data %>%
  ggplot(aes(x = age, y = circumference, group = Tree)) +
  geom_point(aes(colour = Tree)) +
  geom_line(aes(y = mmod_pred_random_int_slope, colour = Tree), , linetype = "dashed", size = 1) +
  ggtitle("Mixed effects model (random intercepts and random slopes)") +
  ylab("Circumference") +
  xlab("Tree Age")

# Check out model summaries (will have to interpret these summaries in the presentation)
summary(fmod) # Fixed effects model
summary(mmod_random_intercept) # Mixed effects model with random intercepts
summary(mmod_random_int_slope) # Mixed effects model with random intercepts and random slopes