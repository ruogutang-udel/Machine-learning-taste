x <- 5
y <- 3
x + y
x
y
x
y
x
y
x <- 5
y <- 3
x+y
x <- 5
y <- 3
x + y
x <- 6
y <- 2
x-y
x*y
x/y
x^2
sqrt(6)
numbers <- c(3, 6, 9, 12, 15)
numbers
numbers <- c(3, 6, 9, 12, 15)
numbers
numbers + 2
numbers * 3
mean(numbers)
max(numbers)
numbers[2:4]
mean(numbers)
max(numbers)
median(numbers)
weird <- c(1, 1, 1, 1, 100)
mean(weird)
median(weird)
mode(weird)
mode(weird)
median(weired)
median (weird)
custom <- c(0, 1, 1, 3, 5, 100)
mean(custom)
median(custom)
`mode<-`(custom)
mode(custom)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
data <- c(1, 2, 2, 3, 3, 3, 4)
get_mode(data)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
data <- custom
custom <- c(0, 1, 1, 3, 5, 100)
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
get_mode(custom)
# Install once (if needed)
# install.packages(c("tidyverse", "tidymodels"))

library(tidyverse)
library(tidymodels)

# 1) Read your data (CSV)
# Replace with your file name
df <- read_csv("your_data.csv")

# 2) Make a binary label from a continuous outcome (biomass as example)
df <- df %>%
  mutate(
    high = if_else(biomass >= median(biomass, na.rm = TRUE), "high", "low"),
    high = factor(high, levels = c("low", "high"))
  )

# 3) Keep only predictors you want + the label
# Edit these to match your column names
ml_df <- df %>%
  select(high, pH, CEC, BET, Temp, Dose) %>%
  drop_na()

# 4) Train/test split
set.seed(123)
split <- initial_split(ml_df, prop = 0.8, strata = high)
train <- training(split)
test  <- testing(split)

# 5) Preprocessing recipe (standardize numeric predictors)
rec <- recipe(high ~ ., data = train) %>%
  step_normalize(all_numeric_predictors())

# 6) Model choice (Random Forest is robust + good for beginners)
rf_model <- rand_forest(trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_model)

# 7) Fit
fit <- fit(wf, data = train)

# 8) Evaluate on test set
pred <- predict(fit, test, type = "prob") %>%
  bind_cols(predict(fit, test, type = "class")) %>%
  bind_cols(test %>% select(high))

# Accuracy
pred %>% accuracy(truth = high, estimate = .pred_class)

# Confusion matrix
pred %>% conf_mat(truth = high, estimate = .pred_class)

# ROC AUC (optional but useful)
pred %>% roc_auc(truth = high, .pred_high)

# 9) Variable importance (what features matter most)
rf_fit <- extract_fit_parsnip(fit)
vip <- ranger::importance(rf_fit$fit)

sort(vip, decreasing = TRUE)
# Install once (if needed)
# install.packages(c("tidyverse", "tidymodels"))

library(tidyverse)
library(tidymodels)

# 1) Read your data (CSV)
# Replace with your file name
df <- read_csv("your_data.csv")

# 2) Make a binary label from a continuous outcome (biomass as example)
df <- df %>%
  mutate(
    high = if_else(biomass >= median(biomass, na.rm = TRUE), "high", "low"),
    high = factor(high, levels = c("low", "high"))
  )

# 3) Keep only predictors you want + the label
# Edit these to match your column names
ml_df <- df %>%
  select(high, pH, CEC, BET, Temp, Dose) %>%
  drop_na()

# 4) Train/test split
set.seed(123)
split <- initial_split(ml_df, prop = 0.8, strata = high)
train <- training(split)
test  <- testing(split)

# 5) Preprocessing recipe (standardize numeric predictors)
rec <- recipe(high ~ ., data = train) %>%
  step_normalize(all_numeric_predictors())

# 6) Model choice (Random Forest is robust + good for beginners)
rf_model <- rand_forest(trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_model)

# 7) Fit
fit <- fit(wf, data = train)

# 8) Evaluate on test set
pred <- predict(fit, test, type = "prob") %>%
  bind_cols(predict(fit, test, type = "class")) %>%
  bind_cols(test %>% select(high))

# Accuracy
pred %>% accuracy(truth = high, estimate = .pred_class)

# Confusion matrix
pred %>% conf_mat(truth = high, estimate = .pred_class)

# ROC AUC (optional but useful)
pred %>% roc_auc(truth = high, .pred_high)

# 9) Variable importance (what features matter most)
rf_fit <- extract_fit_parsnip(fit)
vip <- ranger::importance(rf_fit$fit)

sort(vip, decreasing = TRUE)
# 创建一个简单数据
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 打印数据
print(data)
# 创建一个简单数据
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 打印数据
print(data)

model <- glm(plant_status ~ temperature, data = data, family = "binomial")
summary(model)
# 创建一个简单数据
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 打印数据
print(data)
model <- glm(plant_status ~ temperature, data = data, family = "binomial")
summary(model)
model <- glm(plant_status ~ temperature, data = data, family = "binomial")
summary(model)
summary(model)



# 创建数据
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 创建数据
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 建立模型
model <- glm(plant_status ~ temperature, data = data, family = "binomial")

# 查看结果
summary(model)

# 创建数据
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 建立模型
model <- glm(plant_status ~ temperature, data = data, family = "binomial")
# 查看结果
summary(model)

# ----------------------------------------
# Simple Machine Learning Example in R
# Logistic Regression (Classification)
# ----------------------------------------

# 1. Create dataset
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)

# 2. View dataset
print(data)

# 3. Train model (logistic regression)
model <- glm(plant_status ~ temperature,
             data = data,
             family = "binomial")

# 4. Summarize model results
summary(model)
# 5. Predict probability
predicted_prob <- predict(model, type = "response")

# View predictions
print(predicted_prob)

# ----------------------------------------
# Simple Machine Learning Example in R
# Logistic Regression (Classification)
# ----------------------------------------
# 1. Create dataset
data <- data.frame(
  temperature = c(20, 22, 25, 30, 35),
  plant_status = c(0, 0, 0, 1, 1)
)
# 2. View dataset
print(data)
# 3. Train model (logistic regression)
model <- glm(plant_status ~ temperature,
             data = data,
             family = "binomial")
# 4. Summarize model results
summary(model)
# 5. Predict probability
predicted_prob <- predict(model, type = "response")
# View predictions
print(predicted_prob)
