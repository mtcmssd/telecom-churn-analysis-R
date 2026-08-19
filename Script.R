# Customer Churn Analysis
# Data: Churn.xlsx

library(tidyverse)
library(readxl)
library(broom)
library(MASS)
library(class)
library(FNN)
library(car)

# Loading & cleaning data

Churn <- read_excel("Churn.xlsx")

set.seed(52)

Churn <- Churn |>
  slice_sample(n = 1000)

glimpse(Churn)
names(Churn)
dim(Churn)

churn <- Churn |>
  mutate(TotalCharges = as.numeric(TotalCharges)) |>
  drop_na(TotalCharges)

dim(churn)

churn <- churn |>
  mutate(
    SeniorCitizen = factor(SeniorCitizen, levels = c(0, 1), labels = c("No", "Yes")),
    Churn         = factor(Churn, levels = c("No", "Yes")),
    Contract      = factor(Contract),
    InternetService = factor(InternetService),
    PaperlessBilling = factor(PaperlessBilling),
    Partner       = factor(Partner),
    Dependents    = factor(Dependents),
    StreamingTV   = factor(StreamingTV),
    StreamingMovies = factor(StreamingMovies)
  )

# Descriptive Statistics & Data Exploration

churn |>
  count(Churn) |>
  mutate(Percent = round(100 * n / sum(n), 2))

churn |>
  count(Contract) |>
  mutate(Percent = round(100 * n / sum(n), 2))

churn |>
  count(InternetService) |>
  mutate(Percent = round(100 * n / sum(n), 2))

churn |>
  count(PaymentMethod) |>
  mutate(Percent = round(100 * n / sum(n), 2))

churn |>
  summarise(
    across(
      c(tenure, MonthlyCharges, TotalCharges),
      list(
        mean = \(x) mean(x, na.rm = TRUE),
        sd   = \(x) sd(x, na.rm = TRUE),
        min  = \(x) min(x, na.rm = TRUE),
        max  = \(x) max(x, na.rm = TRUE)
      )
    )
  )

cor(churn$tenure, churn$MonthlyCharges)
cor(churn$tenure, churn$TotalCharges)
cor(churn$MonthlyCharges, churn$TotalCharges)

p_scatter <- churn |>
  ggplot(aes(tenure, MonthlyCharges, color = Churn)) +
  geom_point(alpha = 0.55, size = 1.4) +
  labs(
    title = "Monthly Charges vs Tenure, by Churn Status",
    x = "Tenure (months)",
    y = "Monthly Charges (USD)"
  ) +
  theme_minimal()

print(p_scatter)
ggsave("scatter_tenure_charges.png", plot = p_scatter, width = 6.5, height = 4.2)

p_box <- churn |>
  ggplot(aes(Contract, MonthlyCharges, fill = Contract)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Distribution of Monthly Charges by Contract Type",
    x = "Contract Type",
    y = "Monthly Charges (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_box)
ggsave("boxplot_charges_contract.png", plot = p_box, width = 6.5, height = 4.2)

# Linear Regression
#Dependent variable (continuous): MonthlyCharges

lm_fit <- lm(
  MonthlyCharges ~ tenure + Contract + StreamingTV + PaperlessBilling + SeniorCitizen,
  data = churn
)

summary(lm_fit)
tidy(lm_fit)
glance(lm_fit)
confint(lm_fit)

aug_lm <- augment(lm_fit)

p_resid <- aug_lm |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residuals vs Fitted Values (Linear Regression Model)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

print(p_resid)
ggsave("resid_vs_fitted.png", plot = p_resid, width = 6.5, height = 4.2)

p_qq <- aug_lm |>
  ggplot(aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot of Standardized Residuals") +
  theme_minimal()

print(p_qq)
ggsave("qq_plot.png", plot = p_qq, width = 6.5, height = 4.2)

vif(lm_fit)

#Logistic Regression
#Dependent variable (binary): Churn (No/Yes)

glm_fit <- glm(
  Churn ~ tenure + MonthlyCharges + Contract + InternetService +
    PaperlessBilling + SeniorCitizen + Partner + Dependents,
  data = churn,
  family = binomial
)

summary(glm_fit)
tidy(glm_fit)
glance(glm_fit)

tidy(glm_fit) |>
  mutate(
    OR       = exp(estimate),
    OR_lower = exp(estimate - 1.96 * std.error),
    OR_upper = exp(estimate + 1.96 * std.error)
  ) |>
  dplyr::select(term, estimate, std.error, statistic, p.value, OR, OR_lower, OR_upper)

glm_null <- glm(Churn ~ 1, data = churn, family = binomial)
anova(glm_null, glm_fit, test = "Chisq")

1 - (glm_fit$deviance / glm_fit$null.deviance)

# Model Comparison

n <- nrow(churn)
train_idx <- sample(seq_len(n), size = floor(0.7 * n))

churn_train <- churn[train_idx, ]
churn_test  <- churn[-train_idx, ]

dim(churn_train)
dim(churn_test)

#Logistic Regression vs LDA

glm_fit_tr <- glm(
  Churn ~ tenure + MonthlyCharges + Contract + InternetService +
    PaperlessBilling + SeniorCitizen + Partner + Dependents,
  data = churn_train,
  family = binomial
)

glm_test_results <- churn_test |>
  mutate(
    glm_prob = predict(glm_fit_tr, newdata = churn_test, type = "response"),
    glm_pred = if_else(glm_prob > 0.5, "Yes", "No"),
    glm_pred = factor(glm_pred, levels = levels(Churn))
  )

glm_test_results |>
  count(glm_pred, Churn) |>
  pivot_wider(names_from = Churn, values_from = n, values_fill = 0)

glm_test_results |>
  summarise(
    accuracy = mean(glm_pred == Churn),
    error_rate = mean(glm_pred != Churn)
  )

lda_fit <- lda(
  Churn ~ tenure + MonthlyCharges + Contract + InternetService +
    PaperlessBilling + SeniorCitizen + Partner + Dependents,
  data = churn_train
)

lda_fit

lda_pred <- predict(lda_fit, newdata = churn_test)

lda_results <- churn_test |>
  mutate(lda_class = lda_pred$class)

lda_results |>
  count(lda_class, Churn) |>
  pivot_wider(names_from = Churn, values_from = n, values_fill = 0)

lda_results |>
  summarise(
    accuracy = mean(lda_class == Churn),
    error_rate = mean(lda_class != Churn)
  )

p_accuracy <- tibble(
  Model = c("Logistic Regression", "LDA"),
  Accuracy = c(
    mean(glm_test_results$glm_pred == glm_test_results$Churn),
    mean(lda_results$lda_class == lda_results$Churn)
  )
) |>
  ggplot(aes(Model, 100 * Accuracy, fill = Model)) +
  geom_col(width = 0.5) +
  labs(
    title = "Logistic Regression vs LDA",
    y = "Test-Set Accuracy (%)", x = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_accuracy)
ggsave("accuracy_comparison.png", plot = p_accuracy, width = 6, height = 4)

#Linear Regression vs KNN Regression

lm_fit_tr <- lm(
  MonthlyCharges ~ tenure + Contract + StreamingTV + PaperlessBilling + SeniorCitizen,
  data = churn_train
)

lm_pred_test <- predict(lm_fit_tr, newdata = churn_test)

rmse_lm <- sqrt(mean((churn_test$MonthlyCharges - lm_pred_test)^2))
rmse_lm

knn_formula <- ~ tenure + Contract + StreamingTV + PaperlessBilling + SeniorCitizen - 1

X_train <- model.matrix(knn_formula, data = churn_train)
X_test  <- model.matrix(knn_formula, data = churn_test)

X_train_scaled <- scale(X_train)
X_test_scaled  <- scale(
  X_test,
  center = attr(X_train_scaled, "scaled:center"),
  scale  = attr(X_train_scaled, "scaled:scale")
)

k_grid <- c(3, 5, 10, 15, 20, 25, 30)

knn_rmse <- map_dbl(k_grid, function(k) {
  fit <- knn.reg(
    train = X_train_scaled,
    test  = X_test_scaled,
    y     = churn_train$MonthlyCharges,
    k     = k
  )
  sqrt(mean((churn_test$MonthlyCharges - fit$pred)^2))
})

knn_results <- tibble(k = k_grid, test_rmse = knn_rmse)
knn_results

best_k <- knn_results$k[which.min(knn_results$test_rmse)]
best_k

p_rmse <- knn_results |>
  ggplot(aes(k, test_rmse)) +
  geom_line(color = "firebrick") +
  geom_point(color = "firebrick") +
  geom_hline(yintercept = rmse_lm, linetype = "dashed", color = "steelblue") +
  labs(
    title = "Linear Regression vs KNN Regression",
    x = "K (number of neighbors)",
    y = "Test RMSE (USD)"
  ) +
  annotate("text", x = max(k_grid), y = rmse_lm, label = "Linear Regression",
           vjust = -0.7, hjust = 1, color = "steelblue") +
  theme_minimal()

print(p_rmse)
ggsave("rmse_comparison.png", plot = p_rmse, width = 6, height = 4)
