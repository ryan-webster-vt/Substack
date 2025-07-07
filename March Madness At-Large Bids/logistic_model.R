library(tidyverse)
library(rsample)
library(caret)
library(pROC)

source("March Madness At-Large Bids/data_prep.R")


# Training and Testing Data -----------------------------------------------

set.seed(1)
at_large_team_sheets <- at_large_team_sheets[sample(nrow(at_large_team_sheets)), ]

train <- at_large_team_sheets %>% filter(season != 2024)
test <- at_large_team_sheets %>% filter(season == 2024)


# Train Full Logit Model --------------------------------------------------

full_glm_model <- glm(
  at_large ~ net + sor + kp + q1_wins + q3q4_losses, 
  data = train, 
  family = binomial
)

full_summary <- summary(full_glm_model)

(r2_mcfadden <- 1 - (full_summary$deviance / full_summary$null.deviance))

# R2: 90.14
# AIC: 101.49


# Predict -----------------------------------------------------------------


test$predictions <- predict(full_glm_model, newdata = test, type = "response")

test <- test %>% 
  group_by(season) %>% 
  mutate(
    adj_prediction = ifelse(
      season != 2021 | season >= 2024, 
      pmin(36 / sum(predictions) * predictions, 1),
      pmin(37 / sum(predictions) * predictions, 1)
    )
  )

test <- test %>%
  arrange(desc(predictions)) %>%
  mutate(predict_bid = row_number() <= 37)

confusionMatrix(
  factor(test$at_large),
  factor(test$predict_bid),
  positive = "TRUE"
)


# ROC ---------------------------------------------------------------------

test <- test %>% 
  mutate(
    at_large = if_else(at_large, 1, 0),
    predict_bid = if_else(predict_bid, 1, 0)
  )

roc_obj <- roc(response = test$at_large, predictor = test$predict_bid)
plot(roc_obj, col = "#1f77b4", lwd = 2, main = "ROC Curve for At-Large Bid Model")

