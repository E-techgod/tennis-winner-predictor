############################################################
# ATP Match Outcome Prediction - MATH 4322 Final Project
# Models: Logistic Regression & Random Forest
# Data: Jeff Sackmann ATP matches (2003–2019, 2021–2024)
############################################################
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(randomForest)
library(pROC)
library(ranger)
  
## ========================================================
## 1. Read combined data 
## ========================================================
matches = read_csv('all_atp_matches.csv', show_col_types = FALSE)
player_choices <- sort(unique(c(matches$winner_name, matches$loser_name)))

# Filtering for Pre-Match Features Only
matches = matches %>%
  filter(
    !is.na(winner_rank), !is.na(loser_rank),
    !is.na(winner_age),  !is.na(loser_age),
    !is.na(winner_rank_points), !is.na(loser_rank_points),
    !is.na(surface), !is.na(tourney_level), !is.na(best_of), !is.na(round)
  )

set.seed(4322)

# Randomly assign Player 1 to avoid leakage
flip = rbinom(nrow(matches), size = 1, prob = 0.5) == 1

data = matches %>%
  mutate(
    Win = if_else(flip, 1L, 0L),
    
    p1_rank        = if_else(flip, winner_rank,        loser_rank),
    p2_rank        = if_else(flip, loser_rank,         winner_rank),
    p1_rank_points = if_else(flip, winner_rank_points, loser_rank_points),
    p2_rank_points = if_else(flip, loser_rank_points,  winner_rank_points),
    p1_age         = if_else(flip, winner_age,         loser_age),
    p2_age         = if_else(flip, loser_age,          winner_age),
    
    rank_diff    = p1_rank        - p2_rank,
    rankpts_diff = p1_rank_points - p2_rank_points,
    age_diff     = p1_age         - p2_age
  ) %>%
  transmute(
    Win,
    year,
    surface       = factor(surface),
    tourney_level = factor(tourney_level),
    best_of,
    round         = factor(round),
    
    p1_rank, p2_rank,
    p1_rank_points, p2_rank_points,
    p1_age, p2_age,
    rank_diff, rankpts_diff, age_diff
  ) %>%
  drop_na()

# Response as factor with levels 0,1 (for classification)
data$Win = factor(data$Win, levels = c(0, 1))
data$surface = relevel(data$surface, ref = "Hard")
data$best_of = factor(data$best_of) 
data = droplevels(data) 

# Save modeling-ready dataset
write_csv(data, "tennis_model_data.csv")

#################### Model 1: Logistic Regression #################### 
glm_formula = Win ~ rank_diff + rankpts_diff + age_diff +
  p1_rank + p2_rank +
  p1_rank_points + p2_rank_points +
  p1_age + p2_age +
  surface + tourney_level + best_of + round + year

# 10 repeated train/test splits (80/20)
set.seed(4322)
n = 10
test_error = numeric(n)
auc_value = numeric(n)

for (i in 1:n){
  
  # 80/20 random split
  train_idx = sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
  train = data[train_idx, ]
  test = data[-train_idx, ]
  
  # Logistic Regression Model
  glm_model = glm(glm_formula, data = train, family = binomial)
  
  # Predict probabilities for Player 1 win
  pred_prob = predict(glm_model, newdata = test, type = "response")
  
  # Convert probs → class predictions (cutoff = 0.5)
  pred_class = ifelse(pred_prob > 0.5, 1, 0)
  
  # Compute misclassification error - FIXED: removed "- 1"
  test_error[i] = mean(pred_class != as.numeric(as.character(test$Win)))
  
  # Compute AUC - FIXED: proper factor conversion
  auc_value[i] = auc(response = as.numeric(as.character(test$Win)), predictor = pred_prob)
  
  cat(sprintf("Iteration %d: Error = %.4f | AUC = %.4f\n",
              i, test_error[i], auc_value[i]))
}

mean_error = mean(test_error)
mean_auc  = mean(auc_value)

cat("\n=== SUMMARY RESULTS ===\n")
cat(sprintf("Logistic Regression - Mean Test Error: %.4f\n", mean_error))
cat(sprintf("Logistic Regression - Mean AUC: %.4f\n",   mean_auc))

glm_model_full = glm(glm_formula, data = data, family = binomial)
summary(glm_model_full)

#################### Model 2: Random Forest #################### 

rf_formula = Win ~ 
  rank_diff + rankpts_diff + age_diff +
  p1_rank + p2_rank +
  p1_rank_points + p2_rank_points +
  p1_age + p2_age +
  surface + tourney_level + best_of + round + year


set.seed(4322)

n = 10
rf_error = numeric(n)
rf_auc= numeric(n)

for (i in 1:n) {
  
  # 80/20 split
  train_idx = sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
  train = data[train_idx, ]
  test  = data[-train_idx, ]
  
  # Fit random forest (probability = TRUE for class probs)
  rf_model = ranger(
    formula        = rf_formula,
    data           = train,
    num.trees      = 500,
    probability    = TRUE,
    importance     = "impurity",   # To inspect variable importance
    respect.unordered.factors = "order"
  )
  
  # Predict probabilities on test set
  rf_pred = predict(rf_model, data = test)
  prob_win = rf_pred$predictions[, "1"]   # P(Win == "1")
  
  # Turn probs into class predictions with cutoff 0.5
  pred_class = ifelse(prob_win > 0.5, 1, 0)
  true_class = ifelse(test$Win == "1", 1, 0)
  
  # Misclassification error
  rf_error[i] = mean(pred_class != true_class)
  
  # AUC
  rf_auc[i] = auc(test$Win, prob_win)
  
  cat(sprintf("RF Iteration %d: Error = %.4f | AUC = %.4f\n",
              i, rf_error[i], rf_auc[i]))
}

rf_mean_error= mean(rf_error)
rf_mean_auc= mean(rf_auc)

cat("\n=== SUMMARY RESULTS ===\n")
cat(sprintf("Random Forest - Mean Test Error: %.4f\n", rf_mean_error))
cat(sprintf("Random Forest - Mean AUC: %.4f\n",   rf_mean_auc))

rf_full = ranger(
  formula        = rf_formula,
  data           = data,
  num.trees      = 500,
  probability    = TRUE,
  importance     = "impurity",
  respect.unordered.factors = "order"
)

rf_full$variable.importance

var_imp = sort(rf_full$variable.importance, decreasing = TRUE)
barplot(var_imp, las = 2, cex.names = 0.7,
        main = "Random Forest Variable Importance")

saveRDS(list(glm = glm_model_full, rf = rf_full, template = data), "saved_models.rds")

#################### Predict Match Winner Program starts here ####################
predict_match=function(
    p1_rank, p2_rank,
    p1_rank_points, p2_rank_points,
    p1_age, p2_age,
    surface,          # e.g. "Hard", "Clay", "Grass"
    tourney_level,    # e.g. "G", "M", "A", "250"
    best_of,          # 3 or 5
    round,            # e.g. "R32", "QF", "SF", "F"
    year,
    glm_model = glm_model_full,
    rf_model= rf_full,
    data_template = data
) {
  # Build a single-row data frame in the same structure as `data`
  newdata=tibble(
    year = year,
    surface = factor(surface, levels = levels(data_template$surface)),
    tourney_level = factor(tourney_level, levels = levels(data_template$tourney_level)),
    best_of = factor(best_of, levels = levels(data_template$best_of)),
    round = factor(round, levels = levels(data_template$round)),
    
    p1_rank = p1_rank,
    p2_rank = p2_rank,
    p1_rank_points = p1_rank_points,
    p2_rank_points = p2_rank_points,
    p1_age = p1_age,
    p2_age = p2_age,
    
    rank_diff= p1_rank - p2_rank,
    rankpts_diff= p1_rank_points - p2_rank_points,
    age_diff= p1_age - p2_age
  )
  
  # --- Logistic regression prediction ---
  glm_prob=predict(glm_model, newdata = newdata, type = "response")  # P(Win == 1)
  
  # --- Random forest prediction ---
  rf_pred=predict(rf_model, data = newdata)
  rf_prob=rf_pred$predictions[, "1"]  # P(Win == "1")
  
  # Build a clean summary table
  summary_tbl=tibble(
    model = c("Logistic Regression", "Random Forest"),
    prob_p1_win = c(glm_prob, rf_prob),
    prob_p2_win = 1 - prob_p1_win,
    predicted_winner = if_else(prob_p1_win >= 0.5, "Player 1", "Player 2")
  )
  
  list(
    input_features = newdata,
    predictions = summary_tbl
  )
}


#################### What if” match up (hypothetical) ####################
res = predict_match(
  p1_rank= 1,
  p2_rank= 2,
  p1_rank_points= 11830,
  p2_rank_points= 7915,
  p1_age= 23,
  p2_age= 27,
  surface= "Hard",
  tourney_level = "M",
  best_of= 5,
  round= "F",
  year= 2024,
  data_template = data
)

res$input_features      # what was fed to the models
res$predictions         

predict_from_match_row=function(
    row_index,
    matches,
    p1_role = c("winner", "loser", "random"),
    glm_model= glm_model_full,
    rf_model= rf_full,
    data_template = data
) {
  p1_role=match.arg(p1_role)
  
  row=matches[row_index, ]
  
  # Decide who is Player 1
  if (p1_role == "random") {
    flip=rbinom(1, size = 1, prob = 0.5) == 1
    p1_is_winner=flip
  } else if (p1_role == "winner") {
    p1_is_winner=TRUE
  } else {
    p1_is_winner=FALSE
  }
  
  if (p1_is_winner) {
    p1_name=row$winner_name
    p2_name=row$loser_name
    
    p1_rank=row$winner_rank
    p2_rank=row$loser_rank
    p1_rank_points=row$winner_rank_points
    p2_rank_points=row$loser_rank_points
    p1_age=row$winner_age
    p2_age=row$loser_age
  } else {
    p1_name=row$loser_name
    p2_name=row$winner_name
    
    p1_rank =row$loser_rank
    p2_rank =row$winner_rank
    p1_rank_points=row$loser_rank_points
    p2_rank_points=row$winner_rank_points
    p1_age=row$loser_age
    p2_age=row$winner_age
  }
  
  # Match context (from the row)
  surface=row$surface
  tourney_level=row$tourney_level
  best_of=row$best_of
  round =row$round
  year=row$year
  
  # Call your generic helper
  res=predict_match(
    p1_rank= p1_rank,
    p2_rank= p2_rank,
    p1_rank_points= p1_rank_points,
    p2_rank_points= p2_rank_points,
    p1_age= p1_age,
    p2_age= p2_age,
    surface= surface,
    tourney_level= tourney_level,
    best_of = best_of,
    round = round,
    year= year,
    glm_model = glm_model,
    rf_model= rf_model,
    data_template = data_template
  )
  
  # Attach player names + who is P1/P2
  res$match_info=list(
    p1_name = p1_name,
    p2_name = p2_name,
    p1_is_winner_in_real_life = p1_is_winner,
    surface = surface,
    tourney_level = tourney_level,
    best_of = best_of,
    round = round,
    year = year
  )
  
  # Add names into the prediction table
  res$predictions=res$predictions %>%
    mutate(
      predicted_winner_name = ifelse(predicted_winner == "Player 1", p1_name, p2_name)
    )
  
  res
}

#################### Real match from your dataset ####################
# Pick any specific match by players/year/round
idx = which(
  matches$tourney_name == "Wimbledon" &
  matches$winner_name == "Novak Djokovic" &
  matches$loser_name == "Roger Federer"   &
  matches$year == 2019             &
  matches$round == "F"
)

idx #C Check the index
matches[idx, c("tourney_name", "winner_name", "loser_name",
               "surface", "round", "year")]

# Look for the match we want to predict in the dataset
res_row = predict_from_match_row(
  row_index= idx[1],
  matches= matches,
  p1_role= "winner",
  data_template= data
)


res_row$match_info       # who is P1/P2, surface, round, etc.
res_row$predictions      # model probs + predicted winner name



