
# Modeling part wave_2 
# G7 vs Non G7 countries
G7_wave2_data <- selected_data_wave_2 %>% filter(country_tag == 'G7') %>% select(gender, age, isced, eurod, bmi, 
                                                                                 br010, br015, smoking, chronic, cogscore)

nonG7_wave2_data <- selected_data_wave_2 %>% filter(country_tag == 'Non_G7') %>% select(gender, age, isced, eurod, bmi, 
                                                                                        br010, br015, smoking, chronic, cogscore)

library(recipes)
# Create the preprocessing recipe for G7
recipe_obj_G7 <- recipe(cogscore ~ ., data = G7_wave2_data) %>%
  # One-hot encoding for unordered categorical variables
  step_dummy(gender, smoking, one_hot = TRUE) %>%
  step_integer(isced, zero_based = TRUE) %>%  # Keep zero_based = TRUE, because isced starts from 0
  step_integer(eurod, zero_based = TRUE) %>%  # Keep zero_based = TRUE, because eurod starts from 0
  step_integer(br010) %>%  # Do not use zero_based = TRUE, keep starting from 1
  step_integer(br015) %>%  # Do not use zero_based = TRUE, keep starting from 1
  step_integer(chronic, zero_based = TRUE) %>%  # Keep zero_based = TRUE, because chronic may start from 0
  # Center and scale numerical predictors
  step_center(age, bmi) %>%
  step_scale(age, bmi)



# Create the preprocessing recipe for G7_rf
recipe_obj_G7_rf <- recipe(cogscore ~ ., data = G7_wave2_data) %>%
  step_center(age, bmi) %>%
  step_scale(age, bmi)



library(caret)
set.seed(3456)
trainIndex_G7 <- createDataPartition(G7_wave2_data$cogscore, p = .8, 
                                     list = FALSE, 
                                     times = 1)


G7_wave2_Train <- G7_wave2_data[ trainIndex_G7,]
G7_wave2_Test  <- G7_wave2_data[-trainIndex_G7,]

trainIndex_nonG7 <- createDataPartition(nonG7_wave2_data$cogscore, p = .8, 
                                        list = FALSE, 
                                        times = 1)

nonG7_wave2_Train <- nonG7_wave2_data[ trainIndex_nonG7,]
nonG7_wave2_Test  <- nonG7_wave2_data[-trainIndex_nonG7,]


# Train the classical regression model
lm_model_G7 <- train(
  recipe_obj_G7,
  data = G7_wave2_Train,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

# Train the Lasso regression model
lasso_model_G7 <- train(
  recipe_obj_G7,
  data = G7_wave2_Train,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.001))
)

# Train the SVMR model
svmR_model_G7 <- train(
  recipe_obj_G7,
  data = G7_wave2_Train,
  method = "svmRadial",
  trControl = trainControl(method = "cv", number = 10)
)

# Train the Random Forest model
rf_model_G7 <- train(
  recipe_obj_G7_rf,
  data = G7_wave2_Train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 5
)





library(DALEX)
explainer_regr_lm_G7 <- DALEX::explain(lm_model_G7, label = "lm", 
                                       data = G7_wave2_Test, y = G7_wave2_Test$cogscore, 
                                       verbose = FALSE)

explainer_regr_lasso_G7 <- DALEX::explain(lasso_model_G7, label = "lasso", 
                                          data = G7_wave2_Test, y = G7_wave2_Test$cogscore, 
                                          verbose = FALSE)


explainer_svmR_G7 <- DALEX::explain(svmR_model_G7, label = "svmRadial", 
                                    data = G7_wave2_Test, y = G7_wave2_Test$cogscore, 
                                    verbose = FALSE)

explainer_regr_rf_G7 <- DALEX::explain(rf_model_G7, label = "rf", 
                                       data = G7_wave2_Test, y = G7_wave2_Test$cogscore, 
                                       verbose = FALSE)



# Model performance in general
mp_explainer_regr_lm_G7 <- model_performance(explainer_regr_lm_G7)
mp_explainer_regr_lm_G7

mp_explainer_regr_lasso_G7 <- model_performance(explainer_regr_lasso_G7)
mp_explainer_regr_lasso_G7

mp_explainer_svmR_G7 <- model_performance(explainer_svmR_G7)
mp_explainer_svmR_G7

mp_explainer_regr_rf_G7 <- model_performance(explainer_regr_rf_G7)
mp_explainer_regr_rf_G7

plot(mp_explainer_regr_lm_G7, mp_explainer_regr_lasso_G7, mp_explainer_svmR_G7, mp_explainer_regr_rf_G7)
plot(mp_explainer_regr_lm_G7, mp_explainer_regr_lasso_G7, mp_explainer_svmR_G7, mp_explainer_regr_rf_G7, geom = "boxplot")


## Instance Level


spec_obs <- G7_wave2_data %>%filter(gender == 1  & age > 69 & age < 71 &
                                      isced == 3 & eurod == 1 & 
                                      br010 == 1 & br015 == 1 ) %>% 
  select(!c('cogscore'))
spec_obs

### Shapley values 

# linear model
bd_lm_G7 <- predict_parts(explainer = explainer_regr_lm_G7,
                          new_observation = spec_obs,
                          type = "break_down")
bd_lm_G7 
# lasso model
bd_lasso_G7 <- predict_parts(explainer = explainer_regr_lasso_G7,
                             new_observation = spec_obs,
                             type = "break_down")
bd_lasso_G7 
# svmRadial model
bd_svmR_G7 <- predict_parts(explainer = explainer_svmR_G7,
                            new_observation = spec_obs,
                            type = "break_down")
bd_svmR_G7 
# random forest model
bd_rf_G7 <- predict_parts(explainer = explainer_regr_rf_G7,
                          new_observation = spec_obs,
                          type = "break_down")
bd_rf_G7 


# Predict
shap_spec_obs_lm_G7 <- predict_parts(
  explainer = explainer_regr_lm_G7, 
  new_observation = spec_obs, 
  type = "shap",  
  B = 25
)

shap_spec_obs_lasso_G7 <- predict_parts(
  explainer = explainer_regr_lasso_G7, 
  new_observation = spec_obs, 
  type = "shap",  
  B = 25
)

shap_spec_obs_svmR_G7 <- predict_parts(
  explainer = explainer_svmR_G7, 
  new_observation = spec_obs, 
  type = "shap",  
  B = 25
)

shap_spec_obs_rf_G7 <- predict_parts(
  explainer = explainer_regr_rf_G7, 
  new_observation = spec_obs, 
  type = "shap",  
  B = 25
)

# Display the result
plot(shap_spec_obs_lm_G7)
plot(shap_spec_obs_lasso_G7)
plot(shap_spec_obs_svmR_G7)
plot(shap_spec_obs_rf_G7)

# Ceteris Paribus Profiles
regr_lm_G7 <- predict_profile(explainer = explainer_regr_lm_G7, 
                              new_observation = spec_obs)
regr_lm_G7

regr_lasso_G7 <- predict_profile(explainer = explainer_regr_lasso_G7, 
                                 new_observation = spec_obs)
regr_lasso_G7

regr_svmR_G7 <- predict_profile(explainer = explainer_svmR_G7, 
                                new_observation = spec_obs)
regr_svmR_G7

regr_rf_G7 <- predict_profile(explainer = explainer_regr_rf_G7, 
                              new_observation = spec_obs)
regr_rf_G7

plot(regr_lm_G7, regr_rf_G7, regr_lasso_G7, regr_svmR_G7, color = "_label_", variables = c("age", "cogscore")) +
  ggtitle("Ceteris-paribus profile", "")
plot(regr_lm_G7, regr_rf_G7, regr_lasso_G7, regr_svmR_G7, color = "_label_", variables = c("bmi", "cogscore")) +
  ggtitle("Ceteris-paribus profile", "")


# Data Set Level

# Variable Importance

vi_regr_lm_G7 <- model_parts(explainer_regr_lm_G7, loss_function = loss_root_mean_square)
vi_regr_lasso_G7 <- model_parts(explainer_regr_lasso_G7, loss_function = loss_root_mean_square)
vi_regr_svmR_G7 <- model_parts(explainer_svmR_G7, loss_function = loss_root_mean_square)
vi_regr_rf_G7 <- model_parts(explainer_regr_rf_G7, loss_function = loss_root_mean_square)

plot(vi_regr_lm_G7, vi_regr_lasso_G7, vi_regr_svmR_G7, vi_regr_rf_G7)
plot(vi_regr_svmR_G7)

# Partial Dependence Plots 

# Age
pdp_regr_lm_G7  <- model_profile(explainer_regr_lm_G7, variable =  "age", type = "partial")
pdp_regr_lasso_G7  <- model_profile(explainer_regr_lasso_G7, variable =  "age", type = "partial")
pdp_regr_svmR_G7  <- model_profile(explainer_svmR_G7, variable =  "age", type = "partial")
pdp_regr_rf_G7  <- model_profile(explainer_regr_rf_G7, variable =  "age", type = "partial")

plot(pdp_regr_lm_G7, pdp_regr_lasso_G7, pdp_regr_svmR_G7, pdp_regr_rf_G7)

# Education level
pdp_regr_lm_G7  <- model_profile(explainer_regr_lm_G7, variable =  "isced", type = "partial")
pdp_regr_lasso_G7  <- model_profile(explainer_regr_lasso_G7, variable =  "isced", type = "partial")
pdp_regr_svmR_G7  <- model_profile(explainer_svmR_G7, variable =  "isced", type = "partial")
pdp_regr_rf_G7  <- model_profile(explainer_regr_rf_G7, variable =  "isced", type = "partial")

plot(pdp_regr_lm_G7, pdp_regr_lasso_G7, pdp_regr_svmR_G7, pdp_regr_rf_G7)


# eurod
pdp_regr_lm_G7  <- model_profile(explainer_regr_lm_G7, variable =  "eurod", type = "partial")
pdp_regr_lasso_G7  <- model_profile(explainer_regr_lasso_G7, variable =  "eurod", type = "partial")
pdp_regr_svmR_G7  <- model_profile(explainer_svmR_G7, variable =  "eurod", type = "partial")
pdp_regr_rf_G7  <- model_profile(explainer_regr_rf_G7, variable =  "eurod", type = "partial")

plot(pdp_regr_lm_G7, pdp_regr_lasso_G7, pdp_regr_svmR_G7, pdp_regr_rf_G7)
