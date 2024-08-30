library(readr)
library(dplyr)
library(ggplot2)
library(visdat)
library(GGally)

setwd("C:/Users/晨钟暮鼓/Desktop")
data <- read_csv("easyshare.csv")
data <- data %>%
  mutate(across(-mergeid, as.numeric))



data <- data %>%
  select(-c(eduyears_mod, mergeid))

clean_wave_data <- function(wave_data, exclude_vars) {
  wave_data %>%
    select(-all_of(exclude_vars)) %>%
    na.omit()
}


data_wave_1_clean <- clean_wave_data(data %>% filter(wave == 1), c("casp"))
data_wave_2_clean <- clean_wave_data(data %>% filter(wave == 2), character(0))
data_wave_4_clean <- clean_wave_data(data %>% filter(wave == 4), c("sp008_"))
data_wave_5_clean <- clean_wave_data(data %>% filter(wave == 5), c("sp008_"))
data_wave_6_clean <- clean_wave_data(data %>% filter(wave == 6), c("br010_mod"))
data_wave_7_clean <- clean_wave_data(data %>% filter(wave == 7), c("br010_mod"))
data_wave_8_clean <- clean_wave_data(data %>% filter(wave == 8), c("br010_mod"))

# Define a function to process the data for each wave
process_wave_data <- function(data) {
  data %>%
    select(female, country, age, isced1997_r, eurod, bmi, smoking, br010_mod, br015_, chronic_mod, cogscore) %>%
    rename(
      gender = female,
      isced = isced1997_r,
      br010 = br010_mod,
      br015 = br015_,
      chronic = chronic_mod
    ) %>%
    mutate(
      smoking = if_else(smoking == 1, 1, 0),
      gender = as.factor(gender),
      isced = as.factor(isced),
      eurod = as.factor(eurod),
      smoking = as.factor(smoking),
      br010 = as.factor(br010),
      br015 = as.factor(br015),
      chronic = as.factor(chronic)
    )
}

# Apply the function to each wave
selected_data_wave_1 <- process_wave_data(data_wave_1_clean)
selected_data_wave_2 <- process_wave_data(data_wave_2_clean)
selected_data_wave_4 <- process_wave_data(data_wave_4_clean)
selected_data_wave_5 <- process_wave_data(data_wave_5_clean)

# Define a function to process the data for each wave and filter by country
process_and_filter_wave_data <- function(data) {
  data %>%
    filter(country %in% c(12, 16, 17, 11, 13, 14, 15, 18, 20, 23)) %>%
    mutate(
      country_tag = case_when(
        country %in% c(12, 16, 17) ~ "G7",
        TRUE ~ "Non_G7"
      )
    )
}

# Apply the function to each wave
selected_data_wave_1 <- process_and_filter_wave_data(selected_data_wave_1)
selected_data_wave_2 <- process_and_filter_wave_data(selected_data_wave_2)
selected_data_wave_4 <- process_and_filter_wave_data(selected_data_wave_4)
selected_data_wave_5 <- process_and_filter_wave_data(selected_data_wave_5)




# Define a function to create and display visualizations for each wave
create_wave_visualizations <- function(data, wave_number) {
  
  # Scatterplot: Age vs Cognitive Score
  p1 <- ggplot(data, aes(x = age, y = cogscore, color = country_tag)) +
    geom_point(aes(color = as.factor(country_tag)), alpha = 0.6) +
    stat_smooth(method = "lm", col = "black") + 
    facet_wrap(~ country, ncol = 4) +
    labs(title = paste("Cognitive Score vs Age by Country - Wave", wave_number),
         x = "Age",
         y = "Cognitive Score",
         color = "Country (G7 vs Non G7)") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Display the scatterplot (Age vs Cognitive Score)
  print(p1)
  
  # Scatterplot: BMI vs Cognitive Score
  p2 <- ggplot(data, aes(x = bmi, y = cogscore, color = country_tag)) +
    geom_point(aes(color = as.factor(country_tag)), alpha = 0.6) +
    stat_smooth(method = "lm", col = "black") + 
    facet_wrap(~ country, ncol = 4) +
    labs(title = paste("Cognitive Score vs BMI by Country - Wave", wave_number),
         x = "BMI",
         y = "Cognitive Score",
         color = "Country (G7 vs Non G7)") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Display the scatterplot (BMI vs Cognitive Score)
  print(p2)
  
  # Pairs plot for selected variables
  p3 <- ggpairs(data, mapping = aes(color = country_tag), 
                columns = c("gender", "age", "bmi", "isced", 'smoking', 'br010', 'chronic', 'cogscore'), 
                lower = "blank") + theme_bw() +
    labs(title = paste("Pairs Plot - Wave", wave_number))
  
  # Display the pairs plot
  print(p3)
  
  # Bivariate analysis for cognitive score
  p4 <- ggbivariate(data, outcome = "cogscore", mapping = aes(color = country_tag),
                    explanatory = c("gender", "isced", 'br010', 'chronic', 'smoking')) +
    labs(title = paste("Bivariate Analysis for Cognitive Score - Wave", wave_number))
  
  # Display the bivariate analysis plot
  print(p4)
}

# Apply the function to each wave and display the images in the R console
create_wave_visualizations(selected_data_wave_1, 1)
create_wave_visualizations(selected_data_wave_2, 2)
create_wave_visualizations(selected_data_wave_4, 4)
create_wave_visualizations(selected_data_wave_5, 5)



# Modeling part wave_1 
# G7 vs Non G7 countries
G7_wave1_data <- selected_data_wave_1 %>% filter(country_tag == 'G7') %>% select(gender, age, isced, eurod, bmi, 
                                                                                 br010, br015, smoking, chronic, cogscore)

nonG7_wave1_data <- selected_data_wave_1 %>% filter(country_tag == 'Non_G7') %>% select(gender, age, isced, eurod, bmi, 
                                                                                        br010, br015, smoking, chronic, cogscore)

library(recipes)
# Create the preprocessing recipe for G7
recipe_obj_G7 <- recipe(cogscore ~ ., data = G7_wave1_data) %>%
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
recipe_obj_G7_rf <- recipe(cogscore ~ ., data = G7_wave1_data) %>%
  step_center(age, bmi) %>%
  step_scale(age, bmi)



library(caret)
set.seed(3456)
trainIndex_G7 <- createDataPartition(G7_wave1_data$cogscore, p = .8, 
                                     list = FALSE, 
                                     times = 1)


G7_wave1_Train <- G7_wave1_data[ trainIndex_G7,]
G7_wave1_Test  <- G7_wave1_data[-trainIndex_G7,]

trainIndex_nonG7 <- createDataPartition(nonG7_wave1_data$cogscore, p = .8, 
                                        list = FALSE, 
                                        times = 1)

nonG7_wave1_Train <- nonG7_wave1_data[ trainIndex_nonG7,]
nonG7_wave1_Test  <- nonG7_wave1_data[-trainIndex_nonG7,]


                                                        





library(DALEX)
explainer_regr_lm_G7 <- DALEX::explain(lm_model_G7, label = "lm", 
                                       data = G7_wave1_Test, y = G7_wave1_Test$cogscore, 
                                       verbose = FALSE)

explainer_regr_lasso_G7 <- DALEX::explain(lasso_model_G7, label = "lasso", 
                                          data = G7_wave1_Test, y = G7_wave1_Test$cogscore, 
                                          verbose = FALSE)


explainer_svmR_G7 <- DALEX::explain(svmR_model_G7, label = "svmRadial", 
                                    data = G7_wave1_Test, y = G7_wave1_Test$cogscore, 
                                    verbose = FALSE)

explainer_regr_rf_G7 <- DALEX::explain(rf_model_G7, label = "rf", 
                                       data = G7_wave1_Test, y = G7_wave1_Test$cogscore, 
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
spec_obs <- G7_wave1_data %>% filter(gender == 1 & age == 70 & isced ==3 &
                                       eurod == 1 &
                                       br010 == 1 & br015 == 4 & 
                                       smoking ==0) %>% 
select(!c('cogscore'))

G7_wave1_data %>% filter(gender == 1 & age == 70 & isced ==3 &
                           eurod == 1 &
                           br010 == 1 & br015 == 4 & 
                           smoking ==0)

### Shapley values 

#linear model
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