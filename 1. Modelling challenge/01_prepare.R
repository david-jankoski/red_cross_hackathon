# Red Cross Hackathon - Amsterdam 20.05.2017
# Data Mission 
#
# Script with code used to inspect, clean
# and wrangle the data for the red cross hackathon ams.
#
# There's some modeling code at the end but still in 
# prototyping stage - not really error-proof.


# 0. Pre-requisites -----

library("caret")
library("tidyverse")
# devtools::install_github("tsellam/findviews")
library("findviews")

df <- read_csv("trainingset.csv")


# Eyeball 

# convert all chars to factors
# Seems not necessary since excluded from modeling.
#
# df <-  
#   df %>% 
#   map_if(is.character, as.factor) %>% 
#   as_tibble()

# predictive power of vars check
findviews::findviews_to_predict(
  "total_damage_houses", 
  # remove zip & target vars
  select(df, -c(1,2,3,4,5,6)  )
  )


# set up train df -------------

train <- 
  df %>%
  # throw away zip codes and target vars
  select(
    -c(#typhoon_name,
      admin_L3_code,
      admin_L2_code,
      admin_L3_name,
      comp_damage_houses,
      part_damage_houses)
  ) %>%
  
  # # convert response to perc
  #
  # ! WATCH OUT - this is actually wrong and it gives results > 100 
  # ! seems like the vars measured are not what we think they are...
  #
  # mutate(
  #   perc_damage_houses = round( total_damage_houses / n_households * 100 , 3  )
  #   ) %>% 
  # 
  # log all predictors 
  mutate_at(
    # except coords and roof info vars
    vars( 
      -c( typhoon_name,
          total_damage_houses,
          x_pos, y_pos, 
          contains("roof"),
          contains("wall")
      ) 
    ) ,
    funs(log)
  ) %>% 
  
  # make roof/wall vars bigger
  mutate_at( 
    vars(contains("roof"), contains("wall")),
    funs( . * 100)
    ) %>% 
  
  # replace NAs with 0s
  mutate_if(
    is.numeric,
    funs( ifelse( is.na(.) | !is.finite(.), 0 , . ) ) )

# Set out Haima as a hold-out hurricane test data
train$typhoon_name %>% table
holdout <- train %>% filter(typhoon_name == "Haima") 

train_stripped <- 
  train %>%
  filter(typhoon_name != "Haima") %>% 
  # remove old response
  select(-typhoon_name)


# set up test df -------

test_df <- read_csv("testset.csv")

# 
test <- 
  test_df %>%
  # throw away zip codes and target vars
  select(-c(typhoon_name,
            admin_L3_code,
            admin_L2_code,
            admin_L3_name,
            comp_damage_houses,
            part_damage_houses,
            total_damage_houses
  )
  ) %>%
  
  # log all predictors 
  mutate_at(
    # except coords and roof info vars
    vars( -c( x_pos, y_pos, contains("roof") ) ) ,
    funs(log)
    
  ) %>% 
  
  # replace NAs with 0s
  mutate_all( funs( ifelse( is.na(.) | !is.finite(.), 0 , . ) ) )


# first jab at model - throw everything at rf 

library("randomForest")

fit_rf <- 
  randomForest::randomForest(total_damage_houses ~ . ,
                             data = train_stripped,
                             ntree = 500,
                             mtry = 20,
                             importance = T)

fit_control <- 
  trainControl(
    # 10-fold CV
    method = "repeatedcv",
    number = 10,
    # repeated ten times
    repeats = 10)


# treebag model pkg ipred 
fit_treebag <- train(total_damage_houses ~ ., 
                     data = train_stripped, 
                     method = "treebag", 
                     trControl = fit_control, 
                     verbose = TRUE
)

# gbm model pkg gbm
fit_gbm <- train(total_damage_houses ~ ., 
                 data = train_stripped, 
                 method = "gbm", 
                 trControl = fit_control, 
                 verbose = FALSE
)



holdout <- 
  holdout %>% 
  mutate(
    
    # random forest prediction
    total_damage_houses_pred_rf = predict(fit_rf, 
                                          select(holdout, -c(typhoon_name, total_damage_houses))
                                          ),
    # treebag prediction
    total_damage_houses_pred_treebag = predict(fit_treebag, 
                                                select(holdout, -c(typhoon_name, total_damage_houses))
                                                ),
    # gbm prediction
    total_damage_houses_pred_gbm = predict(fit_gbm, 
                                            select(holdout, -c(typhoon_name, total_damage_houses))
                                            ),
    
    # differences between reality and predcited
    diff_rf = abs( total_damage_houses_pred_rf - total_damage_houses ),
    diff_treebag = abs( total_damage_houses_pred_treebag - total_damage_houses ),
    diff_gbm = abs( total_damage_houses_pred_gbm - total_damage_houses )
    )

differences_summarised <- 
  holdout %>% 
  summarise_at(
    vars(starts_with("diff_")),
    funs(mean)
    )
