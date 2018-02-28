#wokring with H2o now

h2o.init()
h2o.no_progress()


#making data compatible to H2o

train_h2o <- as.h2o(train_df)
valid_h2o <- as.h2o(valid_df)
test_h2o  <- as.h2o(test_df)

#making model for automatic machine learning

# Automatic Machine Learning
y <- "went_on_backorder"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 45
)

#executing the function
automl_leader <- automl_models_h2o@leader



#tring to pridict data
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
as.tibble(pred_h2o)



#after modelling checking for prediction

pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)
as.tibble(pred_h2o)

