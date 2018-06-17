# Grid Search & CV ----

# AWESOME resource:  https://blog.h2o.ai/2016/06/h2o-gbm-tuning-tutorial-for-r/

deeplearning_h2o <- h2o.loadModel("04_Modeling/h2o_models/DeepLearning_0_AutoML_20180522_084030")

h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))#AUC 0.8624698

# do CV even with grid search
# epochs can lead to overfitting
# grid_id simplycopied from the variable in this function - makes it easier, not required, can be anything
deeplearning_grid_01 <-  h2o.grid(
     algorithm = "deeplearning",
     grid_id = "deeplearning_grid_01",
     
     # from h2o.deeplearning()
     x = x,
     y = y,
     training_frame = train_h2o,
     validation_frame = valid_h2o,
     nfolds = 5, 
     
     hyper_params = list(
          hidden = list(c(10, 10, 10), c(50, 20, 10), c(20, 20, 20)),
          epochs = c(10, 50, 100)
     )
)

deeplearning_grid_01

h2o.getGrid("deeplearning_grid_01", sort_by = "auc", decreasing = T)

deeplearning_grid_01_model_15 <- h2o.getModel("deeplearning_grid_01_model_15")

deeplearning_grid_01_model_15 %>% h2o.auc(train = T, valid = T, xval = T)

deeplearning_grid_01_model_15 %>% 
     h2o.performance(newdata = as.h2o(test_tbl))