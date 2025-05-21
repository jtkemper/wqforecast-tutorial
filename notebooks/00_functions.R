## ----lgbm_selector------------------------------------------------------------------------------------------------------

lgbm_selector <- function(constituent_df,
                          selector = "shap"
                          
                          ) {
  
    
  

      model_stats_lgbm <- list()
      
      var_imp <- list()
      
      vars <- list()
      
      all_model_stats <- list()
      
      shap_values <- list()
      
      shap_value_summary <- list()
      
      model_returns <- list()
      
      predicted_observed_ts <- list()
      
      
      ### Declare the predictors
      
        
      predictors <- constituent_df
  
      
      for(i in 1:ncol(predictors)) {
        
        
        
          run <- paste0("run", i)
        
                    
          cat(crayon::yellow("\nModeling Run", i, "\n"))


                
                  for(j in c(2003, 2008, 2013, 2018)) {
                
              
                        test_years <- seq(j-4, j, 1)
                        
                        train_years <- seq(1990, j-5, 1)
                        
                        k <- (2018 - j)/5
                  
                        k <- ifelse(k == 0, 1, k+1)
                        
                        
                        
                        cat(crayon::cyan("\nTraining ", first(train_years), ":",
                                           last(train_years),
                                           "\n")) 
                        cat(crayon::magenta("\nTesting ", first(test_years), ":",
                                           last(test_years),
                                           "\n")) 
                        
                            
                        
                        #### Training data
                            predictor_dataset_train <- predictors %>%
                            filter(wateryear %in% train_years)
                            
                            if(nrow(predictor_dataset_train) < 1) next
                     
                          ### Now subset the testing data to that same subset
                          predictor_dataset_test <- predictors %>%
                            filter(wateryear %in% test_years) %>%
                            dplyr::select(colnames(predictor_dataset_train))
                          
                          #######################
                        
                          ### Declare the predictor and response variables 
                        preds <- data.matrix(predictor_dataset_train %>%
                                                    dplyr::select(!c(log_conc,
                                                                     wateryear,
                                                                     date, 
                                                                     tributary)))
                        
                        response <- predictor_dataset_train$log_conc
                        
                        ### Set up the environment - 
                        #### this is just preparing the dataset API to be used by lightgbm. 
                        #### This is our training data
                        train_lgbm <- lgb.Dataset(preds, 
                                                         label = response,

                                                         ) 
                        
                        ### Declare the test data
                        test_lgbm <- data.matrix(predictor_dataset_test %>%
                                                              dplyr::select(!c(log_conc,
                                                                     wateryear,
                                                                     date, 
                                                                     tributary)))
                    

                        
                        #########
                    
                      ### Declare the hyperparameters 
                      ### These are just default for now
                      
                      hyperparams <- list(objective = "regression",
                                          num_leaves = 31L,
                                          learning_rate = 0.1,
                                          min_data_in_leaf = 20L,
                                          num_threads = 10L)

                        
                        ### Train the model
                        
                        set.seed(913)
                        
                        nutrient_model_lgbm <- lgb.train(hyperparams,
                                                          data = train_lgbm,
                                                          verbose = 1L,
                                                          nrounds = 100L
                                                         )
                        
                        ### Get model fits on training data
                        nutrient_fits <- predict(nutrient_model_lgbm, 
                                                       data = preds) %>%
                          as_tibble() %>% rename(log_predicted_conc = 1)
                        
                        
                        ### Predict with the model on test data
                        nutrient_predicted <- predict(nutrient_model_lgbm, 
                                                       data = test_lgbm) %>%
                          as_tibble() %>% rename(log_predicted_conc = 1)
                        
                        ### Calculate the SHAP values
                        shap_values[[k]] <- SHAPforxgboost::shap.prep(xgb_model = 
                                                                        nutrient_model_lgbm, 
                                                                   X_train = test_lgbm)
                        
                        shap_value_summary[[j]] <- shap_values[[k]] %>%
                              as_tibble() %>%
                              dplyr::group_by(variable) %>%
                              summarise(sd_shap = sd(value),
                                        feature_importance = mean_value[1]) %>%
                              mutate(sd_plus_imp = sd_shap + feature_importance)
                        
                        
                        ### Bind predictions on test data
                        ### to observatios of test data
                        predicted_observed <- bind_cols(predictor_dataset_test %>%
                                                                dplyr::rename(log_observed_conc =
                                                                                log_conc),
                                                                       nutrient_predicted) 
                        
                        ### Now convert back to non-log
                        predicted_observed_resc <- predicted_observed %>%
                          mutate(observed_conc = 10^(log_observed_conc),
                                 predicted_conc = 10^(log_predicted_conc))
                        
                        ### And store
                                                
                        predicted_observed_ts[[j]] <- predicted_observed_resc %>%
                          dplyr::select(tributary, date, predicted_conc, observed_conc)
                        
                    
                        #### Evaluate
                        #### For each watershed

                        model_stats_simple <- predicted_observed_resc %>%
                          ungroup() %>%
                          dplyr::group_by(tributary) %>%
                          summarise(mae = hydroGOF::mae(predicted_conc, observed_conc),
                                    nse = hydroGOF::NSE(predicted_conc, observed_conc),
                                    kge = hydroGOF::KGE(predicted_conc, 
                                                        observed_conc),
                                    pbias = hydroGOF::pbias(predicted_conc,
                                                            observed_conc)) %>%
                          dplyr::ungroup() 

                        #### Median performance across all 
                        #### For each [j] run
                        
                        model_stats_lgbm[[j]] <- model_stats_simple %>%
                          reframe(across(where(is.numeric),
                                         list(median = ~median(.x)),
                                         .names = "{.fn}_{.col}"
                                         ))
                        
                        ### Calculate variable importance
                        
                        var_imp[[j]] <- lgb.importance(nutrient_model_lgbm , 
                                                                         percentage = TRUE)
                        
                        
                  
            #####################################################################
          
          
          }
          
              ### Store variable importance and shap values across
              ### All CV runs
            
              all_var_imp <- bind_rows(var_imp) 
              
              ### Summarise SHAP values and var imp. as the mean values for each feature
              ### Across all runs
              
              all_shap_value_summary <- bind_rows(shap_value_summary) %>%
                dplyr::group_by(variable) %>%
                summarise(mean_sd_plus_imp = mean(sd_plus_imp))
          
              all_model_stats[[i]] <- bind_rows(model_stats_lgbm) %>%
                mutate(model = i)
          
              summary_var_imp <- all_var_imp %>%
                dplyr::group_by(Feature) %>%
                summarise(mean_Gain = mean(Gain)) %>%
                dplyr::ungroup()
              
              #### Using SHAP values as the selector
          
            if(selector == "shap") {
              
                ### Sort the features by mean SHAP value
                ### And remove the one with the lowest mean SHAP value
              
                one_removed_predictors <- all_shap_value_summary %>%
                  dplyr::ungroup() %>%
                  arrange(desc(mean_sd_plus_imp)) %>%
                  dplyr::slice(-nrow(.))
                
                ### Store the variable list
                      
                vars[[i]] <- all_shap_value_summary %>%
                  ungroup() %>%
                  mutate(model = i) %>%
                  rename(Feature = variable)
          
                ### See how many we have left
                var_count <- length(one_removed_predictors$variable)
                
                if(var_count == 0) break 
                
                
                  ### Update variable list
                predictors <- predictors %>%
                  dplyr::select(one_removed_predictors$variable,
                                log_conc,
                                wateryear,
                                date, 
                                tributary)
          
              
            } else if(selector == "gain"){
              
                ### Sort the features by mean feature importance value
                ### And remove the one with the lowest mean value
              
                one_removed_predictors <- summary_var_imp %>%
                  dplyr::ungroup() %>%
                  arrange(desc(mean_Gain)) %>%
                  dplyr::slice(-nrow(.))
                
                ### Store the variables
                vars[[i]] <- summary_var_imp %>%
                  ungroup() %>%
                  mutate(model = i)
          
                ### See how many we have left
                var_count <- length(one_removed_predictors$Feature)
                
                if(var_count == 0) break 
          
              
                ### Update variable list
              predictors <- predictors %>%
                dplyr::select(one_removed_predictors$Feature,
                              log_conc,
                              wateryear,
                              date, 
                              tributary)
              
                }
          
      }
          
        
      ### Bind model performance statistics from all runs 
      
      all_all_model_stats <- bind_rows(all_model_stats)
      
      all_var_imp <- bind_rows(vars)
      
      
      #### Save outputs
      
 
      
      model_returns[[1]] <- all_var_imp
      
      
      
     
      summary_model_stats <- all_all_model_stats %>%
        ungroup()
      
      #### Store a list of features included in each [i] model
      #### Abd join the summary statistics to each model
      
      collapsed_models <- all_var_imp %>%
        group_by(model) %>%
        arrange(Feature, .by_group = TRUE) %>%
        summarise(all_vars = paste(Feature, collapse = ",")) %>%
        full_join(., summary_model_stats, 
                  by = "model")
        
        ### Calculate the average performance of the median from each [j] CV run
        ### So this is simply how a given model does, on average, for the entire LC basin 
        
        model_stats <- collapsed_models %>%
              dplyr::group_by(model, all_vars) %>%
                  summarise(#n = n(),
                            mean_kge = mean(median_kge, na.rm = TRUE),
                                  mean_nse = mean(median_nse, na.rm = TRUE),
                                  mean_mae = mean(median_mae, na.rm = TRUE),
                                  mean_pbias = mean(median_pbias, na.rm = TRUE),
                            sd_kge = sd(median_kge),
                            sd_nse = sd(median_nse),
                            sd_mae = sd(median_mae),
                            sd_pbias = sd(median_pbias),
                            ) 
            
              model_returns[[2]] <- model_stats
              
        
        
      
      
      return(model_returns)

}




## ----plot_stats---------------------------------------------------------------------------------------------------------

plot_stats <- function(model_stats_df) {
  
  pos_mods <- nrow(model_stats_df)
  
  if(pos_mods <25){by=1} else{by=5}
  
  model_stats_df %>%
    dplyr::select(model, 
                  mean_kge, mean_nse, mean_mae, mean_pbias,
                  sd_kge,  sd_nse,  sd_mae, sd_pbias) %>%
    pivot_longer(cols = -model, names_to = c(".value", "metric"), names_sep = "_") %>%
    ggplot() +
      geom_line(aes(x = model, y = mean, 
                    color = metric)
                ) +
      geom_errorbar(aes(x = model, 
                        ymin = mean-sd, ymax = mean + sd,
                        color = metric),
                    alpha = 0.4
                    ) +
      geom_point(aes(x = model, y = mean, color = metric),
                shape = 19, size = 1) +
      scale_color_brewer(palette = "Set1",
                         guide = "none") + 
      scale_x_continuous(breaks = seq(0,pos_mods,by),
                         minor_breaks = seq(0,pos_mods,1)) + 
      labs(y = element_blank(),
           x = "Model Iteration") + 
      theme_few() +
      theme(panel.grid = element_line(color = "gray90"),
            legend.position = "bottom",
            strip.placement = "outside") +
      facet_wrap(~metric, scales = "free", ncol = 1,
                 strip.position = "left")

  
}


## ----lgbm_runner--------------------------------------------------------------------------------------------------------

lgbm_runner <- function(train_df = NULL, 
                        test_df = NULL,
                        chosen_mod = NULL,
                        is_tuned = FALSE,
                        tuned_params = NULL,
                        save = FALSE){
  
  
    ########## Model setup #########################################################
  
    #### Get the type of data we have
  
    file_string <- str_extract(deparse(substitute(test_df)), "(?<=_)[^_]+(?=_)")
    
    #### Trim down to selected model
    #### And order alphabeticaly - important for using the model in the future
  
    train_df <- train_df %>% 
      dplyr::select(c(chosen_mod$Feature, log_conc, date, tributary)) %>%
      dplyr::select(order(colnames(.)))
    
    test_df <- test_df %>% 
      dplyr::select(c(chosen_mod$Feature, log_conc, date, tributary)) %>%
      dplyr::select(order(colnames(.)))

    
    ### Declare the predictor and response variables 
    ### Make sure to exclude variables we left in there 
    ### For interpretability
    
    preds <- data.matrix(train_df %>%
                           dplyr::select(!c(log_conc, date, tributary)))
                                                                        
                            
    response <- train_df$log_conc
                            
    ### Set up the environment - this is just preparing the dataset API for use by lightgbm.
    #### This is our training data
    train_lgbm <- lgb.Dataset(preds, label = response)
                            
    #### Declare the test data
    test_lgbm <- data.matrix(test_df %>%
                           dplyr::select(!c(log_conc, date, tributary)))
                        
    ### Declare the hyperparameters 
                    
    if(is_tuned == FALSE ) { 
      
      hyperparams <- list(objective = "regression",
                          num_leaves = 31L,
                          learning_rate = 0.1,
                          min_data_in_leaf = 20L,
                          num_threads = 10L)
      } else if(is_tuned == TRUE ) {
        
        hyperparams <- list(objective = "regression",
                            num_leaves = tuned_params$num_leaves,
                            min_data_in_leaf = tuned_params$min_n,
                            bagging_fraction = tuned_params$sample_size,
                            bagging_freq = 1,
                            num_iterations = tuned_params$trees,
                            max_depth = tuned_params$tree_depth
                            )
                        }
                        

    
    ################################################################################
    ################################################################################
    
    ### Now, let's do some actual modeling stuff
    
    ### Inform what we are running:
    
    cat(crayon::cyan("Running model with Features:"),
        crayon::green(chosen_mod$Feature),
        crayon::green("\n"))

    
    #### Train the model
                            
    set.seed(913)
                            
    model_lgbm <- lgb.train(hyperparams,
                                   data = train_lgbm,
                                   verbose = 1L,
                                   nrounds = 100L)
    
    ### Save the model
    
    if(save == TRUE){
      
      
      lgb.save(model_lgbm, filename =
                                here("input-data", "models",
                                     paste0("lgbm_",
                                            file_string, 
                                            ".txt")))
      
    }
                            
    ### Predict with the model on test data
    
    predicted <- predict(model_lgbm, 
                                      data = test_lgbm) %>%
      as_tibble() %>% 
      rename(log_predicted_conc = 1)
                            
                            
    ### Bind predictions on test data to observations
    
    pred_obs <- bind_cols(test_df %>% 
                                      dplyr::rename(log_observed_conc = log_conc),
                                    predicted) 
    
    ### Now use the scalars to convert back to linear scale
    
    # rescale_pred_obs <- pred_obs %>%
    #   mutate(observed_conc = 10^(log_observed_conc*conc_scalars$sd + conc_scalars$mean),
    #          predicted_conc = 10^(log_predicted_conc*conc_scalars$sd + conc_scalars_test$mean))
    
     rescale_pred_obs <- pred_obs %>%
       mutate(observed_conc = 10^(log_observed_conc),
             predicted_conc = 10^(log_predicted_conc))
    
                        
    ### Evaluate - we are going to use multiple error metrics
    
    #### For each watershed
    
    model_stats_each <- rescale_pred_obs %>%
      ungroup() %>%
      dplyr::group_by(tributary) %>%
      summarise(mae = hydroGOF::mae(predicted_conc, observed_conc),
                nse = hydroGOF::NSE(predicted_conc, observed_conc),
                kge = hydroGOF::KGE(predicted_conc, 
                                    observed_conc),
                pbias = hydroGOF::pbias(predicted_conc,
                                        observed_conc)) %>%
      dplyr::ungroup() 
    
    #### Median across all 
    
    model_stats_summary <- model_stats_each %>%
      reframe(across(where(is.numeric),
                     list(median = ~median(.x)),
                     .names = "{.fn}_{.col}"
                     ))
    
    
    ##### Return stuff
    
    test_stats <- list()
    
    test_stats[[1]] <- model_stats_each
    
    test_stats[[2]] <- model_stats_summary
    
    test_stats[[3]] <- rescale_pred_obs %>%
      dplyr::select(tributary, date, observed_conc, predicted_conc)
    
    return(test_stats)

  
  
}

