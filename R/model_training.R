

### model training functions ------
plot_predict_var <- function(x, y, xlabel = "x", ylabel = "y", cutoffs = NA){
  ## see response by average predictor ##
  df <- data.frame(x = x, y = y)
  y_1 <-  summarise(group_by(df, x), mean(y))
  y_2 <- summarise(group_by(df, x), n())
  y_1 <- y_1 %>% left_join(y_2)
  p <- ggplot(y_1, aes(x, y = `mean(y)`, size = `n()`)) +
    geom_point(shape = 1) +
    geom_smooth(method = "loess")
  if(!is.na(cutoffs)){
    p + geom_vline(xintercept = cutoffs, linetype = "3313") +
      labs(title = "Variable Profile: Average Response (y) by Predictor (x) \nCategory Groups Shown with Vertical Lines", x = xlabel, y = ylabel)
  } else if(is.na(cutoffs)){
    p + labs(title = "Variable Profile: Average Response (y) by Predictor (x)", x = xlabel, y = ylabel)
  }
}


coefs_to_points <- function(){
  divisorStrat <- -log((1 / thresholdStrat) - 1) - coef(objModel$finalModel, objModel$bestTune$lambda)[1] # this is the GLM standard
  constantMultiple <- 100 / divisorStrat

  # round(coef(objModel$finalModel, objModel$bestTune$lambda) * constantMultiple, 0)[-1,]

  OutputVars <- data_frame("Variable" = rownames(coef(objModel$finalModel, objModel$bestTune$lambda)),
                           "Points" = as.vector(round(coef(objModel$finalModel, objModel$bestTune$lambda) * constantMultiple, 0)))


  # remove intercept
  OutputVars <- OutputVars[-1, ] # remove intercept
  OutputVars
}



translate_coefs_to_points <- function(model_coef, thresholdStrat = 0.40){
  # handle other kinds of inputs
  if(sum(class(model_coef) %in% "glm") > 0){
    model_coef <- coef(summary(model_coef))
  }
  if(sum(class(model_coef) %in% "summary.glm") > 0){
    print(model_coef)
    model_coef <- coef(model_coef)
    print(model_coef)
  }

  # handle negative coefs
  if(sum(model_coef[-1,1] < 0) > 0){
    warning(paste0("There are ", sum(model_coef[-1, 1] < 0),
                   " negative coeficients, which may need to be removed by reordering factors for reasonable Points outputs"))
  }
  divisorStrat <- -log((1 / thresholdStrat) - 1) - model_coef[1] # this is the GLM standard
  if(divisorStrat < 0){
    warning(paste0("your coeficient multiplier is negative, you need to set a higher thresholdStrat than ", thresholdStrat))
  }
  divisorStrat <- -log((1 / thresholdStrat) - 1) - min(model_coef[1], 0) # this attempts to handle positive intercepts
  constantMultiple <- 100 / divisorStrat

  # round(coef(objModel$finalModel, objModel$bestTune$lambda) * constantMultiple, 0)[-1,]

  OutputVars <- data_frame("Variable" = rownames(model_coef),
                           "Points" = as.vector(round(model_coef[,1] * constantMultiple, 0)))

  # remove intercept
  OutputVars <- OutputVars[-1, ] # remove intercept
  OutputVars
}


train_model <- function(x = model_input, vars = input_vars, resp = "response_label", cont = objControl, thresholdStrat = 0.40){
  objModel <- train(x[, vars], x[, resp], method='glmnet', metric = "ROC",
                    # set alpha = 0 for ridge regression, since already ran selection to find significant variables
                    tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.5, by = 0.001)),
                    trControl = cont)
  # evaluate the model
  # plot(objModel)
  # objModel$bestTune$lambda
  # coef(objModel$finalModel, objModel$bestTune$lambda)

  OutputVars <- translate_coefs_to_points(model_coef = coef(objModel$finalModel, objModel$bestTune$lambda),
                                          thresholdStrat = thresholdStrat)
  OutputVars
}


create_int_lms <- function(datfr, resp_var = "response", pred_vars = NA, int_var = NA,
                           run_forward_selection = FALSE, trace_fs = FALSE, steps_fs = 5){
  # takes a dataframe and a set of variables and fits three logistics regressions,
  # 1. small  = no additional interaction terms
  # 2. medium = one specified additional interaction term
  # 3. large  = all pairwise interaction terms
  # it then either returns the three models, or if run_forward_selection = TRUE,
  #  it will perform forward selection on small to medium and small to large and return
  #  those results as well as the three models
  # -i would be good to add other variable selection options like L1

  # check datfr is a data frame
  if(!(class(datfr) %in% c("data.frame", "tbl_df", "tbl") )){stop("data frame came back as not a data frame")}
  if(nrow(datfr) == 0){stop("data frame has no rows of data")}

  if(typeof(resp_var) != "character") {stop(paste0("resp_var given not a character"))}
  # if no pred vars given, assume all but the response
  if(is.na(pred_vars)) {
    pred_vars <- names(datfr)[which(names(datfr) != resp_var)]
  }
  # check vars are strings, if not, turn them into strings
  if(typeof(pred_vars) != "character") {stop(paste0("one of pred_vars given is not a character vector"))}
  if(!is.na(int_var)){
    if(typeof(int_var) != "character") {stop(paste0("int_var is not a character"))}
  }

  # turn pred vars into a formula
  glm_small_formula <-
    paste0(resp_var, " ~ ",
           paste0(pred_vars, collapse = " + ")
    )

  glm_medium_formula <-
    paste0(resp_var, " ~ ",
           paste0(pred_vars, collapse = " + "),
           " + ",
           paste0(paste0(pred_vars, ":", int_var), collapse = " + ")
    )

  glm_large_formula <-
    paste0(resp_var, " ~ .^2")


  which_vars_used <- logical(length(datfr))
  for(pred_var in c(pred_vars, resp_var)){
    which_vars_used <-
      which_vars_used + grepl(pred_var, names(datfr))
  }
  which_vars_used <- as.logical(which_vars_used)

  # check for negative coefs and adjust factors accordingly
  glm_small  <- glm(as.formula(glm_small_formula),  data = datfr[ , which_vars_used])
  nmbr_neg_coefs <- sum(coef(summary(glm_small))[-1,1] < 0)
  current_attempt <- 1
  max_attempts <- 10
  while(nmbr_neg_coefs > 0 & current_attempt < max_attempts){
    print(paste0("There are ", nmbr_neg_coefs,
                 " negative coeficients, which need to be removed by reordering factors for reasonable Points outputs"))
    print(paste0("Attempt ", current_attempt, " of ", max_attempts," to fix the negative coeficient issue..."))

    # which predvar is it?
    for(pv in pred_vars){
      if(grepl(substr(names(which.min(coef(summary(glm_small))[-1,1])), 1, length(pv)), pv)){
        fixvarlevel <- pv
      }
    }
    newref <- substr(names(which.min(coef(summary(glm_small))[-1,1])), nchar(fixvarlevel) + 1, 1000)
    datfr[[fixvarlevel]] <- factor(datfr[[fixvarlevel]])
    datfr[[fixvarlevel]] <- relevel(datfr[[fixvarlevel]], ref = newref)

    # then retry
    glm_small  <- glm(as.formula(glm_small_formula),  data = datfr[ , which_vars_used])
    nmbr_neg_coefs <- sum(coef(summary(glm_small))[-1,1] < 0)

    current_attempt <- current_attempt + 1
  }


  glm_medium <- glm(as.formula(glm_medium_formula), data = datfr)
  glm_large  <- glm(as.formula(glm_large_formula),  data = datfr[ , which_vars_used])


  data_export <- list("small" = glm_small, "medium" = glm_medium, "large" = glm_large)


  if(run_forward_selection){
    step_glm <-  MASS::stepAIC(glm_small, scope = list(lower = glm_small, upper = glm_medium),
                               direction = "forward", trace = trace_fs, steps = steps_fs)
    step_glm_2 <-  MASS::stepAIC(glm_small, scope = list(lower = glm_small, upper = glm_large),
                                 direction = "forward", trace = trace_fs, steps = steps_fs)
    # append to end of list
    data_export[4:5] <- list("small_to_medium" = step_glm, "small_to_large" = step_glm_2)
    names(data_export[4:5]) <- c("small_to_medium", "small_to_large") # seems to be ignoring the names here
  }

  data_export

}


