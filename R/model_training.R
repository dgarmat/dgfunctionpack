

### model training functions ------

#' Buids a variable profile plot with optional cutoffs at a vector that differentiated categorical mappings
#'
#' @param x predictor variable
#' @param y response variable
#' @param xlabel what to call your predictor
#' @param ylabel what to call your response
#' @param cutoffs an optional vector of cut points where a numeric variable becomes a continuous variable
#'
#' @return ggplot2 object
#' @export
#' @importFrom dplyr summarise
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom scales percent_format
#' @importFrom purrr map_lgl
#' @import ggplot2
#'
#' @examples
#' plot_predict_var(mtcars, "wt", "mpg", "weight", "miles per gallon")
#' plot_predict_var(mtcars, "hp", "mpg", "horsepower", "miles per gallon", cutoffs = c(80, 135, 200))
plot_predict_var <- function(df, x, y = NA, xlabel = "default", ylabel = "default", cutoffs = NA, y_as_pct = FALSE, y_accuracy = 1, x_name_exact = TRUE){
  ## see response by average predictor ##
  
  # handle backward compatability
  if((length(df) > 1) & (length(x) > 1)){
    warning("attempting backward compatibility, prefer start with df")
    # reset the vars
    if((is.numeric(ylabel)) & (is.na(cutoffs))) {
      cutoffs <- ylabel
    }
    if(xlabel == "default"){
      ylabel <- "y"
    } else{
      ylabel <- xlabel
    }    
    if(is.na(y)){
      xlabel <- "x"
    } else{
      xlabel <- y
    }
    y <- x
    x <- df
    df <- data.frame(x = x, y = y)
    
    y_1 <-  summarise(group_by(df, x), mean(y), n())
    p <- ggplot(y_1, aes(x, y = `mean(y)`, size = `n()`)) +
      geom_point(shape = 1) +
      geom_smooth(method = "loess") +
      theme(legend.position = "none")
    
  } else if(is.data.frame(df) & is.character(x) & is.character(y)){
    if(!x_name_exact) {
      # if no exact match
      if(sum(colnames(df) %in% x) == 0){
        # search for a match substring
        x <- colnames(df)[map_lgl(colnames(df), grepl, x = x)][1]
      }
    }
    df <- df[, c(y, x)]
    y_1 <-  summarise_all(group_by_(df, x),  funs(mean, n()))
    p <- ggplot(y_1, aes_string(
      x = x, 
      y = "mean", 
      size = "n")) +
      geom_point(shape = 1) +
      geom_smooth(method = "loess") +
      theme(legend.position = "none")
    
    # if df, default x is variable name now
    if(xlabel == "default"){
      xlabel <- x
    }
    if(ylabel == "default"){
      ylabel <- y
    }
  } else{
    stop("expecting data frame, x column name, and y column name")
  }

  if(sum(!is.na(cutoffs) > 0)){
    p <- p + geom_vline(xintercept = cutoffs, linetype = "3313") +
      labs(title = paste0("Variable Profile: Average ",
                          ylabel, " by ", 
                          xlabel, 
                          "\nCategory Groups Shown with Vertical Lines"), 
           x = xlabel, 
           y = ylabel) 
  } else if(is.na(cutoffs)){
    p <- p + labs(title = paste0("Variable Profile: Average ",
                            ylabel, " by ", 
                            xlabel), 
             x = xlabel, 
             y = ylabel)
  }
  
  if(y_as_pct) {
    p <- p + scale_y_continuous(labels = percent_format(accuracy = y_accuracy))
  }
  
  p
}


#' Map a set of elastic net logistic regression coeficients 1-1 to points where 100 is the chosen threshold
#'
#' @param objModel an elastic net logistic model fit using caret
#' @param thresholdStrat the percentage probability to cut off as positive
#' @param pointsStrat the threshold in points to map the cut off to
#'
#' @return
#' @export
#' @importFrom dplyr data_frame
#'
#' @examples
coefs_to_points <- function(objModel, thresholdStrat = 0.40, pointsStrat = 100){
  divisorStrat <- -log((1 / thresholdStrat) - 1) - coef(objModel$finalModel, objModel$bestTune$lambda)[1] # this is the GLM standard
  constantMultiple <- pointsStrat / divisorStrat

  # round(coef(objModel$finalModel, objModel$bestTune$lambda) * constantMultiple, 0)[-1,]

  OutputVars <- data_frame("Variable" = rownames(coef(objModel$finalModel, objModel$bestTune$lambda)),
                           "Points" = as.vector(round(coef(objModel$finalModel, objModel$bestTune$lambda) * constantMultiple, 0)))


  # remove intercept
  OutputVars <- OutputVars[-1, ] # remove intercept
  OutputVars
}



#' Map a generic logisitc model's coeficients 1-1 to points where 100 is stratification, handling relevelling of factors
#'
#' @param model_coef 
#' @param thresholdStrat 
#' @param pointsStrat 
#'
#' @return
#' @export
#' @importFrom dplyr data_frame
#'
#' @examples
translate_coefs_to_points <- function(model_coef, thresholdStrat = 0.40, pointsStrat = 100){
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
  constantMultiple <- pointsStrat / divisorStrat

  # round(coef(objModel$finalModel, objModel$bestTune$lambda) * constantMultiple, 0)[-1,]

  OutputVars <- data_frame("Variable" = rownames(model_coef),
                           "Points" = as.vector(round(model_coef[,1] * constantMultiple, 0)))

  # remove intercept
  OutputVars <- OutputVars[-1, ] # remove intercept
  OutputVars
}


#' Train a ridge regression on a set of variables and thranslate coeficients into points
#'
#' @param x 
#' @param vars 
#' @param resp 
#' @param cont 
#' @param thresholdStrat 
#'
#' @return
#' @export
#' @importFrom caret train
#'
#' @examples
train_model <- function(x = model_input, vars = input_vars, resp = "response_label", cont = objControl, 
                        thresholdStrat = 0.40, pointsStrat = 100){
  objModel <- train(x[, vars], x[, resp], method='glmnet', metric = "ROC",
                    # set alpha = 0 for ridge regression since already ran selection to find significant variables
                    tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.5, by = 0.001)),
                    trControl = cont)
  # evaluate the model
  # plot(objModel)
  # objModel$bestTune$lambda
  # coef(objModel$finalModel, objModel$bestTune$lambda)

  OutputVars <- translate_coefs_to_points(model_coef = coef(objModel$finalModel, objModel$bestTune$lambda),
                                          thresholdStrat = thresholdStrat,
                                          pointsStrat = pointsStrat)
  OutputVars
}


#' Take a dataframe and a set of variables and fit three logitic regressions using forward selection
#'
#' @param datfr 
#' @param resp_var 
#' @param pred_vars 
#' @param int_var 
#' @param run_forward_selection 
#' @param trace_fs 
#' @param steps_fs 
#'
#' @return
#' @export
#' @importFrom MASS stepAIC
#'
#' @examples
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


