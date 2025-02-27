#' @name make_cate
#' @title Function to calculate the Causal Treatment Effect
#' @description Predicting conditional average treatment effect (CATE) on a new policy based on the training over an old policy
#'
#' @references
#' - Athey, S., and Wager S. 2021. Policy Learning with Observational Data, Econometrica, 89, 1, 133–161.
#' - Cerulli, G. 2021. Improving econometric prediction by machine learning, Applied Economics Letters, 28, 16, 1419-1425.
#' - Cerulli, G. 2022. Optimal treatment assignment of a threshold-based policy: empirical protocol and related issues, Applied Economics Letters, DOI: 10.1080/13504851.2022.2032577.
#' - Gareth, J., Witten, D., Hastie, D.T., Tibshirani, R. 2013. An Introduction to Statistical Learning : with Applications in R. New York, Springer.
#' - Kitagawa, T., and A. Tetenov. 2018. Who Should Be Treated? Empirical Welfare Maximization Methods for Treatment Choice, Econometrica, 86, 2, 591–616.
#'
#' @importFrom dplyr %>% filter
#' @importFrom randomForest randomForest
#' @importFrom stats glm predict
#' @importFrom stats as.formula gaussian
#'
#' @param model A `model` object used for estimation.
#' @param train_data The training dataset.
#' @param test_data The test dataset.
#' @param w Set the treatment variable.
#' @param x set Independent variables for the model.
#' @param y Set the outcome variable.
#' @param family The family type for the model (e.g., 'binomial').
#' @param ntree Number of trees for the Random Forest model.
#' @param mtry Number of variables to consider at each tree split in the Random Forest model.
#' @param verbose Set TRUE to print the output on the console.
#'
#' @return An object containing the estimated causal treatment effect results.
#'
#' @export


##### Make Cate ################################################################
##### Predicting CATE for a binary policy ######################################

make_cate <- function(model,train_data,test_data,w,x,y,family=gaussian(),ntree=100,mtry=2,verbose=TRUE) {

  obs_train<-dim(train_data)[1]
  obs_test<-dim(test_data)[1]

  #train_set and test_set subset into treated and untreated
  train_W1 <- train_data %>% filter(!!sym(w)==1)    #treated
  train_W0 <- train_data %>% filter(!!sym(w)==0)    #untreated
  test_W1 <- test_data %>% filter(!!sym(w)==1)      #treated
  test_W0 <- test_data %>% filter(!!sym(w)==0)      #untreated

  # models formula
  fmla <- as.formula(paste(y, "~", paste(x, collapse= "+")))

  # REGRESSION ADJUSTMENT #######################
  if (model == "glm") {
    #Calculate the treatment effect with glm on train_set
    lm1<-glm(fmla,data=train_W1, family = family)
    y1_x <-predict(lm1,type="response",train_data)

    lm0<-glm(fmla,data=train_W0, family = family)
    y0_x <-predict(lm0,type="response",train_data)
  }

  if (model == "rf") {
    #Calculate the treatment effect with random forest on train_set
    lm1<-randomForest(fmla,data=train_W1, ntree = ntree, mtry = mtry, importance = FALSE)
    y1_x <-predict(lm1,type="response",train_data)

    lm0<-randomForest(fmla,data=train_W0, ntree = ntree, mtry = mtry, importance = FALSE)
    y0_x <-predict(lm0,type="response",train_data)
  }

    train_data$my_cate<-y1_x - y0_x

    # effects calculated on the train_set
    DIM<- mean(eval(parse(text=paste0("train_W1$",y))))-mean(eval(parse(text=paste0("train_W0$",y))))
    ATE<- mean (y1_x - y0_x)  #AVERAGE TREATEMENT EFFECT
    ATENT <- mean(y0_x)       #AVERAGE TREATEMENT EFFECT ON NON TREATED
    ATET <- mean(y1_x)        #AVERAGE TREATEMENT EFFECT ON TREATED

    # results output for the train dataset  #######################
    # Output block
    output <- paste(
      " --------------------------------------------------\n",
      "- Treatment-effects estimation: OLD POLICY       -\n",
      "--------------------------------------------------\n",
      paste("Number of obs =", obs_train), "\n",
      paste("Estimator = regression adjustment"), "\n",
      paste("Outcome model = linear"), "\n",
      paste("DIM =", round(DIM, 7)), "\n",
      paste("ATE =", round(ATE, 7)), "\n",
      paste("ATET =", round(ATET, 7)), "\n",
      paste("ATENT =", round(ATENT, 7))
    )

    # Output print
    if(verbose){
      cat(output, "\n")
    }

    if (model == "glm") {
    #Calculate the treatment effect with glm on test_set
    y1_x <-predict(lm1,type="response",test_data)
    y0_x <-predict(lm0,type="response",test_data)
    }

    if (model == "rf") {
      #Calculate the treatment effect with random forest on train_set
      y1_x <-predict(lm1,type="response",test_data)
      y0_x <-predict(lm0,type="response",test_data)
    }

    test_data$my_cate<-y1_x - y0_x

    # effects calculated on the test_set  #######################
    DIM<- mean(eval(parse(text=paste0("test_W1$",y))))-mean(eval(parse(text=paste0("test_W0$",y))))
    ATE<- mean (y1_x - y0_x)  #AVERAGE TREATEMENT EFFECT
    ATENT <- mean(y0_x)      #AVERAGE TREATEMENT EFFECT ON NON TREATED
    ATET <- mean(y1_x)

    #results output for the test dataset - new policy
    # Output block
    output <- paste(
      " --------------------------------------------------\n",
      "- Treatment-effects estimation: NEW POLICY       -\n",
      "--------------------------------------------------\n",
      paste("Number of obs =", obs_test), "\n",
      paste("Estimator = regression adjustment"), "\n",
      paste("Outcome model =",model), "\n",
      paste("DIM =", round(DIM, 7)), "\n",
      paste("ATE =", round(ATE, 7)), "\n",
      paste("ATET =", round(ATET, 7)), "\n",
      paste("ATENT =", round(ATENT, 7))
    )

    # Output print
    if(verbose){
      cat(output, "\n")
    }

    # Adding _train_new_index index
    # Dataset append
    train_data$train_new_index <- "train"
    test_data$train_new_index <- "new"
    data <- rbind(train_data, test_data)

    gc()

  if (model != "glm" & model != "rf") {

    stop("Unsupported model. Please enter a valid model.")
  }

  invisible(data)
}
