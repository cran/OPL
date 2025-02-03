#' @name overlapping
#' @title Testing overlap between old and new policy sample
#' @description
#' Function to perform overlap analysis between train and test datasets.
#' The function performs principal component analysis (PCA) on the covariates for both sets
#' and calculates the Kolmogorov-Smirnov test for overlap.
#'
#' @param train_data Train Dataset indicating the old policy sample.
#' @param test_data Test Dataset indicating the new policy sample.
#' @param x Vector of predictor variables.
#'
#' @return The function prints the superposition graph and the results of the Kolmogorov-Smirnov test.
#'
#' @importFrom stats prcomp density ks.test
#' @importFrom pander pander
#' @importFrom graphics legend lines
#'
#' @export

##### Overlapping #############################################################
##### 1) Execute principal component on covariates for train and test set #####
##### 2) Execute Kolmogorov Smirnov test to verify overlapping ################

overlapping <- function(train_data,test_data,x) {

##################################################
### OVERLAPPING between train and test sets ######
##################################################

# overlap between training and test set
train <- prcomp(train_data[,x],
                        center = TRUE,
                        scale. = TRUE)
test <- prcomp(test_data[,x],
                center = TRUE,
                scale. = TRUE)

train<-train$x[, 1]
test<-test$x[, 1]

# Graph: Train vs test
density_train <- density(train)
density_test <- density(test)

plot(density_train, main = "Overlap between Train and Test",
     xlab = "Values", ylab = "Density", col = "blue", lwd = 2,
     xlim = range(c(density_train$x, density_test$x)),
     ylim = range(c(density_train$y, density_test$y)))

lines(density_test, col = "red", lwd = 2)
legend("topright", legend = c("Test", "Train"), col = c("blue", "red"), lwd = 2, cex = 0.8)


########################################
# Test di Kolmogorov-Smirnov
########################################

ks_train_test <- ks.test(train, test)

#results output
  pander("-----------------------------------------------------\n")
  pander("-------------- Train vs Test set --------------------")
  pander(ks_train_test)

}




