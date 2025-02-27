# Global variables declaration
utils::globalVariables(c("X1", "X2", "my_cate","c1","c2","c3"))

#' @name opl_lc_c
#' @title Linear Combination Based Policy Learning
#' @description Implementing ex-ante treatment assignment using as policy class a linear-combination approach at specific parameters' values c1, c2, and c3 for the linear-combination of variables var1 and var2: c1*var1+c2*var2>=c3.
#'
#' @references
#' - Athey, S., and Wager S. 2021. Policy Learning with Observational Data, Econometrica, 89, 1, 133–161.
#' - Cerulli, G. 2021. Improving econometric prediction by machine learning, Applied Economics Letters, 28, 16, 1419-1425.
#' - Cerulli, G. 2022. Optimal treatment assignment of a threshold-based policy: empirical protocol and related issues, Applied Economics Letters, DOI: 10.1080/13504851.2022.2032577.
#' - Gareth, J., Witten, D., Hastie, D.T., Tibshirani, R. 2013. An Introduction to Statistical Learning : with Applications in R. New York, Springer.
#' - Kitagawa, T., and A. Tetenov. 2018. Who Should Be Treated? Empirical Welfare Maximization Methods for Treatment Choice, Econometrica, 86, 2, 591–616.
#'
#' @param make_cate_result A data frame containing the input data. It must include
#'   a column named `my_cate` representing conditional average treatment effects (CATE) generated using make_cate function.
#' @param z A character vector of length 2 specifying the column names of the two
#'   threshold variables to be standardized.
#' @param w A character string specifying the column name indicating treatment assignment (binary variable).
#' @param c1 Threshold for var1 given by the user or optimized by the the function. This number must be chosen between 0 and 1.
#' @param c2 Threshold for var2 given by the user or optimized by the the function. This number must be chosen between 0 and 1.
#' @param c3 Third parameter of the linear-combination. This number must be chosen between 0 and 1.
#' @param verbose Set TRUE to print the output on the console.
#'
#' @return The function returns a data frame containing the standardized variables
#'   and treatment assignments, and prints a summary of the results and a plot
#'   showing the optimal policy assignment.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Standardizes the threshold variables using a min-max scaling technique.
#'   \item Determines the optimal treatment assignment based on the linear
#'     combination of the threshold variables.
#'   \item Performs a grid search to estimate the optimal policy.
#'   \item Outputs a plot visualizing the optimal treatment assignments.
#'   \item Prints the main results, including the percentage of treated units,
#'     the unconstrained and constrained welfare, and the policy parameters.
#' }
#'
#' @importFrom ggplot2 ggplot geom_point labs geom_abline geom_hline geom_vline
#' @importFrom dplyr filter sym
#' @importFrom tidyr crossing
#'
#' @export
#'

##### OP Linear combination #####################################################
##### Linear combination based policy learning at specific threshold values #####

opl_lc_c <- function(make_cate_result,z,w,c1=NA,c2=NA,c3=NA,verbose=TRUE) {

  if (!is.data.frame(make_cate_result)) {
    stop("`make_cate_result` deve essere un data frame.")
  }

obs<-dim(make_cate_result)[1]

# ******************************************************************************
#   * Standardize threshold variables [0-1]
# ******************************************************************************
# indexing constraints
z_df<-make_cate_result[,c(z[1],z[2])]

var_std<-list()
n<-2
i<-1

for (i in 1:n)
{
  #`var_std = (`var' - r(min)') / (r(max)'-r(min)')
  var_std[[i]]<-(z_df[,i]-min(z_df[,i])) / (max(z_df[,i])-min(z_df[,i]))
}

var_std<-as.data.frame(var_std)
colnames(var_std)<-c("X1","X2")

###### Standardization - END ###################################################

# Optimal policy learning ####################################

# W_opt_constrained
make_cate_result$D_opt<-"False"
make_cate_result$D_opt[make_cate_result$my_cate>=0] <- "True"
var_std$D_opt<-make_cate_result$D_opt
var_std$my_cate<-make_cate_result$my_cate

#grid search
# Define a named list of parameter values
Tc <- crossing(X1 = seq(0, 1, 0.1), X2 = seq(0, 1, 0.1), X3 = seq(0, 1, 0.1)) # Convert to data frame grid

########################
# Selection only for my_cate>0
appo<-var_std[which(var_std$D_opt=="True"),]
d1<-as.data.frame(lapply(appo$X1, function(a) {sapply(Tc[,1], function(b) a * b) }))
d2<-as.data.frame(lapply(appo$X2, function(a) {sapply(Tc[,2], function(b) a * b) }))
d<-d1+d2
mat<-as.data.frame(lapply(d, function(a) {sapply(Tc[,3], function(b) a > b) }))
mat=as.data.frame(t(mat))
#################
Tc$W_cons<-0
for (i in  1:nrow(Tc))
{
  Tc$W_cons[i]<-mean(appo$my_cate[which(mat[,i]==1)])
}

# W_opt_constrained
W_opt_constr<-max(Tc$W_cons,na.rm=TRUE)  #AVG WELFARE GENERATED CONSTRAINED

# calculating results
cons<-(Tc[Tc$W_cons == W_opt_constr & !is.na(Tc$W_cons),])

# check if thresholds c1 & c2 are customized by the user or optimized
if (is.na(c1) && is.na(c2) && is.na(c3)) {
  best_c1 <- mean(cons$X1)
  best_c2 <- mean(cons$X2)
  best_c3 <- mean(cons$X3)

}
else  {
  # user threshold selection
  best_c1 <- c1
  best_c2 <- c2
  best_c3 <- c3
}

# calculating and showing final results
J_1 <- make_cate_result %>% filter(!!sym(w)==1)
W_rct<-mean(J_1$my_cate)      # ATET = average welfare actually realized

# W_opt_unconstrained
D_opt <- make_cate_result %>% filter(my_cate>=0)
W_opt_unconstr<-mean(D_opt$my_cate)   #AVG WELFARE GENERATED UNCONSTRAINED

# units to be treated
#Tbi(c1, c2, c3) = Tb∗i· 1[c1 × x1 + c2 × x2 >= c3]
treated<-sum(((appo$X1*best_c1) + (appo$X2*best_c2)>=best_c3))
units_to_be_treated<-ifelse(((appo$X1*best_c1) + (appo$X2*best_c2)>=best_c3),1,0)
perc_opt_treat=treated/obs*100
untreated<-obs-treated
var_std$units_to_be_treated<-0
appo$units_to_be_treated[which(appo$D_opt=="True")]<-units_to_be_treated
var_std$units_to_be_treated[which(var_std$D_opt=="True")]<-appo$units_to_be_treated

# creating graph
#var_plot<-var_std[which(var_std$D_opt=="True"),]
Treated<-ifelse(appo$units_to_be_treated==1,"True","False")

myColors <- c("orange","blue")
names(myColors) <- levels(Treated)
colScale <- scale_colour_manual(name = "Treated",values = myColors)

graph<-ggplot(appo,aes(x=X1,y=X2,colour=Treated)) +
  geom_point() +
  labs(title = "Optimal policy assignment - policy class: linear combination",
    x = paste(z[1],"_std",sep=""),
    y = paste(z[2],"_std",sep=""))+
    geom_abline(intercept = best_c3/best_c2, slope = -best_c1/best_c2,color="red",linewidth=1)+
    geom_hline(yintercept= best_c1,linetype="dashed",color="red")+
    geom_vline(xintercept= best_c2,linetype="dashed",color="red")

  #plot(var_plot$X2~var_plot$X1) +  abline(a=best_c3/best_c2,b=-best_c1/best_c2)

# add standardized variables to the dataset
data_lc<-cbind(make_cate_result,var_std$X1,var_std$X2,var_std$units_to_be_treated)
colnames(data_lc)[colnames(data_lc) == "var_std$X1"] <- paste(z[1],"_std",sep="")
colnames(data_lc)[colnames(data_lc) == "var_std$X2"] <- paste(z[2],"_std",sep="")
colnames(data_lc)[colnames(data_lc) == "var_std$units_to_be_treated"] <- "units_to_be_treated"
W_opt_constr<-mean(data_lc$my_cate[data_lc$'var_std$units_to_be_treated'==1])

# print results and graph
output <- paste(
  "--------------------------------\n",
  "-        Main results          -\n",
  "--------------------------------\n",
  paste("Policy class:", "Linear combination"), "\n",
  paste("Learner =", "Regression adjustment"), "\n",
  paste("N. of units =", obs), "\n",
  paste("Selection variables =", paste(z, collapse = ", ")), "\n",
  paste("Lin. comb.parameter c1 =", best_c1), "\n",
  paste("Lin. comb.parameter c2 =", best_c2), "\n",
  paste("Lin. comb.parameter c3 =", best_c3), "\n",
  paste("Average unconstrained welfare =", round(W_opt_unconstr, 7)), "\n",
  paste("Average constrained welfare =", round(W_opt_constr, 7)), "\n",
  paste("Percentage of treated =", round(perc_opt_treat, 1)), "\n",
  paste("N. of treated =", treated), "\n",
  paste("N. of untreated =", untreated)
)

# Output print
if(verbose){
  cat(output, "\n")
  # Tabulate the variable "units_to_be_treated"
  print(as.data.frame(table(var_std$units_to_be_treated),row.names = c("Not to treat","To treat")))
  # plot graph
  print(graph)
}

gc()
invisible(data_lc)
}
