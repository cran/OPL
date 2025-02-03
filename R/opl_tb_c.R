# Global variables declaration
utils::globalVariables(c("X1", "X2", "my_cate","c1","c2"))

#' @name opl_tb_c
#' @title Threshold-based policy learning at specific values
#' @description Implementing ex-ante treatment assignment using as policy class a threshold-based (or quadrant)
#'    approach at specific threshold values c1 and c2 for respectively the selection variables var1 and var2.
#'
#' @references
#' - Athey, S., and Wager S. 2021. Policy Learning with Observational Data, Econometrica, 89, 1, 133–161.
#' - Cerulli, G. 2021. Improving econometric prediction by machine learning, Applied Economics Letters, 28, 16, 1419-1425.
#' - Cerulli, G. 2022. Optimal treatment assignment of a threshold-based policy: empirical protocol and related issues, Applied Economics Letters, DOI: 10.1080/13504851.2022.2032577.
#' - Gareth, J., Witten, D., Hastie, D.T., Tibshirani, R. 2013. An Introduction to Statistical Learning : with Applications in R. New York, Springer.
#' - Kitagawa, T., and A. Tetenov. 2018. Who Should Be Treated? Empirical Welfare Maximization Methods for Treatment Choice, Econometrica, 86, 2, 591–616.
#'
#' @importFrom dplyr %>% filter
#' @importFrom tidyr crossing
#' @importFrom ggplot2 ggplot geom_point labs geom_abline geom_hline geom_vline scale_colour_manual
#'
#' @param make_cate_result A data frame containing the input data. It must include
#'   a column named `my_cate` representing conditional average treatment effects (CATE) generated using make_cate function.
#' @param z A character vector of length 2 specifying the column names of the two
#'   threshold variables to be standardized.
#' @param w A character string specifying the column name indicating treatment assignment (binary variable).
#' @param c1 Threshold for var1 given by the user or optimized by the the function. This number must be chosen between 0 and 1.
#' @param c2 Threshold for var2 given by the user or optimized by the the function. This number must be chosen between 0 and 1.
#' @param verbose Set TRUE to print the output on the console.
#'
#' @return The function invisibly returns the input data frame augmented with the following columns:
#' \itemize{
#'   \item \code{z[1]_std}: Standardized version of the first threshold variable.
#'   \item \code{z[2]_std}: Standardized version of the second threshold variable.
#'   \item \code{units_to_be_treated}: Binary indicator for whether a unit should be treated based on the optimal policy.
#' }
#' Additionally, the function:
#' \itemize{
#'   \item Prints the main results summary, including optimal threshold values, average constrained and unconstrained welfare, and treatment proportions.
#'   \item Displays a scatter plot visualizing the policy assignment.
#' }
#'
#' @details The function:
#' \enumerate{
#'   \item Standardizes the threshold variables to a 0-1 range.
#'   \item Identifies the optimal thresholds based on grid search for maximizing constrained welfare.
#'   \item Computes and displays key statistics, including average welfare measures and the percentage of treated units.
#' }
#'
#' @export
#'

##### OP Threshold Based C #####################################################
##### Threshold-based policy learning at specific threshold values #############

opl_tb_c <- function(make_cate_result,z,w,c1=NA,c2=NA,verbose=TRUE) {

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
Tc <- crossing(X1 = seq(0, 1, 0.1), X2 = seq(0, 1, 0.1)) # Convert to data frame grid

########################
# Selection only for my_cate>0
appo<-var_std[which(var_std$D_opt=="True"),]
d1<-as.data.frame(lapply(appo$X1, function(a) {sapply(Tc[,1], function(b) a > b) }))
d2<-as.data.frame(lapply(appo$X2, function(a) {sapply(Tc[,2], function(b) a > b) }))
mat<-as.data.frame(ifelse((d1==1 & d2==1),1,0))
mat=as.data.frame(t(mat))
#################
Tc$W_cons<-0

for (i in 1:nrow(Tc))
{
  Tc$W_cons[i]<-mean(appo$my_cate[which(mat[,i]==1)])
}

# W_opt_constrained
W_opt_constr<-max(Tc$W_cons,na.rm=TRUE)    #AVG WELFARE GENERATED CONSTRAINED

# calculating results
cons<-(Tc[Tc$W_cons == W_opt_constr & !is.na(Tc$W_cons),])

# check if thresholds c1 & c2 are customized by the user or optimized
if (is.na(c1) && is.na(c2)) {
  best_c1 <- mean(cons$X1)
  best_c2 <- mean(cons$X2)
  }
          else  {
            # user threshold selection
            best_c1 <- c1
            best_c2 <- c2
          }

# calculating and showing final results
J_1 <- make_cate_result %>% filter(!!sym(w)==1)
W_rct<-mean(J_1$my_cate)      # ATET = average welfare actually realized

# W_opt_unconstrained
D_opt <- make_cate_result %>% filter(my_cate>=0)
W_opt_unconstr<-mean(D_opt$my_cate)   #AVG WELFARE GENERATED UNCONSTRAINED

# units to be treated
#Tbi(c1, c2) = Tb∗i· 1[x1 >= c1] · 1[x2 >= c2]
treated<-sum((appo$X1>best_c1)&(appo$X2>best_c2))
perc_opt_treat=treated/obs*100
untreated<-obs-treated
var_std$units_to_be_treated<-0
appo$units_to_be_treated[which(appo$D_opt=="True")]<-ifelse((appo$X1>best_c1)&(appo$X2>best_c2),1,0)
var_std$units_to_be_treated[which(var_std$D_opt=="True")]<-appo$units_to_be_treated

# creating graph
#var_plot<-var_std[which(var_std$D_opt=="True"),]
Treated<-ifelse(appo$units_to_be_treated==1,"True","False")

myColors <- c("orange","blue")
names(myColors) <- levels(Treated)
colScale <- scale_colour_manual(name = "Treated",values = myColors)

graph<-ggplot(appo,aes(x=X2,y=X1,colour=Treated)) +
  geom_point() +
  labs(title = "Optimal policy assignment - policy class: threshold based",
    x = paste(z[1],"_std",sep=""),
    y = paste(z[2],"_std",sep=""))+
  geom_hline(yintercept= best_c1,linetype="dashed",color="red")+
  geom_vline(xintercept= best_c2,linetype="dashed",color="red")

graph <- graph + colScale

# add standardized variables to the dataset
data_tb<-cbind(make_cate_result,var_std$X1,var_std$X2,var_std$units_to_be_treated)
colnames(data_tb)[colnames(data_tb) == "var_std$X1"] <- paste(z[1],"_std",sep="")
colnames(data_tb)[colnames(data_tb) == "var_std$X2"] <- paste(z[2],"_std",sep="")
colnames(data_tb)[colnames(data_tb) == "var_std$units_to_be_treated"] <- "units_to_be_treated"

# print results and graph
output <- paste(
  "--------------------------------\n",
  "-        Main results          -\n",
  "--------------------------------\n",
  paste("Policy class:", "Threshold-based"), "\n",
  paste("Learner =", "Regression adjustment"), "\n",
  paste("N. of units =", obs), "\n",
  paste("Selection variables =", paste(z, collapse = ", ")), "\n",
  paste("Threshold value c1 =", best_c1), "\n",
  paste("Threshold value c2 =", best_c2), "\n",
  paste("Average unconstrained welfare =", W_opt_unconstr), "\n",
  paste("Average constrained welfare =", W_opt_constr), "\n",
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
invisible(data_tb)
}
