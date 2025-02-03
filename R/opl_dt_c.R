# Global variables declaration
utils::globalVariables(c("X1", "X2", "my_cate","c1","c2","c3"))

#' @name opl_dt_c
#' @title Optimal Policy Learning with Decision Tree
#' @description Implementing ex-ante treatment assignment using as policy class a 2-layer fixed-depth decision-tree at specific splitting variables and threshold values.
#'
#' @references
#' - Athey, S., and Wager S. 2021. Policy Learning with Observational Data, Econometrica, 89, 1, 133–161.
#' - Cerulli, G. 2021. Improving econometric prediction by machine learning, Applied Economics Letters, 28, 16, 1419-1425.
#' - Cerulli, G. 2022. Optimal treatment assignment of a threshold-based policy: empirical protocol and related issues, Applied Economics Letters, DOI: 10.1080/13504851.2022.2032577.
#' - Gareth, J., Witten, D., Hastie, D.T., Tibshirani, R. 2013. An Introduction to Statistical Learning : with Applications in R. New York, Springer.
#' - Kitagawa, T., and A. Tetenov. 2018. Who Should Be Treated? Empirical Welfare Maximization Methods for Treatment Choice, Econometrica, 86, 2, 591–616.
#'
#' @param make_cate_result A data frame resulting from the `make_cate` function, containing the predicted treatment effects (`my_cate`) and other variables for treatment assignment.
#' @param z A character vector containing the names of the variables used for treatment assignment.
#' @param w A string representing the treatment indicator variable name.
#' @param c1 Value of the threshold value c1 for the first splitting variable. This number must be chosen between 0 and 1.
#' @param c2 Value of the threshold value c2 for the second splitting variable. This number must be chosen between 0 and 1.
#' @param c3 Value of the threshold value c3 for the third splitting variable. This number must be chosen between 0 and 1.
#' @param verbose Set TRUE to print the output on the console.
#'
#' @return A list containing:
#' - `W_opt_constr`: The maximum average constrained welfare.
#' - `W_opt_unconstr`: The average unconstrained welfare.
#' - `units_to_be_treated`: A data frame of the units to be treated based on the optimal policy.
#' - A plot showing the optimal policy assignment.
#'
#' @importFrom dplyr filter
#' @importFrom tidyr crossing
#' @importFrom pander pander
#' @importFrom ggplot2 ggplot aes geom_point
#'
#' @export
#'


##### DT Decision Tree #####################################################
##### Decision tree based policy learning at specific threshold values #####

opl_dt_c <- function(make_cate_result,z,w,c1=NA,c2=NA,c3=NA,verbose=TRUE) {

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
Tc <- crossing(X1 = seq(0, 1, 0.1), X2 = seq(0, 1, 0.1), X3 = seq(0, 1, 0.1)) # Convert to data frame grid
#combinazioni di variabili a 2 a 2
combinations <- expand.grid(rep(list(c(1,2)), 3))
# seleziono solo dove my_cate>0
appo<-var_std[which(var_std$D_opt=="True"),]

# results list
ris<-matrix(0, nrow = 8)
Wc_res<-matrix(0, ncol=8, nrow = 1331)
Wc_list<-list()

for (k in 1:8) {
  gc()
  mat = matrix(0,  nrow(Tc), nrow(appo))
  X1<-appo[,combinations[k,1]]
  X2<-appo[,combinations[k,2]]
  X3<-appo[,combinations[k,3]]

  d1<-as.data.frame(lapply(X1, function(a) {sapply(Tc[,1], function(b) a >= b) }))

  ris2<-as.data.frame(lapply(X2, function(a) {sapply(Tc[,2], function(b) a >= b) }))
  d2<-as.data.frame(ifelse(ris2==TRUE & d1==TRUE,1,0))

  ris3<-as.data.frame(lapply(X3, function(a) {sapply(Tc[,3], function(b) a >= b) }))
  d2_b<-as.data.frame(ifelse(ris3==TRUE & d1==FALSE,1,0))

  d2<-d2+d2_b

  mat<-as.data.frame(ifelse(((d1==FALSE & d2==1) | (d1==TRUE & d2==1)),1,0))
  Wc<-as.data.frame(t(t(mat)*appo$my_cate))
  Wc_list[[k]]<-Wc

  rowMeans_no_zeros <- function(x) {
    apply(x, 1, function(row) mean(row[row != 0]))
  }
  Wc_res[,k] <- rowMeans_no_zeros(Wc)

  ris[k]<-max(Wc_res[,k],na.rm = TRUE)
}

Wc_list<-Wc_list[which(ris==max(ris))]
max_combinations<-combinations[which(ris==max(ris)),]
max_combinations<-ifelse(max_combinations==1,z[1],z[2])
nc<-dim(max_combinations)[1]

# calculating results
W_opt_constr<-max(ris)

# Selection for columns where welfare constrained is max
col_max<-which(ris==max(ris))
Wc_opt<-Wc_res[,col_max]

# Tabulate Max Avg Constrained Welfare
comb_name<-ifelse(combinations==1,z[1],z[2])
comb_name<-cbind(comb_name,round(ris,7))
colnames(comb_name)<-c("Variable 1","Variable 2","Variable 3","Avg Constrained Welfare")

output <- paste(
  "----------------------------------\n",
  "- Results on selection variables -\n",
  "----------------------------------\n",
  "Selection variables and Avg Constrained Welfare\n",
  paste(capture.output(pander(comb_name)), collapse = "\n"),
  "                                \n",
  "                                \n",
  "--------------------------------\n",
  "-  Max Avg Constrained Welfare -\n",
  "--------------------------------\n",
  paste(capture.output(pander(comb_name[which(comb_name[, 4] == max(round(ris, 7))),])), collapse = "\n"),
  "                                \n",
  sep = ""
)

# Output print
if(verbose){
  cat(output, "\n")
}

############################################
### FUNZIONE PER SCELTA CON MASSIMI MULTIPLI
j<-1
h<-1
if (nc>1) {
  h<-opl_dt_max_choice(nc,col_max)
  h<-as.numeric(h)
}

j<-which(col_max==h)

############################################
# selezione trattati e non

best<-matrix(0,nc,3)
units_to_be_treated<-list()
untreated<-0
treated<-0
treat_row<-0
perc_opt_treat<-0


  # calculating results
  Tc1<-cbind(Tc,Wc_opt[,j])
  cons<-Tc1[Tc1[4] == W_opt_constr & !is.na(Tc1[4]),]

  # check if thresholds c1 & c2 & c3 are customized by the user or not
  if (is.na(c1) && is.na(c2) && is.na(c3)) {
    # threshold after selection
    best_1 <- cons$X1[1]
    best_2 <- cons$X2[1]
    best_3 <- cons$X3[1]
  }
  else  {
    # user threshold selection
    best_1 <- c1
    best_2 <- c2
    best_3 <- c3
  }

  # obs to be treated
  X1<-appo[,combinations[h,1]]
  X2<-appo[,combinations[h,2]]
  X3<-appo[,combinations[h,3]]

  x_full<-cbind(X1,X2,X3)

  treat_row<-row(x_full)[which(
    (X1>=best_1 & X2>=best_2) | (X1<best_1 & X3>=best_3)
  ),1]

  units_to_be_treated <-appo[treat_row,]

  # number of treated and untreated
  treated<-length(treat_row)
  perc_opt_treat=treated/obs*100
  untreated<-obs-treated

# calculating and showing final results
J_1 <- make_cate_result %>% filter(!!sym(w)==1)
W_rct<-mean(J_1$my_cate)      # ATET = average welfare actually realized

# W_opt_unconstrained
D_opt <- make_cate_result %>% filter(my_cate>=0)
W_opt_unconstr<-mean(D_opt$my_cate)   #AVG WELFARE GENERATED UNCONSTRAINED

gc()

# print results and graph
output <- paste(
  "                                \n",
  "                                \n",
  "--------------------------------\n",
  "-        Main results          -\n",
  "--------------------------------\n",
  paste("Policy class", ":", "Fixed-depth decision-tree"), "\n",
  paste("Learner", "=", "Regression adjustment"), "\n",
  paste("N. of units = ", obs), "\n",
  paste("Selection variable = ", paste(comb_name[h,1:3], collapse = ", ")), "\n",
  paste("Lin. comb.parameter c1 = ", best_1), "\n",
  paste("Lin. comb.parameter c2 = ", best_2), "\n",
  paste("Lin. comb.parameter c3 = ", best_3), "\n",
  paste("Average unconstrained welfare = ", round(W_opt_unconstr, 7)), "\n",
  paste("Average constrained welfare = ", round(W_opt_constr, 7)), "\n",
  paste("Percentage of treated = ", round(perc_opt_treat, 1)), "\n",
  paste("N. of treated = ", treated), "\n",
  paste("N. of untreated = ", untreated), "\n",
  "--------------------------------\n",
  sep = ""
)

# Stampa dell'output in un unico blocco
if(verbose){
  cat(output, "\n")
}

# plot graph

#### PLOT  ###########
# # creating graph
Treated <- rep(0,nrow(appo))
Treated[treat_row] = 1

appo$Treated <- as.factor(ifelse(Treated==1,"True","False"))


 myColors <- c("blue","orange")
 names(myColors) <- levels(Treated)
 colScale <- scale_colour_manual(name = "Treated",values = myColors)

     graph<-ggplot(appo,aes(x=X1,y=X2,colour=Treated)) +
     geom_point() +
     labs(title = "Optimal policy assignment - policy class: decision tree",
     x = paste(z[1],"_std",sep=""),
     y = paste(z[2],"_std",sep=""))

if(verbose){
  print(graph)
}


gc()
}

