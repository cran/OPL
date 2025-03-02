% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opl_lc_c.R
\name{opl_lc_c}
\alias{opl_lc_c}
\title{Linear Combination Based Policy Learning}
\usage{
opl_lc_c(make_cate_result, z, w, c1 = NA, c2 = NA, c3 = NA, verbose = TRUE)
}
\arguments{
\item{make_cate_result}{A data frame containing the input data. It must include
a column named \code{my_cate} representing conditional average treatment effects (CATE) generated using make_cate function.}

\item{z}{A character vector of length 2 specifying the column names of the two
threshold variables to be standardized.}

\item{w}{A character string specifying the column name indicating treatment assignment (binary variable).}

\item{c1}{Threshold for var1 given by the user or optimized by the the function. This number must be chosen between 0 and 1.}

\item{c2}{Threshold for var2 given by the user or optimized by the the function. This number must be chosen between 0 and 1.}

\item{c3}{Third parameter of the linear-combination. This number must be chosen between 0 and 1.}

\item{verbose}{Set TRUE to print the output on the console.}
}
\value{
The function returns a data frame containing the standardized variables
and treatment assignments, and prints a summary of the results and a plot
showing the optimal policy assignment.
}
\description{
Implementing ex-ante treatment assignment using as policy class a linear-combination approach at specific parameters' values c1, c2, and c3 for the linear-combination of variables var1 and var2: c1\emph{var1+c2}var2>=c3.
}
\details{
The function performs the following steps:
\itemize{
\item Standardizes the threshold variables using a min-max scaling technique.
\item Determines the optimal treatment assignment based on the linear
combination of the threshold variables.
\item Performs a grid search to estimate the optimal policy.
\item Outputs a plot visualizing the optimal treatment assignments.
\item Prints the main results, including the percentage of treated units,
the unconstrained and constrained welfare, and the policy parameters.
}
}
\references{
\itemize{
\item Athey, S., and Wager S. 2021. Policy Learning with Observational Data, Econometrica, 89, 1, 133–161.
\item Cerulli, G. 2021. Improving econometric prediction by machine learning, Applied Economics Letters, 28, 16, 1419-1425.
\item Cerulli, G. 2022. Optimal treatment assignment of a threshold-based policy: empirical protocol and related issues, Applied Economics Letters, DOI: 10.1080/13504851.2022.2032577.
\item Gareth, J., Witten, D., Hastie, D.T., Tibshirani, R. 2013. An Introduction to Statistical Learning : with Applications in R. New York, Springer.
\item Kitagawa, T., and A. Tetenov. 2018. Who Should Be Treated? Empirical Welfare Maximization Methods for Treatment Choice, Econometrica, 86, 2, 591–616.
}
}
