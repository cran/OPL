#' @name opl_dt_max_choice
#' @title User selection on multiple choice
#' @description
#' Function that allows the user to select a row of maximum welfare among the rows with maximum welfare constrained. The function prints out the result and requires user input to select the row.
#'
#' @param nc Numeber of max welfare.
#' @param col_max Row index for max constrained welfare.
#' @param verbose Set TRUE to print the output on the console.
#'
#' @return Return the user's selection as an input.
#'
#' @importFrom utils capture.output
#'
#' @export

opl_dt_max_choice <- function(nc,col_max,verbose=TRUE) {
  # Output block
  output <- paste(
    "Number of multiple maximum:", nc, "\n",
    "Rows with Max Avg Constrained Welfare:\n",
    paste("Row:", col_max, collapse = "; "), "\n",
    "                                  \n",
    "Please type the row number to display and press enter:\n"
  )

  # User's input
  if(verbose){
    cat(output)
  }
  input <- readLines(stdin(), n = 1)

  # Selected input print
  if(verbose){
  cat(paste("You selected Item #", input, ": ", sep = ""))
  }

  # Input return to the function
  return(input)
}
