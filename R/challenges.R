#' @title First function
#' @description
#' Given a square matrix, calculates the average over the sum of row averages and column averages
#' 
#' @param m a square matrix with no missing values
#' @return Single numerical value
#' @examples
#' m <- matrix(seq(16),nrow=4)
#' un(m)
#' @export
un <- function(m) {
  mean(colMeans(m) + rowMeans(m))
}

un_origin <- function(m) {
  mean(apply(m,1,mean) + apply(m,2,mean))
}

#' @title Second
#' @description
#' Given a vector gives the longest continuous increasing subset
#' 
#' @param vec Numerical vector with no missing values
#' @return A numerical vector containing the longest continuous increasing subset
#' @export
deux <- function(vec) {
  longest_seq <- c(vec[1])  # Longest increasing subsequence found
  current_seq <- c(vec[1])  # Current increasing subsequence

  for (i in 2:length(vec)) {
    if (vec[i] > vec[i - 1]) {
      # Continue current increasing subsequence
      current_seq <- c(current_seq, vec[i])
    } else {
      # Compare and reset current subsequence
      if (length(current_seq) > length(longest_seq)) {
        longest_seq <- current_seq
      }
      current_seq <- c(vec[i])  # Start a new subsequence
    }
  }

  if (length(current_seq) > length(longest_seq)) {
    longest_seq <- current_seq
  }
  
  return(longest_seq)
}

#' @title Third
#' @description
#' Given a vector return the count of each unique element
#' 
#' Hint: Try looking into `tabulate`, `fastmatch::fastmatch`
#' @param vec Numerical vector
#' @return A single numerical vector with counts of each unique element
#'
#' @export
trois <- function(vec) {
  table(vec)
}
