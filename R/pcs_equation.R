#' PCS calculation
#'
#' This function takes a correlation matrix (as data frame!) as input and returns the values of a PCS model.
#' In an iterative process, nodes are either activated or inhibited to represent a mental model.
#'
#' @param corr Dataframe of a correlation matrix
#' @param t Maximum number of iterations, standard 100
#' @param d Decay parameter, standard .05
#' @param min Minimum activation, standard 0
#' @param max Maximum activation, standard 1
#' @param in_act Initial activation, standard .1
#'
#' @return Returns activation of each node each iteration in dataframe with the timepoint.
#' @export
#'
#' @examples
#' pcs_equation(data.frame(X = c(1, .8, .2), Y = c(.8, 1, -.5), Z = c(.2, -.5, 1)))
#'
pcs_equation <- function(corr, t = 100, d = .05, min = 0, max = 1, in_act = .1){
  # corr must be a data frame containing a correlation matrix
  n_factors <- length(corr)

  # Predefine results dataset with NAs
  results <- cbind(timestep = 1,
                   data.frame(matrix(NA, nrow = 1, ncol = n_factors)))

  # Initial activation
  results[1, 2:(n_factors+1)] <- in_act # .05 suggested by Thagard in Explanatory Coherence 1989

  ## Differential Equations ----

  for (ti in 2:t){ # activation over time
    for (fac_num in 1:n_factors){

      # Calculate change in activation
      netj <- sum(corr[setdiff(1:n_factors, fac_num), fac_num] *
                    # Selects correlations of all cols but the diagonal
                    results[(ti-1), (setdiff(1:n_factors, fac_num)+1)])
      # multiplies with all prev. activations but the factor's own

      # Calculate new activation (at current timestep)
      results[ti, (fac_num+1)] <- max(min, min(max,
                                               netj*
                                                 results[(ti-1), (fac_num+1)] +
                                                 (1 - d)*results[(ti-1), (fac_num+1)]))
    }
    # Add ti as timestep
    results[ti, 1] <- ti

    # browser()
    # Loop should stop if maximum timestep is reached, or System is settled (Minimum-Change-Criterion)
    if (ti > 2 &&
        dplyr::between(sum(results[(ti-1), 2:n_factors]-results[(ti-2), 2:n_factors]), -0.00001, 0.00001)) {
      break
    }
  }

  return(results)

}
