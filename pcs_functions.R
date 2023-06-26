# 4. PCS Modelling

### This script was created using R 4.2.2. All necessary packages can be 
### found, installed and loaded using the groundhog.library() command
### below. 
### Package loading is done with groundhog 2.2.1 until further notice,
### due to a bug in more recent versions. (For switching to that version
### use: groundhog::meta.groundhog('2023-04-01'))
# groundhog::meta.groundhog('2023-04-01')
# groundhog::groundhog.library(c("dplyr", "xlsx", "ggplot2",
#                                "tibble"), "2023-04-01")



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

res <- pcs_equation(vax_fac_corr)
bla <- pcs_equation(data.frame(X = c(1, .8, .2),
                               Y = c(.8, 1, -.5),
                               Z = c(.2, -.5, 1)))


pcs_viz <- function(pcsresult){
  
  neg_palette <- c("red", "magenta", "darkred",
                   "#C70E51", "#c70e63", "#c70e3e",
                   "#d5032a", "#d53503", "#d503a6",
                   "#f09c98", "#fe6d66", "#fe66f7",
                   "#fe6691", "#b24766", "#fe66c7")
  
  pos_palette <- c("yellow4", "yellowgreen", "olivedrab",
                   "palegreen3", "palegreen4", "seagreen4", 
                   "#0ecb27", "#6ee07d", "powderblue",
                   "#3ceeea", "#3ceed8", "#3cbaee", "#3cccee",
                   "#3ca8ee", "#5cee3c", "#3c43ee", "#6369f1",
                   "#4e3cee", "#3cee79", "#27994e")
  
  vplotty <- pcsresult |>
    reshape2::melt() |> # "reshape2:: " technically not necessary, but "melt" 
    # is not exclusive to that pacakge, so specification is useful
    dplyr::filter(variable != "timestep") |>
    dplyr::mutate(iteration = rep(1:max(pcsresult$timestep), (dim(pcsresult)[2]-1)),
                  variable = as.character(variable))

  sample_vector <- round(pcsresult[max(pcsresult$timestep), 2:dim(pcsresult)[2]], 2) 
  # sample_vector contains a vector of 1 and 0, which represents the setted state of a pcs model
  pal_samp <- ifelse(sample_vector == 1, 
                     sample(pos_palette, length(sample_vector[sample_vector == 1]), replace = F),
                     sample(neg_palette, length(sample_vector[sample_vector == 0]), replace = F))
  # pal_samp samples colors from a green-blue palette for activated nodes and from a pink-red palette for inactive nodes

  library(ggplot2)
  ## Plotting ---
  ggplot(vplotty, aes(x = iteration, y = value)) + 
    geom_line(aes(color = variable), linewidth = 1) +
    scale_color_manual(breaks = unique(vplotty$variable), values = pal_samp) +
    papaja::theme_apa() +
    labs(x = "Iteration", y = "Activation", 
         title = "PCS Model Results",
         color = "Factors") +
    scale_x_continuous(limits = c(1, max(pcsresult$timestep)))
}

pcs_viz(bla)



# palette <- rep(c("red", "blue", "magenta"), 2)
palette <- c("red", "blue", "magenta", "darkred", "cornflowerblue", "blueviolet")
