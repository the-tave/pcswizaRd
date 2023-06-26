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
  # sample_vector contains a vector of 1 and 0, which represents the settled state of a pcs model
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
