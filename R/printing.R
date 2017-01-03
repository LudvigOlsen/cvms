print_boxplot_ = function(data, var_start, var_end=NULL, plot_theme=theme_bw()){

  # Data: gaussian_return or likewise
  # Vars (Variables to plot):
  # .. var_start: first in the var range (e.g. columns 1:3)
  # .. var_end: last in the var range (e.g. columns 1:3)
  # plot_theme: The theme for the plot

  # gather() data by the chosen range of vars
  # .. This leaves us with a column for the chosen measure variables
  # .. and a column for the associated values
  # If only var_start is given, it means that we only want to plot var_start

  if (is.null(var_end)){
    data = data %>%
      tidyr::gather(key, value, var_start)

  } else {

    data = data %>%
      tidyr::gather(key, value, var_start:var_end)
  }

  # Create and print plot
  gg = ggplot2::ggplot(data, aes(key, value))
  print(gg +
          ggplot2::geom_boxplot() +
          ggplot2::labs(x = 'Measure', y = 'Value') +
          plot_theme)


}
