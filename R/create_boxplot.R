
create_boxplot_ <- function(data, var_start = NULL, var_end = NULL) {

  # Data: gaussian_return or likewise
  # Vars (Variables to plot):
  # .. var_start: first in the var range (e.g. columns 1:3)
  # .. var_end: last in the var range (e.g. columns 1:3)

  # gather() data by the chosen range of vars
  # .. This leaves us with a column for the chosen measure variables
  # .. and a column for the associated values
  # If only var_start is given, it means that we only want to plot var_start

  ### I encountered a (likely) non-standard-evaluation error after
  ### Hadley updated his packages. It seems I don't need to specify
  ### columns though, as only the columns to use are in the data passed
  ### But leaving here to fix in the future
  # print(var_start)
  # if (is.null(var_end)){
  #   data <- data %>% print() %>%
  #     tidyr::gather(key, value, var_start)
  #
  # } else {
  #
  #   data <- data %>%
  #     tidyr::gather(key, value, var_start:var_end)
  # }

  # Fix for now
  data <- data %>%
    tidyr::gather(key = "key", value = "value")

  # Create and print plot
  gg <- ggplot2::ggplot(data, ggplot2::aes(.data$key, .data$value)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Measure", y = "Value")

  return(gg)
}
