

#' get_data 
#' @description
#' fetches the data used for the assignment
#' @export
#' @returns tibble
get_data <- function() {
  res <- readr::read_delim(
    file = here::here("stock_data (1).csv") ,
    delim = ","
  )  |> 
    dplyr::arrange(Date)  |> 
    dplyr::mutate(
      GOOG =     log(GOOG) - log(dplyr::lag(GOOG)),
      MSFT =     log(MSFT) - log(dplyr::lag(MSFT)),
      MRK =     log(MRK) - log(dplyr::lag(MRK)),
      IDU =     log(IDU) - log(dplyr::lag(IDU))
    )  |> 
    na.omit()
  return(res)
}




#' fit t distribution plot
#' @description
#' function for fitting t distribution and returning plot
#' @param data
#' @export
#' @returns ggplot2
t.dist.plot <- function(
  ticker
) {
  data = get_data()
  fit_dat <- data  |> dplyr::select( !!ticker)  |> dplyr::pull(!!ticker)
  fit <- MASS::fitdistr(x = fit_dat , "t")
  m <- fit$estimate["m"]
  s <- fit$estimate["s"]
  df <- fit$estimate["df"]

  plot_dat <- data  |> dplyr::select( !!ticker)  
  plot <- ggplot2::ggplot(data = plot_dat, ggplot2::aes(x = plot_dat[[ticker]]))  + 
    ggplot2::stat_ecdf(
      geom = "step", 
      color = "steelblue", 
      linewidth = 1
    ) + 
      ggplot2::stat_function(
        fun = function(x) { stats::pt((x - m )/ s , df = df)},
        color = "darkred", 
        linewidth = 1, 
        linetype = "dashed"
      ) + 
        ggplot2::theme_minimal() + 
        ggplot2::labs(
      title = paste("Empirical vs. Theoretical CDF for", ticker),
      subtitle = paste0("Location (\u03bc): ", round(m, 4), 
                        " | Scale (\u03c3): ", round(s, 4), 
                        " | df (\u03bd): ", round(df, 2)),
      x = "Log-Returns",
      y = "Cumulative Probability",
      caption = "Solid Blue: Empirical CDF | Dashed Red: Fitted Student-t"
    )

    ks_test <- stats::ks.test((data$GOOG - m) / s,  y = "pt", df = df)

    return(list(plot = plot,ks_test =  ks_test))
}