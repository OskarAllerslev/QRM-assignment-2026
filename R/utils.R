

#' get_data
#' @description
#' fetches the data used for the assignment
#' @export
#' @returns tibble
get_data <- function() {
  res <- readr::read_delim(
    file = here::here("stock_data (1).csv"),
    delim = ","
  ) |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      GOOG = (log(GOOG) - log(dplyr::lag(GOOG))) * -1,
      MSFT = (log(MSFT) - log(dplyr::lag(MSFT))) * -1,
      MRK = (log(MRK) - log(dplyr::lag(MRK))) * -1,
      IDU = (log(IDU) - log(dplyr::lag(IDU))) * -1
    ) |>
    na.omit()
  return(res)
}


#' fit t distribution plot
#' @description
#' function for fitting t distribution and returning plot
#' @param data data
#' @export
#' @returns ggplot2
t.dist.plot <- function(
  ticker
) {
  data = get_data()
  fit_dat <- data |> dplyr::select(!!ticker) |> dplyr::pull(!!ticker)
  fit <- MASS::fitdistr(x = fit_dat, "t")
  m <- fit$estimate["m"]
  s <- fit$estimate["s"]
  df <- fit$estimate["df"]

  plot_dat <- data |> dplyr::select(!!ticker)
  plot <- ggplot2::ggplot(
    data = plot_dat,
    ggplot2::aes(x = plot_dat[[ticker]])
  ) +
    ggplot2::stat_ecdf(
      geom = "step",
      color = "steelblue",
      linewidth = 1
    ) +
    ggplot2::stat_function(
      fun = function(x) {
        stats::pt((x - m) / s, df = df)
      },
      color = "darkred",
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Empirical vs. Theoretical CDF for", ticker),
      subtitle = paste0(
        "Location (\u03bc): ",
        round(m, 4),
        " | Scale (\u03c3): ",
        round(s, 4),
        " | df (\u03bd): ",
        round(df, 2)
      ),
      x = "Log-Returns",
      y = "Cumulative Probability",
      caption = "Solid Blue: Empirical CDF | Dashed Red: Fitted Student-t"
    )

  ks_test <- stats::ks.test((fit_dat - m) / s, y = "pt", df = df)

  return(list(plot = plot, ks_test = ks_test, fit = fit))
}


#' mean.excess
#' @export
#' @returns tibble
mean.excess <- function(
  u,
  ticker
) {
  res <- get_data() |>
    dplyr::select(dplyr::all_of(ticker)) |>
    dplyr::filter(.data[[ticker]] > u) |>
    dplyr::summarise(mean = mean(.data[[ticker]] - u)) |>
    dplyr::pull(mean)

  if (is.nan(res)) {
    return(NA_real_)
  }

  return(res)
}


#' mean.excess.plot
#' @export
#' @returns ggplot2
mean.excess.plot <- function(ticker) {
  data <- get_data() |> tidyr::drop_na()
  min_val <- 0
  max_val <- quantile(data[[ticker]], 0.975)

  sequence <- dplyr::tibble(value = seq(min_val, max_val, length.out = 100)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      me = mean.excess(
        u = value,
        ticker = ticker
      )
    ) |>
    dplyr::ungroup()

  res <- ggplot2::ggplot(
    data = sequence,
    mapping = ggplot2::aes(x = value, y = me)
  ) +
    ggplot2::geom_point(color = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Mean Excess Plot for", ticker),
      x = "Threshold (u)",
      y = "Mean Excess e(u)"
    )

  return(res)
}


#' compare.tail
#'
#' @param u threshold
#' @param ticker ticker
#' @export
#' @returns plot
compare.tail <- function(
  u,
  ticker
) {
  t_fit <- t.dist.plot(ticker = ticker)$fit
  m <- t_fit$estimate["m"]
  s <- t_fit$estimate["s"]
  df <- t_fit$estimate["df"]

  data <- get_data() |>
    dplyr::select(!!ticker)

  emp_data <- data |>
    dplyr::filter(.data[[ticker]] > u)

  gpd_fit <- evir::gpd(
    data = data |> dplyr::pull(ticker),
    threshold = u,
    method = "ml"
  )
  xi <- gpd_fit$par.ests["xi"]
  beta <- gpd_fit$par.ests["beta"]

  res <- ggplot2::ggplot(data = emp_data, ggplot2::aes(x = .data[[ticker]])) +
    ggplot2::stat_ecdf(
      geom = "step",
      color = "black",
      linewidth = 1.2
    ) +
    ggplot2::stat_function(
      fun = function(x) {
        1 - (1 + xi * (x - u) / beta)^(-1 / xi)
      },
      color = "darkred",
      linewidth = 1.2,
      linetype = "dotdash"
    ) +
    ggplot2::stat_function(
      fun = function(x) {
        F_u <- pt((u - m) / s, df = df)
        F_x <- pt((x - m) / s, df = df)
        (F_x - F_u) / (1 - F_u)
      },
      color = "steelblue",
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Conditional Tail Distribution for", ticker),
      subtitle = paste("Threshold u =", u),
      x = "Log-Returns *-1 (x > u)",
      y = "CDF",
      caption = "Black: Data | Red: GPD | Blue: Conditional Student-t"
    )
}



#' multi_t_fit 
#' @export
#' @returns tibble
multi_t_fit <- function(
  portfolio_data
) {
  port.1.fit <- QRM::fit.mst(
    data = portfolio_data |> as.matrix()
  )

  sim.1 <- QRM::rmt(
    n = 2302,
    df = port.1.fit$df,
    mu = port.1.fit$mu,
    Sigma = port.1.fit$Sigma
  )
  simulated <- tibble::tibble(
    sim1 = sim.1[, 1],
    sim2 = sim.1[, 2]
  )
  res <- list(
    simulated = simulated, 
    fit = port.1.fit
  )

  return(res)
}


#' get.pseudo.obs
#' 
#' @param ticker ticker symbol for the asset 
#' @param u chosen threshold for the upper tail
#' @export
#' @returns numeric vector of uniform pseudo-observations
get.pseudo.obs <- function(
  ticker, 
  u
) {
  data <- get_data() 
  x <- data |> dplyr::pull(!!ticker)
  n <- length(x)

  x_tail <- x[x > u]
  Nu <- length(x_tail)
  
  gpd_fit <- evir::gpd(data = x, threshold = u, method = "ml")
  gamma <- gpd_fit$par.ests["xi"]
  beta  <- gpd_fit$par.ests["beta"]
  
  emp_cdf <- ecdf(x)
  
  U <- dplyr::if_else(
    condition = x <= u,
    true  = emp_cdf(x), 
    false = 1 - (Nu / n) * (1 + gamma * (x - u) / beta)^(-1 / gamma)
  )
  
  U <- pmin(pmax(U, 1e-6), 1 - 1e-6)
  
  return(U)
}



#' UpperTailDependence 
#' @export
#' @returns float
UpperTailDependence <- function(
  quantile_for_uniform = 0.95, 
  copula_data
) {
  n <- nrow(copula_data)
  
  Cuu <- copula_data  |> dplyr::filter(
    dplyr::if_all(dplyr::everything(), ~ .x <= quantile_for_uniform)
  )   |> 
    nrow()
  C <- Cuu / n
  tæller <- (1 - 2 * quantile_for_uniform) +  C
  nævner <- (1- quantile_for_uniform)
  return(tæller / nævner)
  
}





var_eliptical <- function(
  fit, 
  w = c(10000L, 10000L), 
  alpha = 0.9999 
) {
  df <- fit$df
  mu <- fit$mu
  Sigma <- fit$Sigma

  wt_mm_mu <- (as.numeric(t(w) %*% mu)  ) 
  wt.mm.sigma.mm.mu <- as.numeric(t(w) %*% Sigma %*% w)
  rho_y <- stats::qt(p = alpha, df = df)

  res <- wt_mm_mu + wt.mm.sigma.mm.mu * rho_y
  return(res)
}

inverse_semi_parametric <- function(p, ticker_name, u_thresh) {
  data_vec <- data |> dplyr::pull(!!ticker_name)
  n <- length(data_vec)
  x_tail <- data_vec[data_vec > u_thresh]
  Nu <- length(x_tail)
  gpd_fit <- evir::gpd(data = data_vec, threshold = u_thresh, method = "ml")
  gamma <- gpd_fit$par.ests["xi"]
  beta  <- gpd_fit$par.ests["beta"]
  prob_u <- 1 - (Nu / n)
  res <- numeric(length(p))
  idx_emp <- which(p <= prob_u)
  if (length(idx_emp) > 0) {
    res[idx_emp] <- stats::quantile(data_vec, probs = p[idx_emp], type = 1)
  }
  idx_gpd <- which(p > prob_u)
  if (length(idx_gpd) > 0) {
    res[idx_gpd] <- u_thresh + (beta / gamma) * (((1 - p[idx_gpd]) / (Nu / n))^(-gamma) - 1)
  }
  return(res)
}