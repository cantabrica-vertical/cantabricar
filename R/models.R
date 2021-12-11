# fit models

#' Fit germinacion model
#' @export fit_model
#' @importFrom brms brm
#' @importFrom brms prior_string
#' @importFrom brms save_pars
#' @importFrom brms bf
#' @param type type of data to fit. One of "germinacion", "hojas", or "cosecha"
#' @param y response variable name in data
#' @param especie especie variable name in data
#' @param data data frame with a row for each plant and at least one
#' @param rds_path character string indicating the path in which to save the brmsfit object
#' column with the germination days
#' @returns a brmsfit object
fit_model <- function(type, y = "y", especie = "especie", data, rds_path = NULL) {

    # create data
    data$y <- round(as.numeric(gsub(" days", "", t(data[, y])[1,])))

    # model priors
    prior <- c(prior_string("normal(15, 1)", "b"), prior_string("exponential(4)", "sd"))

    # model formula
    form <- bf(y ~ 1 + (1 | especie))

    # path to save RDS objects
    if (is.null(rds_path)) {
        rds_path <- file.path(
            system.file("rds", package = "cantabricar"),
            paste0(format(Sys.Date(), "%Y-%m-%d_fit-"), type, ".rds")
        )
    }

    # path to save Stan code
    stan_path <- file.path(
        system.file("stan", package = "cantabricar"),
        paste0(type, ".stan")
    )

    # fit model
    fit <- brm(
        formula = form, data = data, family = "poisson",
        backend = "cmdstanr", chains = 1, cores = 1, inits = 0, iter = 1000,
        file_refit = "always", file = rds_path, save_model = stan_path,
        save_pars = save_pars("all")
    )

    return(fit)
}

#' Diagnose germination model
#' @export get_diagnostics
#' @importFrom brms rhat
#' @importFrom brms bayes_R2
#' @importFrom brms loo_R2
#' @importFrom brms loo
#' @param fit brmsfit object (output from \code{fit_germination})
get_diagnostics <- function(fit){
    x <- list(
        rhat = rhat(.env$fit), # Gelman-Rubin diagnostic of MCMC chain convergence
        bayes_R2 = bayes_R2(.env$fit), # Bayesian R-squared estimate
        loo_R2 = loo_R2(.env$fit), # leave-one-out cross-validation R-squared estimate
        loo = loo(.env$fit) # leave-one-out cross-validation estimate
    )
    return(x)
}

#' Get posterior draws
#' @export get_draws
#' @importFrom tidybayes gather_draws
#' @importFrom brms fixef
#' @param fit brmsfit object (output from \code{fit_germination})
get_draws <- function(fit) {
    x <- gather_draws(fit, `r_especie`["especie", "param"], regex = TRUE)
    x$.value <- x$.value + fixef(fit)[1]
    x$.chain <- as.factor(x$.chain)
    return(x)
}

#' Check posterior predictions
#' @export get_post_preds_checks
#' @importFrom brms pp_check
#' @param fit brmsfit object (output from \code{fit_germination})
#' @param n Number of samples per observation
get_post_preds_checks <- function(fit, n = 20) {
    x <- pp_check(fit, ndraws = n) + theme_custom()
    return(x)
}

#' Plot MCMC posterior draws as a histogram
#' @export plot_model
#' @import cmdstanr
#' @import ggplot2
#' @importFrom brms fixef
#' @importFrom tidybayes mean_qi
#' @param fit model fit, as returned by \code{fit_model}
#' (output from \code{draws_germination})
plot_model <- function(fit) {

    # get posterior draws
    draws <- mean_qi(get_draws(fit))

    # plot draws
    ggplot(draws) +
        aes(.data$.value, .data$especie, xmin = .data$.lower, xmax = .data$.upper, colour = .data$especie, fill = .data$especie) +
        geom_vline(xintercept = fixef(fit)[1], colour = "black", linetype = "dashed") +
        geom_errorbar(size = 1, width = 0) +
        geom_point(size = 4) +
        labs(x = "D\u00edas", y = "Especie", colour = "Especie", fill = "Especie", parse = TRUE) +
        theme_custom() +
        theme(
            legend.position = "none",
            legend.title = element_blank(),
            text = element_text(colour = "black", size = 12),
            axis.text = element_text(colour = "black", size = 12),
            axis.title = element_text(colour = "black", size = 15)
        )
}
