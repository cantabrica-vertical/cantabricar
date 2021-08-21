# fit models

#' Fit germinacion model
#' @import cmdstanr
#' @param type Type of data to fit. One of "germinacion", "hojas", or "cosecha".
#' @param data Data frame with a row for each plant and at least one column with the germination days
#' @param lambda_prior Prior distribution of the lambda parameter in the Poisson distribution used to model germination days
#' @param save Path where the CmdStanFit object should be saved (as a RDS file)
#' @param ... Arguments to be passed to the \code{$sample} operator
fit_model <- function(
    type,
    data = NULL,
    lambda_prior_alpha = 15,
    lambda_prior_beta = 1,
    save = NULL,
    ...
){
    if (is.null(data)) data <- data.frame(y = double())
    if (type=="germinacion") stan_code <- system.file("stan/germinacion.stan", package = "cantabricar")
    if (type=="hojas") stan_code <- system.file("stan/hojas.stan", package = "cantabricar")
    if (type=="cosecha") stan_code <- system.file("stan/cosecha.stan", package = "cantabricar")
    data_stan <- list(N = nrow(data), y = data$y, lambda_prior_alpha = lambda_prior_alpha, lambda_prior_beta = lambda_prior_beta)
    mod <- cmdstan_model(stan_code)
    fit <- mod$sample(data = data_stan, ...)
    if (!is.null(save)) fit$save_object(file = save)
    return(fit)
}


#' Diagnose germination model using CmdStan diagnostics
#' @import cmdstanr
#' @param fit CmdStanFit object (output from \code{fit_germination})
get_diagnostics <- function(fit){
    fit$cmdstan_diagnose()
}

#' Leave-one-out cross-validation using
#' @import cmdstanr
#' @param fit CmdStanFit object (output from \code{fit_germination})
get_loo <- function(fit){
    fit$loo()
}

#' Get posterior draws
#' @import cmdstanr
#' @importFrom posterior as_draws_df
#' @importFrom tidyr pivot_longer
#' @param fit CmdStanFit object (output from \code{fit_germination})
get_draws <- function(
    fit
){
    suppressWarnings({
        draws <- fit$draws() %>%
            as_draws_df() %>%
            pivot_longer(
                -one_of(".chain", ".iteration", ".draw"),
                names_to = "variable",
                values_to = "value"
            )

        return(draws)
    })
}

#' Get posterior predictions
#' @import cmdstanr
#' @importFrom rjson fromJSON
#' @importFrom posterior as_draws_matrix
#' @importFrom bayesplot ppc_dens_overlay
#' @param fit CmdStanFit object (output from \code{fit_germination})
#' @param n Number of samples per observation
get_post_preds <- function(
    fit,
    n = 20
){
    library(cmdstanr)
    library(rjson)
    data <- fromJSON(paste(readLines(fit$data_file()), collapse=""))
    draws <- fit$draws()
    preds <- draws[,,dimnames(draws)$variable[grepl("y_rep", dimnames(draws)$variable)]] %>%
        as_draws_matrix()
    preds$
        ppc_dens_overlay(y = data$y, yrep = preds[1:n, 0:data$N])
}

#' Get prior predictions
#' @import cmdstanr
#' @importFrom rjson fromJSON
#' @importFrom posterior as_draws_matrix
#' @importFrom bayesplot ppc_dens_overlay
#' @param fit CmdStanFit object (output from \code{fit_germination})
#' @param n Number of samples per observation
get_prior_preds <- function(
    fit,
    n = 20
){
    library(cmdstanr)
    library(rjson)
    data <- fromJSON(paste(readLines(fit$data_file()), collapse=""))
    draws <- fit$draws()
    preds <- draws[,,dimnames(draws)$variable[grepl("y_sim", dimnames(draws)$variable)]] %>%
        as_draws_matrix()
    ppc_dens_overlay(y = data$y, yrep = preds[1:n, 0:data$N])
}

#' Plot MCMC posterior draws as a histogram
#' @import cmdstanr
#' @import ggplot2
#' @importFrom dplyr mutate
#' @param draws Posterior draws from a CmdStanFit object (output from \code{draws_germination})
plot_model <- function(
    fit
){

    mcmc_hist(fit$draws(), "lambda", "lambda_sim")$data %>%
        mutate(
            parameter = ifelse(grepl("_sim", Parameter), "Previa", "Posterior"),
            parameter = factor(parameter, levels = c("Previa", "Posterior"))
        )%>%
        ggplot(aes(value)) +
        annotate(geom = "rect", xmin = fit$summary()$q5[2], xmax = fit$summary()$q95[2], ymin = -Inf, ymax = Inf,
                 colour = "white", fill = "grey", alpha = 0.5) +
        geom_histogram(
            aes(fill = parameter, colour = parameter),
            colour = "white", alpha = 0.75, bins = 30, position = position_identity()) +
        geom_vline(xintercept = fit$summary()$median[2], colour = "black") +
        labs(
            x = expression(lambda),
            y = "Muestras",
            colour = "Distribuci\u00f3n",
            fill = "Distribuci\u00f3n",
            parse = TRUE
        ) +
        scale_color_brewer(palette = "Dark2") +
        scale_fill_brewer(palette = "Dark2") +
        theme_minimal() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            text = element_text(colour = "black", size = 12),
            axis.text = element_text(colour = "black", size = 12),
            axis.title = element_text(colour = "black", size = 15)
        )
}
