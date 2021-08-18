# fit models

#' Fit germination model
#' @import cmdstanr
#' @param stan_code File containing the Stan code of the model
#' @param data Data frame with a row for each plant and at least one column with the germination days
#' @param lambda_prior Prior distribution of the lambda parameter in the Poisson distribution used to model germination days
#' @param save Path where the CmdStanFit object should be saved (as a RDS file)
#' @param ... Arguments to be passed to the \code{$sample} operator
fit_germination <- function(
    stan_code,
    data = NULL,
    lambda_prior = 1,
    save = NULL,
    ...
){
    if (is.null(data)) data <- data.frame(y = double())
    data_stan <- list(N = nrow(data), y = data$y, lambda_prior = 1)
    mod <- cmdstan_model(stan_code)
    fit <- mod$sample(data = data_stan, ...)
    if (!is.null(save)) fit$save_object(file = save)
    return(fit)
}


#' Diagnose germination model using CmdStan diagnostics
#' @import cmdstanr
#' @param fit CmdStanFit object (output from \code{fit_germination})
diagnose_germination <- function(fit){
    fit$cmdstan_diagnose()
}

#' Leave-one-out cross-validation using
#' @import cmdstanr
#' @param fit CmdStanFit object (output from \code{fit_germination})
loo_germination <- function(fit){
    fit$loo()
}

#' Get posterior draws
#' @import cmdstanr
#' @importFrom posterior as_draws_df
#' @importFrom tidyr pivot_longer
#' @param fit CmdStanFit object (output from \code{fit_germination})
draw_germination <- function(
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
post_preds_germination <- function(
    fit,
    n = 20
){
    library(cmdstanr)
    library(rjson)
    data <- fromJSON(paste(readLines(fit$data_file()), collapse=""))
    draws <- fit$draws()
    preds <- draws[,,dimnames(draws)$variable[grepl("y_rep", dimnames(draws)$variable)]] %>%
        as_draws_matrix()
    ppc_dens_overlay(y = data$y, yrep = preds[1:n, 0:data$N])
}

#' Get prior predictions
#' @import cmdstanr
#' @importFrom rjson fromJSON
#' @importFrom posterior as_draws_matrix
#' @importFrom bayesplot ppc_dens_overlay
#' @param fit CmdStanFit object (output from \code{fit_germination})
#' @param n Number of samples per observation
prior_preds_germination <- function(
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
plot_germination <- function(
    draws
){

    mcmc_hist(fit$draws(), "lambda", "lambda_sim")$data %>%
        mutate(
            parameter = ifelse(grepl("_sim", Parameter), "Previa", "Posterior"),
            parameter = factor(parameter, levels = c("Previa", "Posterior"))
        )%>%
        ggplot(aes(value, fill = parameter, colour = parameter)) +
        geom_histogram(colour = "white", alpha = 0.75, bins = 30, position = position_identity()) +
        labs(
            title = "D\u00edas hasta germinaci\u00f3n",
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
