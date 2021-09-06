# fit models

#' Fit germinacion model
#' @export fit_model
#' @importFrom brms brm
#' @importFrom brms prior
#' @importFrom brms save_pars
#' @param type Type of data to fit. One of "germinacion", "hojas", or "cosecha"
#' @param y Response variable name in data
#' @param especie Especie variable name in data
#' @param data Data frame with a row for each plant and at least one
#' column with the germination days
#' @returns A brmsfit object.
fit_model <- function(
    type = NULL,
    y = "y",
    especie = "especie",
    data
) {
    if (!(type %in% c("germinacion", "hojas", "cosecha"))) {
        stop("type must be one of germinacion, hojas, and cosecha")
    }
    data$y <- round(as.numeric(gsub(" days", "", t(data[, y])[1,])))

    especie <- data[, especie]
    prior <- c(
        prior(normal(15, 1), "b"),
        prior(exponential(4), "sd")
    )
    x <- brm(
        formula = y ~ 1 + (1 | especie),
        data = data,
        family = poisson,
        backend = "cmdstanr",
        chains = 1,
        cores = 1,
        inits = 0,
        iter = 1000,
        file = system.file("RDS", paste0("fit_", type, ".rds"), package = "cantabricar"),
        file_refit = "always",
        save_model = system.file("Stan", "model.stan", package = "cantabricar"),
        save_pars = save_pars("all")
    )

    return(x)
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
        rhat = rhat(fit),
        bayes_R2 = bayes_R2(fit),
        loo_R2 = loo_R2(fit),
        loo = loo(fit)
    )
    return(x)
}

#' Get posterior draws
#' @export get_draws
#' @importFrom tidybayes gather_draws
#' @importFrom brms fixef
#' @param fit brmsfit object (output from \code{fit_germination})
get_draws <- function(
    fit
) {
    x <- gather_draws(fit, r_especie[especie, param], regex = TRUE)
    x$.value <- x$.value + fixef(fit)[1]
    x$.chain <- as.factor(x$.chain)
    return(x)
}

#' Check posterior predictions
#' @export get_post_preds_checks
#' @importFrom brms pp_check
#' @param fit brmsfit object (output from \code{fit_germination})
#' @param n Number of samples per observation
get_post_preds_checks <- function(
    fit,
    n = 20
) {
    x <- pp_check(fit, ndraws = n) +
        theme_custom()
    return(x)
}

#' Plot MCMC posterior draws as a histogram
#' @export plot_model
#' @import cmdstanr
#' @import ggplot2
#' @importFrom brms fixef
#' @importFrom tidybayes mean_qi
#' @param draws ggplot with the 95% credible interval of the day count for each species,
#' scaled to Intercept
#' (output from \code{draws_germination})
plot_model <- function(
    fit
) {

    get_draws(fit) %>%
        mean_qi() %>%
        ggplot(
            aes(.value, especie, xmin = .lower, xmax = .upper,
            colour = especie, fill = especie)
        ) +
        geom_vline(
            xintercept = fixef(fit)[1], colour = "black",
            linetype = "dashed"
        ) +
        geom_errorbar(size = 1, width = 0) +
        geom_point(size = 4) +
        labs(
            x = "Días",
            y = "Especie",
            colour = "Especie",
            fill = "Especie",
            parse = TRUE
        ) +
        scale_color_brewer(palette = "Dark2") +
        scale_fill_brewer(palette = "Dark2") +
        theme_custom() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            text = element_text(colour = "black", size = 12),
            axis.text = element_text(colour = "black", size = 12),
            axis.title = element_text(colour = "black", size = 15)
        )
}
