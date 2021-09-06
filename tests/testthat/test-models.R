library(cantabricar)

d <- data.frame(
    id = 1:10,
    especie = sample(c("Tomillo", "Albahaca"), 10, replace = TRUE),
    t_germinacion = rpois(10, 15),
    t_hojas = rpois(10, 15),
    t_cosecha = rpois(10, 15)
)

t <- tempdir()
f <- fit_model(type = "germinacion", y = "t_germinacion", data = d)

test_that("Models can be fit", {
    expect_error(fit_model(type = "germinacion", y = "t_germinacion", data = d), NA)
    expect_error(fit_model(type = "hojas", y = "t_hojas", data = d), NA)
    expect_error(fit_model(type = "cosecha", y = "t_cosecha", data = d), NA)
})

test_that("Diagnostics are extracted without errors", {
    expect_error(get_diagnostics(f), NA)
})

test_that("LOO is computed without errors", {
    expect_true("loo" %in% class(get_loo(f)))
    expect_null(get_diagnostics(f))
})

test_that("Draws can be extracted without errors", {
    expect_true("data.frame" %in% class(get_draws(f)))
})

test_that("Posterior predictions can be computed without errors", {
    expect_true("ggplot" %in% class(get_post_preds_checks(f)))
})

test_that("Posterior distributions can be plotted without errors", {
    expect_true("ggplot" %in% class(plot_model(f)))
})

