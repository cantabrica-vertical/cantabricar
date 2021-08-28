library(cantabricar)

d <- data.frame(id = 1, t_germinacion = rpois(10, 15), t_hojas = rpois(10, 15), t_cosecha = rpois(10, 15))

t <- tempdir()
f_null <- fit_model(type = "germinacion", response = "t_germinacion", data = NULL, refresh = 0)
f <- fit_model(type = "germinacion", response = "t_germinacion", data = d, refresh = 0)

test_that("Models can be fit with no observations", {
    expect_error(fit_model(type = "germinacion", response = "t_germinacion", data = NULL, save = NULL, refresh = 0), NA)
    expect_error(fit_model(type = "hojas", response = "t_hojas", data = NULL, save = NULL, refresh = 0), NA)
    expect_error(fit_model(type = "cosecha", response = "t_cosecha", data = NULL, save = NULL, refresh = 0), NA)
})

test_that("Models can be saved", {
    expect_error(fit_model(type = "germinacion", response = "t_germinacion", data = NULL, refresh = 0, save = paste0(t, "/f1_save.rds")), NA)
    expect_error(fit_model(type = "hojas", response = "t_hojas", data = NULL, refresh = 0, save = paste0(t, "/f2_save.rds")), NA)
    expect_error(fit_model(type = "cosecha", response = "t_cosecha", data = NULL, refresh = 0, save = paste0(t, "/f3_save.rds")), NA)

})

test_that("Models can be fit with data", {
    expect_error(fit_model(type = "germinacion", response = "t_germinacion", data = d, refresh = 0), NA)
    expect_error(fit_model(type = "hojas", response = "t_hojas", data = d, refresh = 0), NA)
    expect_error(fit_model(type = "cosecha", response = "t_cosecha", data = d, refresh = 0), NA)
})

test_that("Diagnostics are extracted without errors", {
    expect_error(get_diagnostics(f_null), NA)
    expect_error(get_diagnostics(f), NA)
})

test_that("LOO is computed without errors", {
    expect_true("loo" %in% class(get_loo(f)))
    expect_null(get_loo(f_null))
})

test_that("Draws can be extracted without errors", {
    expect_true("data.frame" %in% class(get_draws(f)))
    expect_true("data.frame" %in% class(get_draws(f_null)))
})

test_that("Posterior predictions can be computed without errors", {
    expect_true("ggplot" %in% class(get_post_preds(f)))
    expect_null(get_post_preds(f_null))
})

test_that("Prior predictions can be computed without errors", {
    expect_true("ggplot" %in% class(get_prior_preds(f)))
    expect_null(get_prior_preds(f_null))
})

test_that("Posterior distributions can be plotted without errors", {
    expect_true("ggplot" %in% class(plot_model(f)))
    expect_true("ggplot" %in% class(plot_model(f_null)))
})

