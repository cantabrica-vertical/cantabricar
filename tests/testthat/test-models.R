library(cantabricar)

d <- data.frame(
    id = 1:10,
    especie = sample(c("Tomillo", "Albahaca"), 10, replace = TRUE),
    t_germinacion = rpois(10, 15),
    t_hojas = rpois(10, 15),
    t_cosecha = rpois(10, 15)
)

cmdstanr::set_cmdstan_path()

temp_dir <- tempdir()
f <- fit_model(
    type = "germinacion", y = "t_germinacion", data = d,
    rds_path = file.path(temp_dir, "germinacion1.rds"))

test_that("Models can be fit", {
    expect_error(
        fit_model(
            type = "germinacion", y = "t_germinacion", data = d,
            rds_path = file.path(temp_dir, "germinacion2.rds")
        ),
        NA
    )
    expect_error(
        fit_model(
            type = "hojas", y = "t_hojas", data = d,
            rds_path = file.path(temp_dir, "hojas1.rds")
        ),
        NA
    )
    expect_error(
        fit_model(
            type = "cosecha", y = "t_cosecha", data = d,
            rds_path = file.path(temp_dir, "cosecha1.rds")
        ),
        NA
    )})


test_that("Posterior distributions can be plotted without errors", {
    expect_true("ggplot" %in% class(plot_model(f)))
})

