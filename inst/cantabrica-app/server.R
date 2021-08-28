shinyServer(
  function(input, output, session) {
    output$sidebar <- renderMenu({
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Calendario", tabName = "calendario", icon = icon("calendar")),
        menuItem("Base de datos", tabName = "datos", icon = icon("database")),
        menuItem("Estimaciones", tabName = "estimaciones", icon = icon("chart-bar")),
        menuItem("A\u00f1adir datos",  icon = icon("plus"),
                 menuSubItem("Siembra", icon = icon("tint"), tabName = "siembra"),
                 menuSubItem("Germinaci\u00f3n", icon = icon("seedling"), tabName = "germinacion"),
                 menuSubItem("Trasplantes", icon = icon("truck"), tabName = "trasplante"),
                 menuSubItem("Hojas", icon = icon("leaf"), tabName = "hojas"),
                 menuSubItem("Cosecha", icon = icon("tractor"), tabName = "cosecha"),
                 startExpanded = TRUE),
        menuItem("Azure", icon = icon("server"), href = "https://portal.azure.com/#home"),
        menuItem("GitHub", icon = icon("github"), href = "https://github.com/cantabrica-vertical"),
        menuItem("Slack", icon = icon("slack"), href = "https://cantabrica.slack.com/home"),
        menuItem("Instagram", icon = icon("instagram"),href = "https://www.instagram.com/cantabricagr/"),
        menuItem("Twitter", icon = icon("twitter"), href = "https://twitter.com/cantabricagr"),
        menuItem("LinkedIn", icon = icon("linkedin"), href = "https://www.linkedin.com/company/cantabricagr/")
      )
    })

    output$notifications <- renderMenu({
      dropdownMenu(type = "tasks", headerText = "Activas", badgeStatus = "info",
                   taskItem(value = round(100*length(active_ids()$germinadas)/length(active_ids()$sembradas), 2), color = "green", text = "Germinadas"),
                   taskItem(value = round(100*length(active_ids()$hojas)/length(active_ids()$sembradas), 2), color = "yellow", text = "Con hojas"),
                   taskItem(value = round(100*length(active_ids()$trasplantadas)/length(active_ids()$sembradas), 2), color = "red", text = "Trasplantadas")
      )
    })

    output$n_sembradas <- renderText({nrow(d$sembradas)})
    output$n_germinadas <- renderText({nrow(d$germinadas)})
    output$n_hojas <- renderText({nrow(d$hojas)})
    output$n_trasplantadas <- renderText({nrow(d$trasplantadas)})
    output$n_cosechadas <- renderText({nrow(d$cosechadas)})

    active_ids <- reactive({map(d, function(x) filter(x, !(id %in% d$cosechadas$id)) %>% pull(id) %>% sort())})
    inactive_ids <- reactive({map(d, function(x) filter(x, id %in% d$cosechadas$id) %>% pull(id) %>% sort())})
    vacias <- reactive({unique(get_bandejas_vacias(con, d, instalaciones, bandejas))})

    observe({
      updateTextInput(session, "database_siembra_id", value = max(as.numeric(active_ids()$sembradas[!vapply(active_ids()$sembradas, is.null, TRUE)]), na.rm = TRUE)+1, placeholder = max(as.numeric(active_ids()$sembradas[!vapply(active_ids()$sembradas, is.null, TRUE)]), na.rm = TRUE)+1)
      updateSelectInput(session, "database_germinacion_id", choices = active_ids()$sembradas)
      updateSelectInput(session, "database_hojas_id", choices = active_ids()$sembradas)
      updateSelectInput(session, "database_trasplante_id",choices = active_ids()$sembradas)
      updateSelectInput(session, "database_cosecha_id",choices = active_ids()$sembradas)
      updateSelectInput(session, "database_siembra_especie", choices = values$especie)
      updateSelectInput(session, "database_siembra_variedad", choices = values$variedad)
      updateSelectInput(session, "database_siembra_marca", choices = values$marca)
      updateSelectInput(session, "database_siembra_bandeja", choices = vacias())
      updateSelectInput(session, "database_siembra_medio_siembra", choices = values$medio_siembra)
      updateSelectInput(session, "database_siembra_planta_tipo", choices = values$planta_tipo)
      updateSelectInput(session, "database_eliminar_siembra_id", choices = active_ids()$sembradas)
      updateSelectInput(session, "database_eliminar_germinacion_id",choices = active_ids()$germinadas)
      updateSelectInput(session, "database_eliminar_trasplante_id", choices = active_ids()$trasplantadas)
      updateSelectInput(session, "database_eliminar_hojas_id", choices = active_ids()$hojas)
      updateSelectInput(session, "database_eliminar_cosecha_id", choices = inactive_ids()$cosechadas)
    })


    observeEvent(input$actualizar, {
      message("Updating...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })


    ## dashboard ----
    output$dashboard_bandejas_plot <- renderPlotly({
      x <- get_data_summary(con) %>%
        filter(!(id %in% d$cosechadas$id)) %>%
        mutate(
          status = case_when(
            !is.na(t_germinacion) ~ "Germinada",
            !is.na(t_hojas) ~ "Hojas verdaderas",
            TRUE ~ "Sembrada"
          )
        ) %>%
        right_join(
          expand_grid(
            estanteria = 1:instalaciones$max_estanterias,
            balda = 1:instalaciones$max_baldas,
            bandeja = 1:instalaciones$max_bandejas
          ),
          by = c("estanteria", "bandeja", "balda")
        ) %>%
        replace_na(replace = list(status = "Vacia")) %>%
        mutate(
          estanteria = paste0("Estanteria ", estanteria),
          texto = ifelse(
            status=="Vacia", "Bandeja vacia", paste0(
              "Bandeja ", balda, "-", bandeja, "\n",
              especie, " ", variedad, " (ID: ", id, ") \n",
              "Siembra: ", as_date(fecha_siembra), "\n",
              "Cosecha estimada: ", as_date(fecha_siembra+ceiling(fit_cosecha$summary()$median[2]))
            ))
        ) %>%
        ggplot(aes(bandeja, balda, fill = status, text = texto)) +
        facet_wrap(~estanteria, nrow = 2) +
        geom_tile(colour = "white", size = 1) +
        geom_text(aes(label = id)) +
        labs(x = "Bandejas", y = "Baldas", fill = "Fase") +
        coord_fixed() +
        scale_x_continuous(limits = c(0, instalaciones$max_bandejas+1), breaks = 1:instalaciones$max_bandejas) +
        scale_y_continuous(limits = c(0, instalaciones$max_baldas+1), breaks = 1:instalaciones$max_baldas) +
        theme_custom() +
        theme(
          legend.position = "top",
          axis.ticks = element_blank(),
          axis.text = element_text(size = 9),
          panel.grid = element_blank(),
          strip.text.y = element_text(angle = 0)
        )

      font <- list(size = 15, color = "white")
      label <- list(bgcolor = "#232F34", bordercolor = "transparent", font = font)
      y <- ggplotly(x, tooltip = "text") %>%
        style(hoverlabel = label) %>%
        partial_bundle()
      return(x)
    })

    output$dashboard_estanterias <- renderText({
      paste0("N\u00ba estanterias: ", instalaciones$max_estanterias)
    })

    output$dashboard_baldas <- renderText({
      paste0("N\u00ba baldas: ", instalaciones$max_baldas)
    })

    output$dashboard_bandejas <- renderText({
      paste0("N\u00ba bandejas: ", instalaciones$max_bandejas)
    })

    observeEvent(input$bandejas_add_estanteria, {
      message("Adding estanteria...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_estanteria(con, instalaciones)
      instalaciones <<- get_instalaciones(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$bandejas_remove_estanteria, {
      message("Removing estanteria...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      remove_estanteria(con, instalaciones)
      instalaciones <<- get_instalaciones(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$bandejas_add_balda, {
      message("Adding balda...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_balda(con, instalaciones)
      instalaciones <<- get_instalaciones(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$bandejas_remove_balda, {
      message("Removing balda...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      remove_balda(con, instalaciones)
      instalaciones <<- get_instalaciones(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$bandejas_add_bandeja, {
      message("Adding balda...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_bandeja(con, instalaciones)
      instalaciones <<- get_instalaciones(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$bandejas_remove_bandeja, {
      message("Removing bandeja...")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      remove_bandeja(con, instalaciones)
      instalaciones <<- get_instalaciones(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })



    output$dashboard_fechas_plot <- renderPlotly({
      x <- get_data_summary(con) %>%
        count(fecha_siembra,  especie) %>%
        group_by(especie) %>%
        mutate(n_cum = cumsum(n)) %>%
        ungroup() %>%
        ggplot(
          aes(fecha_siembra, n_cum, fill = especie, colour = especie,
              texto = paste0(especie, ": n=", n_cum, " (", fecha_siembra, ")"))
        ) +
        geom_line(aes(group = especie), size = 1) +
        labs(x = "Fecha de siembra", y = "# siembras", colour = "Especie") +
        theme_custom() +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          axis.text = element_text(size = 12)
        )
      return(x)
    })

    output$dashboard_species_plot <- renderPlotly({
      x <-  get_data_summary(con) %>%
        count(especie) %>%
        mutate(prop = n/sum(.$n)) %>%
        ggplot(aes(x = 1, y = prop, fill = especie, text = paste0(especie, " (n=", n, ", ", round(prop*100, 2), "%)"))) +
        geom_bar(stat = "identity") +
        labs(fill = "Especie", y = "%") +
        scale_x_continuous(limits = c(0.5, 1.5)) +
        theme_void() +
        theme(
          legend.position = "none"
        )
      return(x)
    })


    # calendario ----
    output$calendario <- renderCalendar({
      x <- get_data_summary(con) %>%
        select(id, especie, variedad, fecha_siembra) %>%
        mutate(
          start = fecha_siembra,
          end = fecha_siembra + ceiling(fit_cosecha$summary()$median[2]),
          title = paste0("ID: ", id, " (", especie, " ", variedad, ")"),
          body = paste("Tiempo de siembra-cosecha estimado para ID: ", id, "."),
          recurrenceRule = NA_character_,
          category = "time",
          location = "Cantabrica",
          bgColor = case_when(
            especie=="Rábano" ~ "#F8766D",
            especie=="Swisschard" ~ "#D39200",
            especie=="Guisante" ~ "#93AA00",
            especie=="Albahaca" ~ "#00BA38",
            especie=="Melisa" ~ "#00C19F",
            especie=="Pe-tsai" ~ "#00B9E3",
            especie=="Pak choi" ~ "#619CFF",
            especie=="Mizuna" ~ "#DB72FB",
            TRUE ~ "#000000"
          ),
          case_when(
            especie=="Rábano" ~ "#F8766D",
            especie=="Swisschard" ~ "#D39200",
            especie=="Guisante" ~ "#93AA00",
            especie=="Albahaca" ~ "#00BA38",
            especie=="Melisa" ~ "#00C19F",
            especie=="Pe-tsai" ~ "#00B9E3",
            especie=="Pak choi" ~ "#619CFF",
            especie=="Mizuna" ~ "#DB72FB",
            TRUE ~ "#000000"
          )
        ) %>%
        calendar(
          view = "month",
          useDetailPopup = TRUE,
          isReadOnly = TRUE,
          useNavigation = TRUE
        ) %>%
        cal_week_options(
          startDayOfWeek = 1,
          daynames = c("L", "M", "X", "J", "V", "S", "D")
        )

      return(x)
    })

    # datos ----
    output$database_table <- DT::renderDT({

      x <- get_data_summary(con) %>%
        mutate_at(vars(starts_with("t_")), function(x) ifelse(is.na(x), "-", x)) %>%
        mutate_at(vars(starts_with("fecha_")), as_datetime) %>%
        relocate(balda, bandeja, .after = id) %>%
        select(input$datos_cols) %>%
        arrange(desc(id))

      DT::datatable(
        x,
        extensions = c("Buttons", "Select"),
        rownames = FALSE,
        options = list(
          "copy", "print",
          pageLength = 50,
          autoWidth = FALSE,
          scrollX = TRUE,
          search.caseInsensitive = TRUE
        )
      )
    })

    output$descargar <- downloadHandler(
      filename = function(){ paste0("cantabrica_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".csv")},
      content = function(file){
        x <- d_all %>%
          mutate_at(vars(starts_with("t_")), function(x) ifelse(is.na(x), "-", x)) %>%
          mutate_at(vars(starts_with("fecha_")), as_datetime) %>%
          select(
            input$datos_cols
          ) %>%
          arrange(desc(fecha_siembra), desc(id))
        write.table(x, file = file, sep = ",", na = "", row.names = FALSE)
      }
    )

    # estimaciones ----
    observeEvent(input$ajustar_modelos, {
      message("Ajustando modelos")
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Germinaciones (1/3)")
      fit_germinacion <<- fit_model(
        data = d_all %>% drop_na(t_germinacion) %>% select(id, t_germinacion) %>% mutate(t_germinacion = as.integer(t_germinacion)) %>% rename(y = t_germinacion),
        type = "germinacion",
        save = system.file("RDS", "fit_germinacion.rds", package = "cantabricar"),
        lambda_prior_alpha = ceiling(fit_germinacion$summary()$mean[2]),
        lambda_prior_beta = 1
      )
      show_modal_spinner(spin = "semipolar", color = "Crimson", text = "Hojas verdaderas (2/3)")
      fit_hojas <<- fit_model(
        data = d_all %>% drop_na(t_hojas) %>% select(id, t_hojas) %>% mutate(t_hojas = as.integer(t_hojas)) %>% rename(y = t_hojas),
        type = "hojas",
        save = system.file("RDS", "fit_hojas.rds", package = "cantabricar"),
        lambda_prior_alpha = ceiling(fit_hojas$summary()$mean[2]),
        lambda_prior_beta = 1
      )
      show_modal_spinner(spin = "semipolar", color = "DarkOrange", text = "Cosechas (3/3)")
      fit_cosecha <<- fit_model(
        data = d_all %>% drop_na(t_cosecha) %>% select(id, t_cosecha) %>% mutate(t_cosecha = as.integer(t_cosecha)) %>% rename(y = t_cosecha),
        type = "cosecha",
        save = system.file("RDS", "fit_cosecha.rds", package = "cantabricar"),
        lambda_prior_alpha = ceiling(fit_cosecha$summary()$mean[2]),
        lambda_prior_beta = 1
      )
      remove_modal_spinner()
      session$reload()
    })

    ## germinacion
    output$estimaciones_germinacion_table <- renderDataTable({
      x <- fit_germinacion$summary()
      datatable(
        x,
        rownames = FALSE
      ) %>%
        formatRound(columns = c("mean", "median", "sd", "mad", "q5", "q95", "rhat"), digits = 2) %>%
        formatRound(columns = c("ess_bulk", "ess_tail"), digits = 0)

    })

    # estimaciones germinacion
    output$estimaciones_germinacion_plot <- renderPlot({
      x <- fit_germinacion
      plot_model(x)
    })

    # estimaciones hojas
    output$estimaciones_hojas_table <- renderDataTable({
      x <- fit_hojas$summary()
      datatable(
        x,
        rownames = FALSE
      ) %>%
        formatRound(columns = c("mean", "median", "sd", "mad", "q5", "q95", "rhat"), digits = 2) %>%
        formatRound(columns = c("ess_bulk", "ess_tail"), digits = 0)

    })

    output$estimaciones_hojas_plot <- renderPlot({
      x <- fit_hojas
      plot_model(x)
    })

    # estimaciones cosecha
    output$estimaciones_cosecha_table <- renderDataTable({
      x <- fit_cosecha$summary()
      datatable(
        x,
        rownames = FALSE
      ) %>%
        formatRound(columns = c("mean", "median", "sd", "mad", "q5", "q95", "rhat"), digits = 2) %>%
        formatRound(columns = c("ess_bulk", "ess_tail"), digits = 0)

    })

    output$estimaciones_cosecha_plot <- renderPlot({
      x <- fit_cosecha
      plot_model(x)
    })

    ## siembra ----
    observeEvent(input$database_nueva_siembra, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data_row(
        con, "plantas",
        id = input$database_siembra_id,
        especie = input$database_siembra_especie,
        variedad = input$database_siembra_variedad,
        marca = input$database_siembra_marca,
        planta_tipo = input$database_siembra_planta_tipo,
        comentarios = input$database_siembra_comentarios
      )
      add_data_row(
        con, "sembradas",
        id = input$database_siembra_id,
        fecha_siembra = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D")),
        medio_siembra = input$database_siembra_medio_siembra,
        peso = input$database_siembra_peso,
        calor = input$database_siembra_calor,
        domo = input$database_siembra_domo,
        peso_semillas = input$database_siembra_peso_semillas,
        comentarios = input$database_siembra_comentarios
      )
      add_data_row(
        con, "bandejas",
        id = input$database_siembra_id,
        balda = as.integer(strsplit(input$database_siembra_bandeja, split = "-")[[1]][1]),
        bandeja = as.integer(strsplit(input$database_siembra_bandeja, split = "-")[[1]][2]),
        fecha_bandeja = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
      )
      d <<- get_data(con)
      d_all <<- get_data_summary(con, d)
      bandejas <<- get_bandejas(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_siembra_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "sembradas", input$database_eliminar_siembra_id)
      delete_data_row(con, "plantas", input$database_eliminar_siembra_id)
      d <<- get_data(con)
      d_all <<- get_data_summary(con, d)
      bandejas <<- get_bandejas(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      session$reload()
      remove_modal_spinner()
    })

    observeEvent(input$database_siembra_nuevo_especie_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_values(con, "especie", input$database_siembra_nuevo_especie)
      remove_modal_spinner()
      values <<- get_values(con)
      session$reload()
    })

    observeEvent(input$database_siembra_nuevo_variedad_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_values(con, "variedad", input$database_siembra_nuevo_variedad)
      remove_modal_spinner()
      values <<- get_values(con)
      session$reload()
    })

    observeEvent(input$database_siembra_nuevo_marca_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_values(con, "marca", input$database_siembra_nuevo_marca)
      remove_modal_spinner()
      values <<- get_values(con)
      session$reload()
    })

    observeEvent(input$database_siembra_nuevo_medio_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_values(con, "medio_siembra", input$database_siembra_nuevo_medio)
      remove_modal_spinner()
      values <<- get_values(con)
      session$reload()
    })

    observeEvent(input$database_siembra_nuevo_tipo_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_values(con, "planta_tipo", input$database_siembra_nuevo_tipo)
      remove_modal_spinner()
      values <<- get_values(con)
      session$reload()
      remove_modal_spinner()
    })

    output$siembra_table <- DT::renderDataTable({
      x <- d_all  %>%
        drop_na(fecha_siembra) %>%
        select(id, especie, variedad, marca, luz, calor, domo, medio_siembra) %>%
        DT::datatable(
          colnames = c("Especie", "Variedad", "Marca", "Luz", "Calor", "Domo", "Medio de siembra"),
          caption = "Estadisticas de siembra.",
          options = list(autoWidth = TRUE, searching = FALSE),
          rownames = FALSE
        )
      return(x)
    })

    ## germinacion ------------
    observeEvent(input$database_nueva_germinacion, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data_row(
        con, "germinadas",
        id = input$database_germinacion_id,
        fecha_germinacion = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D")),
        comentarios = input$database_germinacion_comentarios
      )
      d <<- get_data(con)
      d_all <<- get_data_summary(con, d)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_germinacion_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "germinadas", input$database_eliminar_germinacion_id)
      d <<- get_data(con)
      d_all <<- get_data_summary(con, data)
      remove_modal_spinner()
      session$reload()
    })

    output$germinacion_table <- DT::renderDataTable({
      x <- d_all  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_germinacion = difftime(fecha_germinacion, fecha_siembra, units = "days")
        ) %>%
        drop_na(t_germinacion) %>%
        select(id, planta, medio_siembra, t_germinacion) %>%
        group_by(planta, medio_siembra) %>%
        summarise(
          n = n(),
          t_germinacion = mean(t_germinacion, na.rm = TRUE),
          sd_t_germinacion = sd(t_germinacion, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        relocate(planta, medio_siembra, n) %>%
        mutate(se_t_germinacion = sd_t_germinacion/sqrt(n)) %>%
        DT::datatable(
          colnames = c("Especie (variedad)", "Medio siembra", "N", "Media", "DE", "EE"),
          caption = "Estadisticas de germinacion (horas). DE: desviacion estandar. EE: error estandar",
          options = list(autoWidth = TRUE, searching = FALSE),
          rownames = FALSE
        )
      return(x)
    })

    ## hojas ----------
    observeEvent(input$database_nuevas_hojas, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data_row(
        con, "hojas",
        id = input$database_hojas_id,
        fecha_hojas = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D")),
        comentarios = input$database_hojas_comentarios
      )
      d <<- get_data(con)
      d_all <<- get_data_summary(con, data)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_hojas_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "hojas", input$database_eliminar_hojas_id)
      d <<- get_data(con)
      d_all <<- get_data_summary(con, d)
      remove_modal_spinner()
      session$reload()
    })

    output$hojas_table <- DT::renderDataTable({
      x <- d_all  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_hojas = difftime(fecha_hojas, fecha_siembra, units = "days")
        ) %>%
        drop_na(t_hojas) %>%
        select(id, planta, medio_siembra, t_hojas) %>%
        group_by(planta, medio_siembra) %>%
        summarise(
          n = n(),
          t_hojas = mean(t_hojas, na.rm = TRUE),
          sd_t_hojas = sd(t_hojas, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        relocate(planta, medio_siembra, n) %>%
        mutate(se_t_hojas = sd_t_hojas/sqrt(n)) %>%
        DT::datatable(
          colnames = c("Especie (variedad)", "Medio siembra", "N", "Media", "DE", "EE"),
          caption = "Estadisticas de hojas (horas). DE: desviacion estandar. EE: error estandar",
          options = list(autoWidth = TRUE, searching = FALSE),
          rownames = FALSE
        )
      return(x)
    })

    ## trasplante ----------
    observeEvent(input$database_nuevo_trasplante, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data(
        con, "trasplantadas",
        id = input$database_trasplante_id,
        fecha_trasplante = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D")),
        medio_trasplante = input$database_trasplante_medio_trasplante,
        luz = input$database_trasplante_luz,
        comentarios = input$database_trasplante_comentarios
      )
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_trasplante_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "trasplantadas", input$database_eliminar_trasplante_id)
      d <<- get_data(con)
      d_all <<- get_data_summary(con, data)
      remove_modal_spinner()
      session$reload()
    })

    output$trasplante_table <- DT::renderDataTable({
      x <- d_all  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_trasplante = difftime(fecha_trasplante, fecha_siembra, units = "days")
        ) %>%
        drop_na(t_trasplante) %>%
        select(id, planta, medio_siembra, luz, t_trasplante) %>%
        group_by(planta, medio_siembra, luz) %>%
        summarise(
          n = n(),
          t_trasplante = mean(t_trasplante, na.rm = TRUE),
          sd_t_trasplante = sd(t_trasplante, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        relocate(planta, medio_siembra, luz, n) %>%
        mutate(se_t_trasplante = sd_t_trasplante/sqrt(n)) %>%
        DT::datatable(
          colnames = c("Especie (variedad)", "Medio siembra", "Luz", "N", "Media", "DE", "EE"),
          caption = "Estadisticas de trasplante (horas). DE: desviacion estandar. EE: error estandar",
          options = list(autoWidth = TRUE, searching = FALSE),
          rownames = FALSE
        )
      return(x)
    })

    ## cosecha ---------
    observeEvent(input$database_nueva_cosecha, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data_row(
        con, "cosechadas",
        id = input$database_cosecha_id,
        fecha_cosecha = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D")),
        comentarios = input$database_cosecha_comentarios
      )
      d <<- get_data(con)
      d_all <<- get_data_summary(con, d)
      bandejas <<- get_bandejas(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      session$reload()
      remove_modal_spinner()
    })

    observeEvent(input$database_cosecha_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "cosechadas", input$database_eliminar_cosecha_id)
      d <<- get_data(con)
      d_all <<- get_data_summary(con, d)
      bandejas <<- get_bandejas(con)
      bandejas_vacias <<- get_bandejas_vacias(con, d, instalaciones, bandejas)
      remove_modal_spinner()
      session$reload()
    })

    output$cosecha_table <- DT::renderDataTable({
      x <- d_all  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_cosecha = difftime(fecha_cosecha, fecha_siembra, units = "days")
        ) %>%
        drop_na(t_cosecha) %>%
        select(id, planta, medio_siembra, t_cosecha) %>%
        group_by(planta, medio_siembra) %>%
        summarise(
          n = n(),
          t_cosecha = mean(t_cosecha, na.rm = TRUE),
          sd_t_cosecha = sd(t_cosecha, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        relocate(planta, medio_siembra, n) %>%
        mutate(se_t_cosecha = sd_t_cosecha/sqrt(n)) %>%
        DT::datatable(
          colnames = c("Especie (variedad)", "Medio siembra", "N", "Media", "DE", "EE"),
          caption = "Estadisticas de cosecha (horas). DE: desviacion estandar. EE: error estandar",
          options = list(autoWidth = TRUE, searching = FALSE),
          rownames = FALSE
        )
      return(x)
    })
  }

)
