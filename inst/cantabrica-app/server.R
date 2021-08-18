shinyServer(
  function(input, output, session) {
    output$sidebar <- renderMenu({
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Calendario", tabName = "calendario", icon = icon("calendar")),
        menuItem("A\u00f1adir datos",  icon = icon("plus"),
                 menuSubItem("Siembra", icon = icon("tint"), tabName = "siembra"),
                 menuSubItem("Germinaci\u00f3n", icon = icon("seedling"), tabName = "germinacion"),
                 menuSubItem("Trasplantes", icon = icon("truck"), tabName = "trasplante"),
                 menuSubItem("Hojas", icon = icon("leaf"), tabName = "hojas"),
                 menuSubItem("Cosecha", icon = icon("tractor"), tabName = "cosecha")),
        menuItem("Base de datos", icon = icon("database"), href = "https://docs.google.com/spreadsheets/d/1asAb_M6ldG4Ozt0BtKHO4QEpnGbBo8FaQ4eEEHpe6D4/edit?ts=6065eb7c#gid=0"),
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

    active_ids <- reactive({map(d, ~filter(., !(id %in% d$cosechadas$id)) %>% pull(id))})
    inactive_ids <- reactive({map(d, ~filter(., id %in% d$cosechadas$id) %>% pull(id))})

    observe({
      updateTextInput(session, "database_siembra_id", value = max(as.numeric(active_ids()$sembradas))+1, placeholder = max(as.numeric(active_ids()$sembradas))+1)
      updateSelectInput(session, "database_germinacion_id", choices = active_ids()$sembradas)
      updateSelectInput(session, "database_hojas_id", choices = active_ids()$sembradas)
      updateSelectInput(session, "database_trasplante_id",choices = active_ids()$sembradas)
      updateSelectInput(session, "database_cosecha_id",choices = active_ids()$sembradas)
      updateSelectInput(session, "database_siembra_especie", choices = get_values(con)$especie)
      updateSelectInput(session, "database_siembra_variedad", choices = get_values(con)$variedad)
      updateSelectInput(session, "database_siembra_marca", choices = get_values(con)$marca)
      updateSelectInput(session, "database_siembra_medio_siembra", choices = get_values(con)$medio_siembra)
      updateSelectInput(session,"database_siembra_planta_tipo", choices = get_values(con)$planta_tipo)
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

    output$descargar <- downloadHandler(
      filename = function(){ paste0("cantabrica_", Sys.Date(), ".rds")},
      content = function(file){ saveRDS(d, file)}
    )

    ## dashboard ----
    output$dashboard_species_plot <- renderPlot({
      summarise_data(con) %>%
        count(especie) %>%
        mutate(prop = n/sum(.$n)) %>%
        ggplot(aes(x = 2, y = prop, fill = especie)) +
        geom_bar(stat = "identity") +
        labs(fill = "Especie") +
        coord_polar("y", start = 300) +
        scale_x_continuous(limits = c(0, 2.5)) +
        theme_void() +
        theme(
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15)
        )
    })

    output$database_table <- DT::renderDataTable({
      x <- summarise_data(con) %>%
        mutate_at(vars(starts_with("t_")), ~ifelse(is.na(.), "-", .)) %>%
        mutate_at(vars(starts_with("fecha_")), as_date) %>%
        select(
          id, especie, variedad, marca, planta_tipo,
          fecha_siembra, peso_semillas, luz, calor, peso, medio_siembra,
          t_germinacion, t_hojas, fecha_cosecha
        ) %>%
        arrange(desc(fecha_siembra), desc(id)) %>%
        DT::datatable(
          colnames = c(
            "ID", "Especie", "Variedad", "Marca", "Tipo", "Siembra",
            "Semillas (g)", "Luz/Oscuridad", "Calor", "Peso", "Medio siembra",
            "Germinaci\u00f3n", "Hojas verdaderas", "Cosecha"
          ),
          rownames= FALSE,
          filter = "top",
          options = list(
            pageLength = 50,
            autoWidth = TRUE
          )
        )
    })

    ## siembra -----------
    observeEvent(input$database_nueva_siembra, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data_row(
        con, "plantas",
        id = input$database_siembra_id,
        especie = input$database_siembra_especie,
        variedad = input$database_siembra_variedad,
        planta_tipo = input$database_siembra_planta_tipo,
        comentarios = input$database_siembra_comentarios
      )
      add_data_row(
        con, "sembradas",
        id = input$database_siembra_id,
        fecha_siembra = input$database_siembra_date,
        medio_siembra = input$database_siembra_medio_siembra,
        peso = input$database_siembra_peso,
        luz = input$database_siembra_luz,
        calor = input$database_siembra_calor,
        peso_semillas = input$database_siembra_peso_semillas,
        comentarios = input$database_siembra_comentarios
      )
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_siembra_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "siembra", input$database_eliminar_siembra_id)
      delete_data_row(con, "plantas", input$database_eliminar_siembra_id)
      d <<- get_data(con)
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
      x <- summarise_data(con)  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")")
        ) %>%
        drop_na(fecha_siembra) %>%
        select(id, planta, medio_siembra, t_germinacion) %>%
        group_by(planta, medio_siembra) %>%
        summarise(
          n = n(),
          .groups = "drop"
        ) %>%
        relocate(planta, medio_siembra, n) %>%
        DT::datatable(
          colnames = c("Especie (variedad)", "Medio de siembra", "N"),
          caption = "Estadisticas de siembra.",
          options = list(autoWidth = TRUE, searching = FALSE)
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
        fecha_germinacion = input$database_germinacion_date,
        comentarios = input$database_germinacion_comentarios
      )
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_germinacion_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "germinacion", input$database_eliminar_germinacion_id)
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    output$germinacion_table <- DT::renderDataTable({
      x <- summarise_data(con)  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_germinacion = difftime(fecha_germinacion, fecha_siembra, units = "days"),
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
          options = list(autoWidth = TRUE, searching = FALSE)
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
        fecha_hojas = input$database_hojas_date,
        comentarios = input$database_hojas_comentarios
      )
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_hojas_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "hojas", input$database_eliminar_hojas_id)
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    output$hojas_table <- DT::renderDataTable({
      x <- summarise_data(con)  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_hojas = difftime(fecha_hojas, fecha_siembra, units = "days"),
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
          options = list(autoWidth = TRUE, searching = FALSE)
        )
      return(x)
    })

    ## trasplante ----------
    observeEvent(input$database_nuevo_trasplante, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data(
        con, "trasplante",
        id = input$database_trasplante_id,
        fecha_trasplante = input$database_trasplante_date,
        medio_trasplante = input$database_trasplante_medio_trasplante,
        comentarios = input$database_trasplante_comentarios
      )
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    observeEvent(input$database_trasplante_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "trasplante", input$database_eliminar_trasplante_id)
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    output$trasplante_table <- DT::renderDataTable({
      x <- summarise_data(con)  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_trasplante = difftime(fecha_trasplante, fecha_siembra, units = "days"),
        ) %>%
        drop_na(t_trasplante) %>%
        select(id, planta, medio_siembra, t_trasplante) %>%
        group_by(planta, medio_siembra) %>%
        summarise(
          n = n(),
          t_trasplante = mean(t_trasplante, na.rm = TRUE),
          sd_t_trasplante = sd(t_trasplante, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        relocate(planta, medio_siembra, n) %>%
        mutate(se_t_trasplante = sd_t_trasplante/sqrt(n)) %>%
        DT::datatable(
          colnames = c("Especie (variedad)", "Medio siembra", "N", "Media", "DE", "EE"),
          caption = "Estadisticas de trasplante (horas). DE: desviacion estandar. EE: error estandar",
          options = list(autoWidth = TRUE, searching = FALSE)
        )
      return(x)
    })

    ## cosecha ---------
    observeEvent(input$database_nueva_cosecha, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      add_data_row(
        con, "cosecha",
        id = input$database_cosecha_id,
        fecha_cosecha = input$database_cosecha_date,
        comentarios = input$database_cosecha_comentarios
      )
      d <<- get_data(con)
      session$reload()
      remove_modal_spinner()
    })

    observeEvent(input$database_cosecha_eliminar_button, {
      Sys.sleep(1.5)
      show_modal_spinner(spin = "semipolar", color = "DeepSkyBlue", text = "Cargando")
      delete_data_row(con, "cosecha", input$database_eliminar_cosecha_id)
      d <<- get_data(con)
      remove_modal_spinner()
      session$reload()
    })

    output$cosecha_table <- DT::renderDataTable({
      x <- summarise_data(con)  %>%
        mutate(
          planta = paste0(especie, " (", variedad, ")"),
          t_cosecha = difftime(fecha_cosecha, fecha_siembra, units = "days"),
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
          options = list(autoWidth = TRUE, searching = FALSE)
        )
      return(x)
    })

    # calendario ----
    output$calendario <- renderCalendar({
      x <- summarise_data(con) %>%
        select(id, especie, variedad, fecha_siembra) %>%
        mutate(
          start = fecha_siembra,
          end = fecha_siembra + 7,
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
          useNavigation = TRUE,
          elementId = "calendario"
        ) %>%
        cal_month_options(
          startDayOfWeek = 1,
          daynames = c("L", "M", "X", "J", "V", "S", "D"),
          visibleWeeksCount = TRUE,
        ) %>%
        cal_week_options(
          startDayOfWeek = 1,
          daynames = c("L", "M", "X", "J", "V", "S", "D")
        )

      return(x)
    })
  }

)
