dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Cantábrica",
    dropdownMenuOutput("notifications")
  ),
  dashboardSidebar(
    width = 150,
    sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    tabItems(
      tab_dashboard(),
      tab_calendario(),
      tab_datos(),
      tab_estimaciones(),
      tab_siembra(),
      tab_germinacion(),
      tab_trasplante(),
      tab_hojas(),
      tab_cosecha()
    ),
    tags$footer(title = "Desarrollada por Gonzalo García-Castro (gonzaloggc95@gmail.com)", align = "right", style = "position:absolute;bottom:0;width:100%;height:10px;color: black;padding: 10px;background-color: white;z-index: 1000;"
    )
  )
)
