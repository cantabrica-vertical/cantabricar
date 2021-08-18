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
            tab_siembra(),
            tab_germinacion(),
            tab_trasplante(),
            tab_hojas(),
            tab_cosecha()
        )
    )
)
