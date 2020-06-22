sidebar = sidebarPanel(
                      htmlOutput("select_state"),
                      htmlOutput("select_county")
              )

main_panel = mainPanel(
    plotOutput(outputId = "distPlot", width="100%", height="700px")
)




ui = fluidPage(
    navbarPage("not_an_epidemiologist"),
    sidebarLayout(
      sidebar,
      main_panel
    )
)
