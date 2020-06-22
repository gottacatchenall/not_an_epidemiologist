sidebar = sidebarPanel(
              sliderInput(inputId = "not functional yet",
                          label = "useless slider:",
                          min = 1,
                          max = 50,
                          value = 30)
              )

main_panel = mainPanel(
    plotOutput(outputId = "distPlot")
)




ui = fluidPage(
    navbarPage("not_an_epidemiologist"),
    sidebarLayout(
      sidebar,
      main_panel
    )
)
