library(shiny)
library(adminlte)

cfooter <- dashFooter(
    shiny::tags$div(
        class = "pull-right hidden-xs",
        shiny::tags$b("Version", "2.3.8")),
    shiny::tags$strong("Copyright ",
                       shiny::icon("copyright"),
                       " 2014-2016",
                       shiny::tags$a(href = "http://almsaeedstudio.com",
                                     "Almsaeed Studio"),
                       "."),"All rights reserved.")

usrPanel <- userPanel()

ui <- dashPage(
    dashHeader(),
    dashSideBar(
        usrPanel,
        menuTab("menu1",
                menuSection("MAIN NAVIGATION"),
                menuItem(text = "Dashboard", tabName = "menu1_item1", icon = icon("dashboard"),
                         menuSubItem("Dashboard v1", "menu1_item1_subitem1", icon = icon("circle-o")),
                         menuSubItem("Dashboard v2", "menu1_item1_subitem2", icon = icon("circle-o"))
                         ),
                menuItem(text = "Layout Options", tabName = "menu1_item2", icon = icon("files-o"),
                         menuSubItem("Top Navigation", "menu1_item2_subitem1", icon = icon("circle-o")),
                         menuSubItem("Boxed", "menu1_item2_subitem2", icon = icon("circle-o")),
                         menuSubItem("Fixed", "menu1_item2_subitem3", icon = icon("circle-o")),
                         menuSubItem("Collapsed Sidebar", "menu1_item2_subitem4", icon = icon("circle-o"))
                         ),
                menuItem(text = "Widgets", tabName = "menu1_item2", icon = icon("th")),
                menuItem(text = "Charts", tabName = "menu1_item3", icon = icon("pie-chart"),
                         menuSubItem("ChartJS", "menu1_item3_subitem1", icon = icon("circle-o")),
                         menuSubItem("Morris", "menu1_item3_subitem2", icon = icon("circle-o")),
                         menuSubItem("Flot", "menu1_item3_subitem3", icon = icon("circle-o")),
                         menuSubItem("Inline charts", "menu1_item3_subitem4", icon = icon("circle-o"))
                         ),
                menuItem(text = "UI Elements", tabName = "menu1_item4", icon = icon("laptop"),
                         menuSubItem("General", "menu1_item4_subitem1", icon = icon("circle-o")),
                         menuSubItem("Icons", "menu1_item4_subitem2", icon = icon("circle-o")),
                         menuSubItem("Buttons", "menu1_item4_subitem3", icon = icon("circle-o")),
                         menuSubItem("Sliders", "menu1_item4_subitem4", icon = icon("circle-o")),
                         menuSubItem("Timeline", "menu1_item4_subitem5", icon = icon("circle-o")),
                         menuSubItem("Modals", "menu1_item4_subitem6", icon = icon("circle-o"))
                         ),
                menuItem(text = "Forms", tabName = "menu1_item5", icon = icon("edit"),
                         menuSubItem("General Elements", "menu1_item5_subitem1", icon = icon("circle-o")),
                         menuSubItem("Advanced Elements", "menu1_item5_subitem2", icon = icon("circle-o")),
                         menuSubItem("Editors", "menu1_item5_subitem3", icon = icon("circle-o"))
                         ),
                menuItem(text = "Tables", tabName = "menu1_item6", icon = icon("table"),
                         menuSubItem("Simple tables", "menu1_item6_subitem1", icon = icon("circle-o")),
                         menuSubItem("Data tables", "menu1_item6_subitem2", icon = icon("circle-o"))
                         ),
                menuItem(text = "Calendar", tabName = "menu1_item7", icon = icon("calendar")),
                menuItem(text = "Mailbox", tabName = "menu1_item8", icon = icon("envelope"),
                         menuSubItem("Inbox", "menu1_item8_subitem1"),
                         menuSubItem("Compose", "menu1_item8_subitem2"),
                         menuSubItem("Read", "menu1_item8_subitem3")
                         ),
                menuItem(text = "Examples", tabName = "menu1_item8", icon = icon("folder")),
                menuItem(text = "Multilevel", tabName = "menu1_item9", icon = icon("share")),
                menuItem(text = "Documentation", tabName = "menu1_item10", icon = icon("book"))
            )
    ),
    dashBody(),
    cfooter
)

srv <- function(input, output) { }

shinyApp(ui, srv)