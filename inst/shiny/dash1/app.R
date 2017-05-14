library(shiny)
library(adminlte)

tags <- shiny::tags

cfooter <- dashFooter(
    shiny::tags$div(
        class = "pull-right hidden-xs",
        shiny::tags$b("Version", "2.3.11")),
    shiny::tags$strong("Copyright ",
                       shiny::icon("copyright"),
                       " 2014-2016",
                       shiny::tags$a(href = "http://almsaeedstudio.com",
                                     "Almsaeed Studio"),
                       "."),"All rights reserved.")

usrPanel <- userPanel("Alexander Price", "user2-160x160.jpg",
                      shiny::tags$a(href = "#", shiny::icon("circle text-success"), "Online"))

#' @importFrom shiny tags icon tabPanel
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
                menuItem(text = "Layout Options", tabName = "menu1_item2", icon = icon("files-o"), status = label("4", "primary"),
                         menuSubItem("Top Navigation", "menu1_item2_subitem1", icon = icon("circle-o")),
                         menuSubItem("Boxed", "menu1_item2_subitem2", icon = icon("circle-o")),
                         menuSubItem("Fixed", "menu1_item2_subitem3", icon = icon("circle-o")),
                         menuSubItem("Collapsed Sidebar", "menu1_item2_subitem4", icon = icon("circle-o"))
                ),
                menuItem(text = "Widgets", tabName = "menu1_item3", icon = icon("th"), badges = badge("new", "green")),
                menuItem(text = "Charts", tabName = "menu1_item4", icon = icon("pie-chart"),
                         menuSubItem("ChartJS", "menu1_item4_subitem1", icon = icon("circle-o")),
                         menuSubItem("Morris", "menu1_item4_subitem2", icon = icon("circle-o")),
                         menuSubItem("Flot", "menu1_item4_subitem3", icon = icon("circle-o")),
                         menuSubItem("Inline charts", "menu1_item4_subitem4", icon = icon("circle-o"))
                ),
                menuItem(text = "UI Elements", tabName = "menu1_item5", icon = icon("laptop"),
                         menuSubItem("General", "menu1_item5_subitem1", icon = icon("circle-o")),
                         menuSubItem("Icons", "menu1_item5_subitem2", icon = icon("circle-o")),
                         menuSubItem("Buttons", "menu1_item5_subitem3", icon = icon("circle-o")),
                         menuSubItem("Sliders", "menu1_item5_subitem4", icon = icon("circle-o")),
                         menuSubItem("Timeline", "menu1_item5_subitem5", icon = icon("circle-o")),
                         menuSubItem("Modals", "menu1_item5_subitem6", icon = icon("circle-o"))
                ),
                menuItem(text = "Forms", tabName = "menu1_item6", icon = icon("edit"),
                         menuSubItem("General Elements", "menu1_item6_subitem1", icon = icon("circle-o")),
                         menuSubItem("Advanced Elements", "menu1_item6_subitem2", icon = icon("circle-o")),
                         menuSubItem("Editors", "menu1_item6_subitem3", icon = icon("circle-o"))
                ),
                menuItem(text = "Tables", tabName = "menu1_item7", icon = icon("table"),
                         menuSubItem("Simple tables", "menu1_item7_subitem1", icon = icon("circle-o")),
                         menuSubItem("Data tables", "menu1_item7_subitem2", icon = icon("circle-o"))
                ),
                menuItem(text = "Calendar", tabName = "menu1_item8", icon = icon("calendar"), badges = badge(c("3","17"), c("red", "blue"))),
                menuItem(text = "Mailbox", tabName = "menu1_item9", icon = icon("envelope"), status = label(c("12", "16", "5"), c("warning", "success", "danger")),
                         menuSubItem("Inbox", "menu1_item9_subitem1"),
                         menuSubItem("Compose", "menu1_item9_subitem2"),
                         menuSubItem("Read", "menu1_item9_subitem3")
                ),
                menuItem(text = "Examples", tabName = "menu1_item10", icon = icon("folder"),
                         menuSubItem("Invoice ", "menu1_item10_subitem1", icon = icon("circle-o")),
                         menuSubItem("Profile", "menu1_item10_subitem2", icon = icon("circle-o")),
                         menuSubItem("Login", "menu1_item10_subitem3", icon = icon("circle-o")),
                         menuSubItem("Register", "menu1_item10_subitem4", icon = icon("circle-o")),
                         menuSubItem("LockScreen", "menu1_item10_subitem5", icon = icon("circle-o")),
                         menuSubItem("404 Error", "menu1_item10_subitem6", icon = icon("circle-o")),
                         menuSubItem("500 Error", "menu1_item10_subitem7", icon = icon("circle-o")),
                         menuSubItem("Blank Page", "menu1_item10_subitem8", icon = icon("circle-o")),
                         menuSubItem("Pace Page", "menu1_item10_subitem9", icon = icon("circle-o"))
                ),
                menuItem(text = "Multilevel", tabName = "menu1_item11", icon = icon("share"),
                         menuSubItem("Level One",  tabName = "menu1_item11_subitem1", icon = icon("circle-o")),
                         menuItem("Level One", tabName = "menu1_item11_subitem2", icon = icon("circle-o"),
                                  menuSubItem("Level Two",  tabName = "menu1_item11_subitem2_subitem1", icon = icon("circle-o")),
                                  menuItem("Level Two",  tabName = "menu1_item11_subitem2_subitem2", icon = icon("circle-o"),
                                           menuSubItem("Level Three",  tabName = "menu1_item11_subitem2_subitem2_subitem1", icon = icon("circle-o")),
                                           menuSubItem("Level Three",  tabName = "menu1_item11_subitem2_subitem2_subitem2", icon = icon("circle-o"))
                                  )
                         )
                ),
                menuItem(text = "Documentation", tabName = "menu1_item12", icon = icon("book"))
        )
    ),
    dashBody(
        tabItems(
            ## MENU DASHBOARD
            tabItem(tabName = "menu1_item1_subitem1",
                    bodyContentHeader(tags$h1("Dashboard", tags$small("Control panel")),
                                      breadcrumb(c("Home", "DashBoard"))),
                    shiny::fluidRow(
                        infoBox2(150, "New Orders", shiny::icon("shopping-cart"), "aqua"),
                        infoBox2("53%", "Bounce Rate", shiny::icon("bar-chart"), "green"),
                        infoBox2(44, "User Registrations", shiny::icon("user-plus"), "yellow"),
                        infoBox2(65, "Unique visitors", shiny::icon("pie-chart"), "red")
                    ),
                    shiny::fluidRow(
                        shiny::tags$section(class = "col-lg-7 connectedSortable ui-sortable",
                                            tabBox(
                                                title = "Sales", icon = shiny::icon("inbox"),
                                                side = "right", id = "tbsales", width = 12,
                                                shiny::tabPanel(title = "Area", "Area Chart"),
                                                shiny::tabPanel(title = "Donut", "Donut Chart")

                                            ),
                                            box(title = "Header 2-L", footer = "Footer 2-L", width = 12),
                                            box(title = "Header 3-L", footer = "Footer 3-L", width = 12),
                                            box(title = "Header 4-L", footer = "Footer 4-L", width = 12)
                                        ),
                        shiny::tags$section(class = "col-lg-5 connectedSortable ui-sortable",
                                        box(title = "Header 1-R", footer = "Footer 1-R", width = 12),
                                        box(title = "Header 2-R", footer = "Footer 2-R", width = 12)
                                        )
                    )
            ),
            tabItem(tabName = "menu1_item1_subitem2", bodyContentHeader(tags$h1("Dashboard", tags$small("Version 2.0")))),
            ## MENU LAYOUT OPTIONS
            tabItem(tabName = "menu1_item2_subitem1", bodyContentHeader(tags$h1("Top Navigation", tags$small("Example 2.0")))),
            tabItem(tabName = "menu1_item2_subitem2", bodyContentHeader(tags$h1("Boxed Layout", tags$small("Blank example to the boxed layout")))),
            tabItem(tabName = "menu1_item2_subitem3", bodyContentHeader(tags$h1("Fixed Layout", tags$small("Blank example to the fixed layout")))),
            tabItem(tabName = "menu1_item2_subitem4", bodyContentHeader(tags$h1("Sidebar Collapsed", tags$small("Layout with collapsed sidebar on load")))),
            ## MENU WIDGETS
            tabItem(tabName = "menu1_item3", bodyContentHeader(tags$h1("Widgets ", tags$small("Preview page")))),
            ## MENU CHARTS
            tabItem(tabName = "menu1_item4_subitem1", bodyContentHeader(tags$h1("ChartJS", tags$small("Preview sample")))),
            tabItem(tabName = "menu1_item4_subitem2", bodyContentHeader(tags$h1("Morris Charts", tags$small("Preview page")))),
            tabItem(tabName = "menu1_item4_subitem3", bodyContentHeader(tags$h1("Flot Charts", tags$small("Preview page")))),
            tabItem(tabName = "menu1_item4_subitem4", bodyContentHeader(tags$h1("Inline Charts", tags$small("multiple types of charts")))),
            ## MENU UI ELEMENTS
            tabItem(tabName = "menu1_item5_subitem1", bodyContentHeader(tags$h1("General UI", tags$small("Preview of UI elements")))),
            tabItem(tabName = "menu1_item5_subitem2", bodyContentHeader(tags$h1("Icons", tags$small("a set of beautiful icons")))),
            tabItem(tabName = "menu1_item5_subitem3", bodyContentHeader(tags$h1("Buttons", tags$small("Control panel")))),
            tabItem(tabName = "menu1_item5_subitem4", bodyContentHeader(tags$h1("Sliders", tags$small("range sliders")))),
            tabItem(tabName = "menu1_item5_subitem5", bodyContentHeader(tags$h1("Timeline", tags$small("example")))),
            tabItem(tabName = "menu1_item5_subitem6", bodyContentHeader(tags$h1("Modals", tags$small("new")))),
            ## MENU FORMS
            tabItem(tabName = "menu1_item6_subitem1", bodyContentHeader(tags$h1("General Form Elements", tags$small("Preview")))),
            tabItem(tabName = "menu1_item6_subitem2", bodyContentHeader(tags$h1("Advanced Form Elements", tags$small("Preview")))),
            tabItem(tabName = "menu1_item6_subitem3", bodyContentHeader(tags$h1("Text Editors", tags$small("Advanced form element")))),
            ## MENU TABLES
            tabItem(tabName = "menu1_item7_subitem1", bodyContentHeader(tags$h1("Simple Tables", tags$small("Preview of simple tables")))),
            tabItem(tabName = "menu1_item7_subitem2", bodyContentHeader(tags$h1("Data Tables", tags$small("Advanced tables")))),
            ## MENU CALENDAR
            tabItem(tabName = "menu1_item8", bodyContentHeader(tags$h1("Calendar", tags$small("Control panel")))),
            ## MENU MAILBOX
            tabItem(tabName = "menu1_item9_subitem1", bodyContentHeader(tags$h1("Mailbox", tags$small("13 new messages")))),
            tabItem(tabName = "menu1_item9_subitem2", bodyContentHeader(tags$h1("Compose", tags$small("new mail")))),
            tabItem(tabName = "menu1_item9_subitem3", bodyContentHeader(tags$h1("Read", tags$small("mail")))),
            ## MENU EXAMPLES
            tabItem(tabName = "menu1_item10_subitem1", bodyContentHeader(tags$h1("Invoice", tags$small("#007612")))),
            tabItem(tabName = "menu1_item10_subitem2", bodyContentHeader(tags$h1("User Profile", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem3", bodyContentHeader(tags$h1("Login", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem4", bodyContentHeader(tags$h1("Register", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem5", bodyContentHeader(tags$h1("Lockscreen", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem6", bodyContentHeader(tags$h1("404 Error", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem7", bodyContentHeader(tags$h1("500 Error", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem8", bodyContentHeader(tags$h1("Blank page", tags$small("example")))),
            tabItem(tabName = "menu1_item10_subitem9", bodyContentHeader(tags$h1("Pace page", tags$small("example"))))

        )
    ),
    cfooter
)

srv <- function(input, output) { }

shinyApp(ui, srv)