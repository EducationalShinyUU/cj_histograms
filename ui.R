library(shiny)
library(shinydashboard)
library(ggplot2)
library(gtable)
library(grid)
uu_color <- " #ffcd00"


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "T-test demonstration", titleWidth = 350),
  dashboardSidebar(width = 350,
                   sidebarMenu(
                               menuItem("T-test", tabName = "tab1"),
                                menuItem("Disclaimer", tabName = "Disclaimer"),
                               HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
                               img(src = 'logo.png', align = "left")

                   )
  ),

  dashboardBody(

    # CSS styles
    tags$style(HTML(paste0(".irs-bar {background:",  uu_color, "}"))),
    tags$style(HTML(paste0(".irs-bar {border-top: 1px solid black}"))),
    tags$style(HTML(paste0(".irs-bar-edge {background:",  uu_color, "}"))),
    tags$style(HTML(paste0(".irs-bar-edge {border: 1px solid black}"))),
    tags$style(HTML(paste0(".irs-single {background:",  uu_color, "}"))),
    tags$style(HTML(paste0(".selectize-input {border-color:",  uu_color, "}"))),
    tags$style(HTML(paste0(".selectize-dropdown {border-color:",  uu_color, "}"))),

    ### note that uu_color is the mustard yellow color used in the sidebar. ###
    ### If possible, you can use this color + different shades of grey (+ black & white) in your figures. ###

    tags$head(tags$style(HTML(
      paste0('.skin-black .main-header .logo {
                               background-color:',  uu_color, ';
                               }
                               .skin-black .main-header .logo:hover {
                               background-color:',  uu_color, ';
                               }

                               /* active selected tab in the sidebarmenu */
                               .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                               background-color:',  uu_color, ';
                               }

                               /* navbar (rest of the header) */
                               .skin-black .main-header .navbar {
                               background-color:',  uu_color, ';
                               }

                               /* toggle button when hovered  */
                               .skin-black .main-header .navbar .sidebar-toggle:hover{
                               background-color:',  uu_color, ';
                               }

                               /* other links in the sidebarmenu when hovered */
                               .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                               background-color:',  uu_color, ';
                               }
                               /* other links in the sidebarmenu */
                               .skin-black .main-sidebar .sidebar .sidebar-menu a{
                               background-color:',  uu_color, ';
                               color: #000000;
                               }

                               /* active selected tab in the sidebarmenu */
                               .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                               background-color: #000000;
                               color: #FFFFFF;
                               }

                               .skin-black .main-sidebar {color: #000000; background-color:',  uu_color, ';}

                               ')
    ))),
    tabItems(
      tabItem(tabName = "Disclaimer", box(width = 12,h5("Terms of Usage Utrecht Unversity Shiny Server", br(), br(), tags$ul(
        tags$li("Purpose of the service “utrecht-university.shinyapps.io” is to provide a digital place for trying out, evaluating and/or comparing methods developed by researchers of Utrecht University for the scientific community worldwide. The app and its contents may not be preserved in such a way that it can be cited or can be referenced to. "), tags$li("The web application is provided ‘as is’ and ‘as available’ and is without any warranty. Your use of this web application is solely at your own risk."), tags$li("	You must ensure that you are lawfully entitled and have full authority to upload  data in the web application. The file data must not contain any  data which can raise issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or intellectual property. You shall not upload data with any confidential or proprietary information that you desire or are required to keep secret. "),tags$li("By using this app you agree to be bound by the above terms."))))),

      tabItem(tabName = "tab1",
              
              box(width = 12, align = "left",
                  column(12, align = "left",
                         h4("Difference between two samples"),
                         h5(
                           "The histograms represent two independent samples. You can manipulate the population means, standard deviation, and sample size to examine what effect this has on the observed mean difference between the two samples."
                         ),
                         plotOutput("anova_plot", width = "600px", height = "400px"))
              ),
              box(
                checkboxInput("full_ui", "Advanced options", value = FALSE),
                fluidRow(
                  conditionalPanel(
                    condition = "input.full_ui == true",
                    
                    column(2,
                           h4("Group 1:"),
                           sliderInput("mean1", "Mean:", min = 0, max = 10, value = 5, step = .1, width = 200),
                           sliderInput("sd1", "SD:", min = 0, max = 4, value = 1, step = .1, width = 200),
                           sliderInput("N1", "N:", min = 1, max = 100, value = 10, width = 200),
                           actionButton("resample_dat", "New sample", width = 200)
                           #actionButton("scale_dat", "Update mean and SD", width = 200)
                           ),
                    column(2,
                           h4("Group 2"),
                           sliderInput("mean2", "Mean:", min = 0, max = 10, value = 5, step = .1,width = 200),
                           sliderInput("sd2", "SD:", min = 0, max = 4, value = 1, step = .1, width = 200),
                           sliderInput("N2", "N:", min = 1, max = 100, value = 10, width = 200)
                           
                           )
                    ),
                  conditionalPanel(
                    condition = "input.full_ui == false",
                    
                    column(4,
                           
                           sliderInput("diff", "Difference:", min = -10, max = 10, value = 0, step = .1, width = 400),
                           sliderInput("sd_overall", "SD:", min = 0, max = 4, value = 1, step = .1, width = 400),
                           sliderInput("N_overall", "Total N (divided equally into two groups):", min = 1, max = 100, value = 20, width = 400),
                           actionButton("resample_dat2", "New sample", width = 200)
                    )
                  )
                  ,
                  column(8,
                         h4("Descriptive statistics"),
                         uiOutput("stats", container = h4))
                ),
                width = 12, align = "left"
              )
             )#,
      # 
      # 
      # tabItem(tabName = "tab2", box(width = 12, align = "center",
      #                               h4(""),
      #                               column(12,align = "left",
      # 
      #                                      h5("Development",
      #                                         br(),
      #                                         br(),
      #                                         tags$ul(
      #                                           tags$li("Utrecht University lay-out developed by Kimberley Lek."),
      #                                           tags$li("This application is developed by Caspar van Lissa.")
      #                                         ) # end tags$ul
      #                                      ))))


    )
  )
)



