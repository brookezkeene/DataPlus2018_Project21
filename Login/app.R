library(shiny)

ui = fluidPage(style= "background-image:url(back.jpg);",
               tags$head(tags$style(
                 HTML(
                   "
                   @import url('https://fonts.googleapis.com/css?family=Open+Sans');
                   "
                 )
                 )),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               
               fluidRow(column(
                 8,
                 align = "center",
                 offset = 2,
                 h1("Co-Curricular Technology Pathway E-advisor", style = "font-family:'Open Sans', sans-serif; font-weight: 800; line-height: 1.2; color:  white;opacity:1;"),
                 tags$style(
                   type = "text/css",
                   "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"
                 )
               )),
               br(),
               fluidRow(column(
                 4,
                 align = "center",
                 offset = 4,
                 actionButton(
                   "login",
                   "Login",
                   icon("paper-plane"),
                   onclick = "window.location.replace('https://oauth.oit.duke.edu/oauth/authorize.php?client_id=duke-cocurricular-eadvisor&client_secret=bY!NsopBG9GY$SM89P8HYaVNmtA$=wtlhP$l1KJUoyrJ*hRzke&redirect_uri=https%3A%2F%2Feadvisorduke.shinyapps.io%2Feadvisor%2F&response_type=token&state=1998&scope=basic')",
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin:auto;text-align:center;opacity:1;"
                 )
               )),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br()
                 )

server <- function(input, output) {
}

shinyApp(ui, server)
