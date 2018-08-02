# Load libraries ----------------------------------------------------------
library(shiny)
library(shinyjs)
library(ggplot2)
library(googlesheets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)

# Access Data from Googlesheets-------------------------------------------
# Create list of majors
gs_maj <- gs_key("1LqaHYZixDr4aMYq64wojMF5AYDVOsD83IWlX-ayDc0c")
maj_list <- gs_read_csv(gs_maj, col_names = TRUE)
maj_choice <- as.list(maj_list$Abbreviations)
names(maj_choice) <- maj_list$Majors

# Create list of activities
gs_prog <- gs_key("1HkqN1ISgHevSjYQw5XJlkAMQao61GMktFl9CO4PqMJY")
prog_list <- gs_read_csv(gs_prog, col_names = TRUE)
prog_choice <- as.list(prog_list$Code)
names(prog_choice) <- prog_list$CoCurriculars

# Create a list of all tags
gs_all_tags <- gs_url("https://docs.google.com/spreadsheets/d/1ilW6nwum8gORGyIXC9drVg43TA6UHOw9XfRxQnLu5vc/edit#gid=0")
tag_list <- gs_read_csv(gs_all_tags, col_names = TRUE)
tag_choice <- as.list(tag_list$Code)
names(tag_choice) <- tag_list$Tags

# Pop-up Messages
js_thanks <-
  'Shiny.addCustomMessageHandler("thanks", function(message) {alert(message);});'
js_check <-
  'Shiny.addCustomMessageHandler("check", function(message) {alert(message);});'
js_exists <-
  'Shiny.addCustomMessageHandler("exists", function(message) {alert(message);});'
js_record <-
  'Shiny.addCustomMessageHandler("noRecord", function(message) {alert(message);});'

#rating = rating

# UI-Header---------------------------------------------------

header <-
  dashboardHeader(title = "Duke Co-Curricular E-Advisor",
                  titleWidth = 300)
# UI-Sidebar -------------------------------------------------
sidebar <-
  dashboardSidebar(
    width = 300,
    sidebarMenuOutput("menu"),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      #menuItem("Favorites", tabName = "favorites", icon= icon("star")),
      menuItem(
        "Co-Curricular Recommender",
        tabName = "hybrid",
        icon = icon("list"),
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "Find Similar Co-Curriculars",
        tabName = "jaccard",
        icon = icon("th"),
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "New Students",
        tabName = "newStudents",
        icon = icon("graduation-cap"),
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "Most Popular",
        tabName = "pop",
        icon = icon("fire"),
        badgeLabel = "hot",
        badgeColor = "red"
      ),
      menuItem(
        "Statistics",
        tabName = "stats",
        icon = icon("table"),
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "Pathways",
        tabName = "pathways",
        icon = icon("location-arrow"),
        badgeLabel = "coming soon",
        badgeColor = "yellow"
      ),
      menuItem("Feedback", tabName = "feedback", icon = icon("comment")),
      menuItem(
        "About Us",
        tabName = "about",
        icon = icon("address-card")
      )
    )
  )
# UI-Body------------------------------------------------------
body <-
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        fluidPage(
          h2("Home"),
          theme = shinytheme("cerulean"),
          h3("Welcome!"),
          p(
            "As we all know, Duke University can be a difficult environment to navigate, especially
            with its countless opportunities and resources. Therefore, we have compiled a list of over
            150 different co-curricular programs, although we hope to eventually include all Duke activities.
            With the information we have gathered regarding these organizations, we have deveoloped
            various algorithms that will recommend certain co-curricular programs based on a student's
            interests and previous participation. Feel free to explore our website and test the tools
            shown in the menu on the left"
          ),
          p(
            "If you would like to help us improve our system, please fill out the User Profile
            below. Additionally, if you would like to add your organization to our database or provide
            leave us a comment, please refer to our \"Feedback\" section. If you would like to learn more
            about our project, please visit the \"About Us\" section."
          ),
          tags$head(tags$script(HTML(js_thanks))),
          tags$head(tags$script(HTML(js_check))),
          tags$head(tags$script(HTML(js_exists))),
          div(
            box(
              title = div("Create User Profile", style = "color:white"),
              status = "primary",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              textOutput("check"),
              column(
                width = 8,
                helpText(
                  HTML(
                    "Please select the programs that you have participated in during each year
                    you have been at Duke.
                    <ul>
                    <li>Summer programs are counted under the academic year
                    you finished directly prior to the program.</li>
                    <li>If you have not completed a specific
                    year yet, please select \"NA\".</li></ul>"
                  )
                ),
                selectInput(
                  "yr1prog",
                  label = h4("*Programs - Year 1"),
                  choices = prog_choice,
                  multiple = TRUE
                ),
                selectInput(
                  "yr2prog",
                  label = h4("*Programs - Year 2"),
                  choices = prog_choice,
                  multiple = TRUE
                ),
                selectInput(
                  "yr3prog",
                  label = h4("*Programs - Year 3"),
                  choices = prog_choice,
                  multiple = TRUE
                ),
                selectInput(
                  "yr4prog",
                  label = h4("*Programs - Year 4+"),
                  choices = prog_choice,
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                helpText(
                  "Please select up to 3 majors. If you are unsure, simply select \"Undeclared\"."
                ),
                selectInput(
                  "major",
                  label = h4("*Major(s)"),
                  choices = maj_choice,
                  multiple = TRUE
                ),
                hr(),
                helpText("Please type the year in which you will graduate from Duke."),
                selectInput(
                  "year",
                  label = h4("*Graduation Year"),
                  choices = c("2019", "2020", "2021")
                ),              br(),br(),br(),
                fluidRow(
                  column(
                  4,
                  align = "center",
                  offset = 4, actionButton(
                    "submit",
                    label = "Submit",
                    style = 'color: #2874A6;
                    position:relative;
                    right:20px;
                    font-size: 1.2em;
                    bottom: 18px;
                    display:block;
                    height: 100px;
                    width: 100px;
                    border-radius: 50%;
                    border: 1px solid #2874A6;'
                  )
                  )
                  )
              )
            )
          )
          )
      ),
      tabItem(
        tabName = "hybrid",
        fluidPage(
          h2("Co-Curricular Recommender"),
          theme = shinytheme("cerulean"),
          ## Hybrid Recommender
          tags$head(tags$script(HTML(js_record))),
          useShinyjs(),
          p("If you have already completed your user profile, you are able to see
             your recommendations below by simply clicking on the button."),
          p(
            "If you have participated in only one or two co-curricular activities
            here at Duke, we would recommend that you initially try the
            \"Find Similar Co-Curriculars\" tab."
          ),
          fluidRow(column(
            4,
            align = "center",
            offset = 4, actionButton("recGo", "Recommend !"))),
          br(),
          div(
            box(
              status = "primary",
              solidHeader = FALSE,
              width = 12,
              collapsible = FALSE,
              DT::dataTableOutput("table")
              )
          )
      )
      ),
      tabItem(tabName = "jaccard",
              fluidPage(
                h2("Find Similar Co-Curriculars"),
                theme = shinytheme("cerulean"),
                p(
                  "With this tool, you can discover Duke co-curricular activities that
                  are similar to each other! Just select the activity that you are interested
                  in from the drop down menu and press \"Recommend!\"."
                ),
                p(
                  "If you have already completed your user profile, you are also able to use
                  our Co-Curricular Recommender."
                ),
                
                ## Jaccard Similarity Recommender
                box(
                  #title = div("Find Similar Co-Curriculars", style="color:white"),
                  status = "primary",
                  solidHeader = FALSE,
                  width = 12,
                  collapsible = FALSE,
                  #column(width = 4,
                  selectInput(
                    "recProg",
                    label = h4(
                      "Enter the co-curricular program for which you would like to
                      see similar activities"
                    ),
                    choices = prog_choice[-1]
                    ),
                  div(actionButton("recGo2", "Recommend!"), style =
                        "padding:10px 18px 12px; float:right"),
                  #),
                  #column(width = 8,
                  DT::dataTableOutput("table2"),
                  #),
                  collapsed = FALSE
                )
                )),
      tabItem(tabName = "newStudents",
              h2("New Students"),
              p("If you haven't participated in any co-curricular from our list, you can fill out your 
                general interests below to receive recommendations."),
              useShinyjs(),
              
              # Interest Based Recommender Widget
              box(title = div("Recommendations for New Students", style="color:white"), status = "primary",
                  solidHeader = TRUE, width = 12, collapsible = TRUE,
                  column(width = 4,
                         helpText("Check the boxes that match your interests. Then, click the recommend button
                                  to see co-curricular activities that match these interests."),
                         selectInput(
                           "checkGroup",
                           label = h4("Interests"),
                           choices = tag_choice,
                           multiple = TRUE
                         ),
                         # checkboxGroupInput("checkGroup", label = h3("Interests"), choices = tag_choice),
                         hr(),
                         actionButton("recGo3", "Recommend!")
                         ),
                  DT::dataTableOutput("table3")
              ),
              collapsed = FALSE
      ),
      tabItem(
        tabName = "pop",
        # Use a fluid Bootstrap layout
        fluidPage(
          theme = shinytheme("cerulean"),
          # Give the page a title
          titlePanel("Most Popular Activities by"),
          fluidRow(column(6,
                          wellPanel(
                            selectInput(
                              "classYear",
                              h4("Class"),
                              choices = c("2019", "2020", "2021", "All")
                            )
                          )),
                   column(6,
                          wellPanel(
                            selectInput("majorPop", h4("Major"), choices = maj_choice)
                          ))),
          fluidRow(
            column(6,
                   DT::dataTableOutput("actTable")),
            column(6,
                   DT::dataTableOutput("majorTable"))
          )
        )
      ),
      tabItem(
        tabName = "stats",
        # Use a fluid Bootstrap layout
        fluidPage(
          theme = shinytheme("cerulean"),
          # Give the page a title
          titlePanel("Student Participation Information from"),
          fluidRow(column(4,
                          wellPanel(
                            selectInput(
                              "class",
                              h4("The Class of"),
                              choices = c("2019" , "2020" , "2021", "All")
                            ),
                            selectInput(
                              "grade",
                              h4("During their"),
                              choices = c(
                                "Freshman Year" = "fresh",
                                "Sophomore Year" = "soph",
                                "Junior Year" = "junior",
                                "Senior Year" = "senior"
                              )
                            )
                          )),
                   mainPanel(plotOutput("gradePlot")))
        )
      ),
      tabItem(tabName = "feedback",
              h2("Feedback"),
              p("Please let us know what you thought of our website! We are currently in the testing stage 
                of this project and would appreciate any and all feedback. If you believe we are missing a 
                co-curricular program or activity, please fill out the box below. If you would like to
                help us by providing more information about an activity, please fill out the following",
                a(" survey", href = "https://goo.gl/forms/BB34EWQfGJofHkyo1"), ". Additionally, please 
                indicate whether you had an overall positive or negative experience with our website by using
                the thumbs up and down icons below."),
              p("All feedback submitted on this page is anonymous unless you provide your netID in the boxes below. 
                Thank you for helping us improve our website!"),
              
              # New Co-Curricular Box
              box(title = div("Suggest a New Co-Curricular", style="color:white"),
                  status = "primary", width = 12, solidHeader = TRUE, 
                  collapsible = TRUE, collapsed = TRUE,
                  helpText("If we are missing a co-curricular program, let us know! Please enter the information below."),
                  textInput("newCoID", label = h4("NetID"), placeholder = "Optional"),
                  textInput("newCo", label = h4("Enter the Co-Curricular Program or Activity"), placeholder = "Ex. Co-Curricular Name"),
                  textInput("newDesc", label = h4("Enter a Brief Description for this Co-Curricular"), placeholder = "Ex. This co-curricular does..." ),
                  br(),
                  div(actionButton("subNewCo", "Submit"), style="padding:10px 18px 12px; float:right")
              ),
              
              # Comment Box
              box(title = div("Leave a Comment", style="color:white"),
                  status = "primary", width = 12, solidHeader = TRUE, 
                  collapsible = TRUE, collapsed = TRUE,
                  helpText("Let us know how we're doing! Leave us a comment below."),
                  textInput("comID", label = h4("NetID"), placeholder = "Optional"),
                  textInput("comment", label = h4("Comments"), placeholder = "Ex. This website rocks!"),
                  br(),
                  div(actionButton("subCom", "Submit"), style="padding:10px 18px 12px; float:right")
              ),
              
              # Thumbs Up/Down
              fluidRow(
                actionButton("up", label = icon("thumbs-up"),
                             style = 'color: green;
                             position: relative;
                             left: 20px;
                             display:block;
                             height: 50px;
                             width: 50px;
                             border-radius: 50%;
                             border: 2px solid green;'),
                actionButton("down", label = icon("thumbs-down"),
                             style = 'color: red;
                             position: relative;
                             top: -50px;
                             left: 80px;
                             display:block;
                             height: 50px;
                             width: 50px;
                             border-radius: 50%;
                             border: 2px solid red;')
                )
                ),
      tabItem(tabName = "pathways", 
              fluidPage(
                h2("Student Pathways")
              )
             ),
      tabItem(tabName = "about",
              fluidPage(
                titlePanel("About Us"),
                p(
                  "We are a team of Duke undergraduate students currently working on a",
                  a("Data+", href = "https://bigdata.duke.edu/data"),
                  "project in collaboration
                  with Duke's ",
                  a("Office of Information Technology", href = "https://oit.duke.edu/"),
                  "(O.I.T.). Duke University is an exciting and ever-changing institution, but as
                  Duke's massive co-curricular environment grows it can become more difficult for
                  undergraduates to navigate this complicated academic and extracurricular landscape. Therefore,
                  our goal is to create an 'e-advisor' tool that will help students determine which
                  co-curricular activities are well-suited for their interests. In order to accomplish
                  this, we hope to gather data that will allow us to track a student's co-curricular
                  'pathway,' a map of the various activities the student was involved in during their
                  time at Duke. With this data, we plan to improve the current recommendation system running
                  behind this initial version of our 'e-advisor.'"
                ),
                p(
                  "Thank you for helping us with this task!
                  If you have any questions, please visit our ",
                  a("website", href = "https://bigdata.duke.edu/projects/co-curricular-technology-pathways-e-advisor"),
                  " or feel free to contact us at ",
                  a("eadvisordukeoit@gmail.com", href = "mailto:eadvisordukeoit@gmail.com"),
                  "."
                ),
                h3("Contact Information"),
                p("Data+ Director: Paul Bendich"),
                p("Project Manager: Lindsay Berry"),
                p(
                  "Project Team Members: Alec Ashforth, Brooke Keene, Vincent Liu, Dezmanique Martin"
                ),
                p("Project Clients: Michael Faber, Evan Levine")
                ))
            )
          )
ui <- dashboardPage(header, sidebar, body)
