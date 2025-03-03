# install.packages("shiny.router")

library(shiny)
library(shiny.router)

# Define UI pages using fluidPage
home_page <- fluidPage(
  titlePanel("Home"),
  sidebarLayout(
    sidebarPanel("This is the home page sidebar."),
    mainPanel(
      h2("Home Page"),
      p("Welcome to the home page of this multi-page Shiny app."),
      actionLink("go_about", "Go to About Page"),
      actionLink("go_contact", "Go to Contact Page")
    )
  )
)

about_page <- fluidPage(
  titlePanel("About"),
  sidebarLayout(
    sidebarPanel("This is the about page sidebar."),
    mainPanel(
      h2("About Page"),
      p("This page contains information about our app."),
      actionLink("go_home", "Go to Home Page"),
      actionLink("go_contact", "Go to Contact Page")
    )
  )
)

contact_page <- fluidPage(
  titlePanel("Contact"),
  sidebarLayout(
    sidebarPanel("This is the contact page sidebar."),
    mainPanel(
      h2("Contact Page"),
      p("Get in touch with us through this page."),
      actionLink("go_home", "Go to Home Page"),
      actionLink("go_about", "Go to About Page")
    )
  )
)

# Define the router
router <- router_ui(
  route("/", home_page),
  route("about", about_page),
  route("contact", contact_page)
)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny Router Example"),
  
  # Corrected navigation links with hash-based URLs
  tags$ul(
    tags$li(a(href = "#!/", "Home")),
    tags$li(a(href = "#!/about", "About")),
    tags$li(a(href = "#!/contact", "Contact"))
  ),
  
  router  # Place the router UI here
)

# Define server
server <- function(input, output, session) {
  router_server()
  
  observeEvent(input$go_about, {
    change_page("about")
  })
  
  observeEvent(input$go_contact, {
    change_page("contact")
  })
  
  observeEvent(input$go_home, {
    change_page("/")
  })
}

# Run the app
shinyApp(ui, server)
