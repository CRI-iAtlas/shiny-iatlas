################################################################################	
# Define general header tag list	
# List of tags to display as a common header above all tabPanels.	
################################################################################

headerTagList <- list(	
  tags$style(type = "text/css", ".navbar .navbar-nav {float: right; font-size: 14px} .navbar .navbar-nav li a {font-size: 14px} .nav-tabs {font-size: 12px}"),
  tags$base(target = "_blank")	
)

footerTagList <- list(
  tags$footer(id = "myFooter",
    shiny::includeHTML("footer.html")
  )
)

################################################################################	
# Define the full user-interface, `ui`	
################################################################################

ui <- navbarPage(
  includeCSS("www/custom.css"),
  includeCSS("www/footer.css"),
  title = strong("CRI iAtlas Portal"), selected = "Explore",	
  tabPanel("Explore", explorepage, icon = icon("bar-chart")),	
  tabPanel("About", aboutpage, icon = icon("info-circle")),	
  tabPanel("Documentation", docspage, icon = icon("file-text")),	
  tabPanel("Resources", resourcespage, icon = icon("link")),	
  header = headerTagList,	
  footer = footerTagList,
  collapsible = TRUE,	inverse = TRUE,
  windowTitle = "CRI iAtlas Portal"	,
  tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '))
)	

shinyUI(ui)
