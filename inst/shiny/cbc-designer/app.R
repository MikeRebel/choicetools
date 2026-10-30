# CBC design studio: design choice-based conjoint exercises in the browser.
# Run from R:   shiny::runApp("inst/shiny/cbc-designer")
# Needs only the shiny package. Code lives in R/ (loaded automatically), assets in www/.
library(shiny)
shinyApp(ui = cbc_ui(), server = cbc_server)
