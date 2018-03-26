# Shiny layout elements
titleBox <- function(title) {
  fluidRow(
    box(width = 12, background = "black",
        span(strong(title),
             style = "font-size:18px")
    )
  )
}

optionsBox <- function(...) {
  box(..., background = "navy")
}

plotBox <- function(...) {
  box(..., status = "warning")
}

tableBox <- function(...) {
  box(..., status = "warning")
}

textBox <- function(...) {
  box(..., status = "success")
}

messageBox <- function(...) {
  box(..., status = "danger", background = "green")
}

# common plot theme
theme_1012 <- theme(
  axis.text = element_text(face = "bold", size = 10, color = "black"),
  axis.title = element_text(face = "bold", size = 12, color = "black"),
  panel.border = element_rect(colour = "black", size = 1),
  strip.text = element_text(face = "bold", size = 10, color = "black"),
  strip.background = element_rect(colour = "black", size = 1),
  title = element_text(face = "bold", size = 14, color = "black"),
  legend.text = element_text(face = "bold", size = 8, color = "black")
)