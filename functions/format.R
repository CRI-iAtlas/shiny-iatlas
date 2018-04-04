# Shiny layout elements
titleBox <- function(title) {
  fluidRow(
    box(width = 12, background = "black",
        span(strong(title),
             style = "font-size:18px")
    )
  )
}

subTitleBox <- function(title) {
    fluidRow(
        box(width = 12,
            span(strong(title),
                 style = "font-size:16px")
        )
    )
}

sectionBox <- function(..., title) {
    fluidRow(
        box(...,
            width = 12,
            title = title,
            solidHeader = TRUE, status = "warning", collapsible = TRUE
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

imgLinkBox <- function(..., linkId, title, imgSrc, boxText, linkText) {
  box(
    ...,
    title = title,
    solidHeader = TRUE, status = "primary",
    fluidRow(
      column(
        width = 4,
        shiny::img(src = imgSrc, width = "100%")
      ),
      column(
        width = 8,
        p(boxText),
        actionButton(inputId = linkId, label = linkText)
      )
    )
  )
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

get_top_margin <- function(p) {
  t <- 0
  if ("layoutAttrs" %in% names(p$x)) {
    p_layout_data <- p$x$layoutAttrs[[length(p$x$layoutAttrs)]]
    if (!is.null(p_layout_data$title)) {
      t <- p_layout_data$font$size * 2 %>% 
        ceiling()
    }
  }
  t
}

get_margins_plotly <- function(p, font_size = 12) {
  
  xlabbuffer <- 0
  ylabbuffer <- 0
  if ("layoutAttrs" %in% names(p$x)) {
    p_layout_data <- p$x$layoutAttrs[[1]]
    if (length(p_layout_data$xaxis$title) > 0) {
      xlabbuffer <- font_size * 3  %>% 
        ceiling()
    }
    if (length(p_layout_data$yaxis$title) > 0) {
      ylabbuffer <- font_size * 3  %>% 
        ceiling()
    }
  }

  p_data <- p$x$attrs[[length(p$x$attrs)]]
  
  xlabangle <- 0
  xlabmax <- 1
  if (class(p_data$x) == "character") {
    xlabs <- p_data$x
    xlabangle <-  map(
      p$x$layoutAttrs, 
      ~ .[["xaxis"]][["tickangle"]]
    ) %>% 
      discard(is.null) %>% 
      unlist()
    xlabmax <- xlabs %>% 
      map_int(str_length) %>% 
      max(na.rm = TRUE)
  }
  xmultiplier <- abs(sin(xlabangle * pi/180))

  
  if (p_data$type %in% c("heatmap")) {
    ylabs <- p_data$y
    ylabmax <- ylabs %>% 
      map_int(str_length) %>% 
      max(na.rm = TRUE)
  } else {
    ylabmax <- 1
  }
  
  list(
    b = xlabbuffer %>% 
      magrittr::add(
        max(font_size - 1, (font_size - 1) * (xlabmax + 1 * xmultiplier) - xlabmax)
      ) %>% 
      ceiling(),
    l = ylabbuffer %>% 
      magrittr::add(
        max(font_size - 3, (font_size - 3) * ylabmax - ylabmax)
      ) %>% 
      ceiling(),
    t = get_top_margin(p)
  )
}

get_margins <- function(p, font_size = 12) {
  if (!("xaxis" %in% names(p$x$layout))) {
    return(get_margins_plotly(p, font_size))
  }
  if (str_length(p$x$layout$xaxis$title) > 0) {
    xlabbuffer <- (p$x$layout$xaxis$titlefont$size - 6) * 3  %>% 
      ceiling()
  } else {
    xlabbuffer <- 0
  }
  
  xlabs <- p$x$layout$xaxis$categoryarray
  xlabangle <- p$x$layout$xaxis$tickangle
  xlabmax <- xlabs %>% 
    map_int(str_length) %>% 
    max(na.rm = TRUE)
  xlabfontsize <- if_else(
    !is.null(font_size), 
    min(font_size, p$x$layout$xaxis$tickfont$size),
    p$x$layout$xaxis$tickfont$size
  ) %>% 
    ceiling()
  xmultiplier <- abs(sin(xlabangle * pi/180))
  
  if (str_length(p$x$layout$yaxis$title) > 0) {
    ylabbuffer <- (p$x$layout$yaxis$titlefont$size - 6) * 3  %>% 
      ceiling()
  } else {
    ylabbuffer <- 0
  }

  ylabs <- p$x$layout$yaxis$categoryarray

  ylabangle <- p$x$layout$yaxis$tickangle
  ylabmax <- ylabs %>% 
    map_int(str_length) %>% 
    max(na.rm = TRUE)
  ylabfontsize <- if_else(
    !is.null(font_size), 
    min(font_size, p$x$layout$yaxis$tickfont$size),
    p$x$layout$yaxis$tickfont$size
  ) %>% 
    ceiling()
  ymultiplier <- abs(cos(ylabangle * pi/180))
  list(
    b = xlabbuffer %>% 
      magrittr::add(
        max(xlabfontsize, (xlabfontsize - 2) * (xlabmax + 1 * xmultiplier))
      ) %>% 
      ceiling(),
    l = ylabbuffer %>% 
      magrittr::add(
        max(ylabfontsize, ylabfontsize * (ylabmax * ymultiplier))
      ) %>% 
      ceiling(),
    t = get_top_margin(p)
  )
}

format_plotly <- function(p) {
  font_size <- 13
  p %>% 
    plotly::layout(
      font = list(
        family = "Roboto, Open Sans, sans-serif",
        size = font_size),
      margin = get_margins(p, font_size)
    ) %>% 
    plotly::config(displayModeBar = F)
}