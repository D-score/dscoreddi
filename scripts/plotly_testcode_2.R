## gm plotly

## baseplot:

#input parameters
agemos <- 36


vline <- function(x){
list(type = "line",
  y0 = 0,
  y1 = 1,
  yref = "paper",
  x0 = x,
  x1 = x,
  line = list(color = "black"))
}

##basic plot for gm:
dscoreddi::prefdat %>%
  rename(kenmerk = labelNL) %>%
  filter(sideA==1 | sideB ==1) %>%
  filter(domein == "Grove motoriek") %>%
  mutate(labelNLn = forcats::fct_reorder(labelNLn, desc(nr))) %>%
  arrange(desc(nr)) %>%
  plot_ly(x = ~A50,
          y= ~labelNLn,
          hoverinfo = "text") %>%
  add_segments(x = ~A10, y = ~labelNLn,
               xend = ~A90, yend = ~labelNLn,
               color = I("black"),
               showlegend = FALSE
  )%>%
  add_markers(xaxis = "x2", inherit = TRUE, data = NULL, color = I("transparent")) %>%
  add_markers(
    x = ~A10,
    y = ~labelNLn,
    color = I("black"),
    text = ~kenmerk,
    marker = list(symbol = "line-ns",
                  size = 3,
                  line = list(
                    color = "black",
                    width = 2
                  )
    ),
    showlegend = FALSE
  ) %>%
  add_markers(
    x = ~A50,
    y = ~labelNLn,
    color = I("black"),
    text = ~kenmerk,
    showlegend = FALSE
  ) %>%
  add_markers(
    x = ~A90,
    y = ~labelNLn,
    color = I("black"),
    text = ~kenmerk,
    marker = list(symbol = "line-ns",
                  size = 3,
                  line = list(
                    color = "black",
                    width = 2
                  )
    ),
    showlegend = FALSE
  ) %>%
  layout(
    xaxis = list(
      title = "",
      tickvals = xtick,
      zeroline = FALSE,
      showgrid = TRUE,
      range = c(0,50),
      ticktext = xtick_text),
    xaxis2 = list(
      overlaying = "x",
      tickvals = xtick,
      ticktext = xtick_text,
      range = c(0,50),
      side = "top",
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(size = 11)
    ),
    yaxis = list(
      title = ""
    ),
    shapes = list(
      vline(NULL)
        )
    )


