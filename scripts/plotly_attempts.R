library(plotly)

# Generate some sample data
x <- seq(1, 10, by = 0.1)
y <- sin(x)

# Create the plot
p <- plot_ly(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'Sin(x)')

# Flip the axes
p <- p %>%
  transpose
# Display the plot
p



## 0-18m
fm_highlight <- pref %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
  arrange(-nr) %>% pull(highlight) #added
fm_bold <- pref %>% filter(domein == "Fijne motoriek"& sideA == 1) %>%
  arrange(-nr) %>% pull(bold)

hline <- function(y = 0, color = "blue") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}

# Filter the data
filtered_data <- pref %>% filter(domein == "Fijne motoriek" & sideA == 1)

# Create the plot
p <- plot_ly(data = filtered_data,
             x = ~reorder(labelNLn, -nr),
             y = ~A50,
             color = ~highlight,
             colors = c("black", "#e6550d", "grey60")) %>%

  # Add point markers
  add_markers() %>%

  # Add error bars
  add_trace(y = ~A90, mode = 'lines', line = list(color = 'black'), name = 'Upper Error') %>%
  add_trace(y = ~A10, mode = 'lines', line = list(color = 'black'), name = 'Lower Error') %>%


  # Add horizontal line
  # hline(y = as.numeric(agemos), color = I("blue")) %>%
  add_trace(y = ~agemos, mode = 'lines', name = 'Horizontal Line',
            line = list(color = 'blue', width = 2)) %>%

  # Customize layout
  layout(
    title = "Fijne motoriek/Adaptie/Persoonlijkheid en Sociaal gedrag",
    xaxis = list(title = ""),
    yaxis = list(
      title = "",
      range = c(0, 18),
      tickvals = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, seq(15, 51, 3)),
      ticktext = c(
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
        "15", "18", "19", "20", "22", "23", "25", "26", "28", "29", "31", "32", "34",
        "35", "37", "38", "40", "41", "43", "44", "46", "47", "49", "50"
      ),
      secondary = list(name = "Leeftijd (maanden)")
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.15
    )
  )

# Display the plot



p






data <-pref %>% filter(domein == "Fijne motoriek"& sideA == 1)

## translate this ggplot code to plotly

#input to highlight certain rows in plot
fm_highlight <- data %>%
  arrange(-nr) %>% pull(highlight) #added
fm_bold <- data %>%
  arrange(-nr) %>% pull(bold)

#plot
  ggplot(data, aes(x = reorder(labelNLn, -nr),  y= A50, group = highlight, color = highlight)) +
  geom_point()+
  geom_errorbar(aes(ymin = A10, ymax = A90))+
  scale_color_manual(values = c("black" = "black", "#e6550d" = "#e6550d", "grey60" = "grey60"))+
  geom_hline(yintercept = 6)+
  coord_flip()+ xlab("") +
  theme(legend.position = "none", axis.text.y = element_text(face = fm_bold, color = fm_highlight)) +
  scale_y_continuous(name = "", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)), minor_breaks = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50), limits= c(0,18),position = "right", sec.axis= dup_axis(name = "Leeftijd (maanden)"), expand = c(0,0.1))+
  ggtitle("Plottitle")




  # Input to highlight certain rows in plot
  fm_highlight <- data %>%
    arrange(-nr) %>% pull(highlight) # Added
  fm_bold <- data %>%
    arrange(-nr) %>% pull(bold)

  # Plot
  plot_ly(data, x = ~reorder(labelNLn, -nr), y = ~A50, type = 'scatter', mode = 'markers',
          text = ~highlight, color = ~highlight, colors = c("black", "#e6550d", "grey60")) %>%
    add_error_bars(ymin = ~A10, ymax = ~A90) %>%
    add_hline(yintercept = 6) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "",
                   tickvals = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                   ticktext = c("",1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(18,51,3)),
                   range = c(0, 18),
                   overlaying = "y",
                   side = "right"),
      yaxis2 = list(title = "Leeftijd (maanden)",
                    tickvals = seq(0,50,2),
                    ticktext = seq(0,50,2),
                    overlaying = "y",
                    side = "left"),
      legend = list(
        title = "",
        orientation = "h",
        x = 0.5, y = -0.1
      )
    ) %>%
    highlight(
      style = list(
        text = fm_highlight,
        fontweight = fm_bold,
        colors = c("black", "#e6550d", "grey60")
      )
    ) %>%
    add_annotations(
      text = "Plottitle",
      x = 0.5, y = 1.05,
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(size = 16)
    )






  library(plotly)
  library(dplyr)
  library(tidyr)

  # Assuming 'data' dataframe is already loaded
  data <-pref %>% filter(domein == "Fijne motoriek"& sideA == 1)
  # Sort data based on 'nr'
  data_sorted <- data %>%
    mutate(labelNLn = factor(labelNLn, levels = labelNLn[order(-nr)]))


  # Create plot
  fig <- plot_ly(data = data_sorted,
                 y = ~labelNLn,
                 x = ~A50,
                 color = ~highlight,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10),
                 showlegend = FALSE) %>%
    add_segments(x = ~A10,
                 xend = ~A90,
                 y = ~labelNLn,
                 yend = ~labelNLn,
                 showlegend = FALSE) %>%
    layout(
      xaxis = list(title = "",
                   tickvals = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                   ticktext = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                   range = c(0,18),
                   showgrid = TRUE,
                   zeroline = FALSE),
      shapes = list(
        list(type = "line",
             x0 = 6, x1 = 6, #age marker vline
             y0 = -Inf, y1 = Inf,
             line = list(dash = "solid")
        )
      ),
      title = "Plottitle"
    )



  # Highlight and bold certain rows (this is tricky in plotly compared to ggplot)
  # This is a workaround using a loop
  for(i in 1:nrow(data_sorted)){
    if(data_sorted$highlight[i] == "black"){
      fig$y$data[[i]]$marker$color <- "black"
    } else if(data_sorted$highlight[i] == "#e6550d"){
      fig$y$data[[i]]$marker$color <- "#e6550d"
    } else {
      fig$y$data[[i]]$marker$color <- "grey60"
    }

    if(data_sorted$bold[i] == TRUE){
      fig$y$data[[i]]$textfont$color <- "black"
    }
  }

  # Show the plot
  fig


#test code from chat:

  #install.packages(c("plotly", "dplyr"))
  library(plotly)
  library(dplyr)
  data <- data.frame(
    labelNLn = c("Label1", "Label2", "Label3"),
    nr = c(3,1,2),
    A50 = c(5,7,6),
    A10 = c(4,6,5),
    A90 = c(6,8,7),
    highlight = c("black", "#e6550d", "grey60"),
    bold = c(TRUE, FALSE, TRUE)
  )


  library(plotly)
  library(dplyr)

  # Assuming 'data' dataframe is already loaded

  # Sort data based on 'nr'
  data_sorted <- data %>%
    arrange(-nr)

  # Create plot
  fig <- plot_ly(data = data_sorted,
                 x = ~labelNLn,
                 y = ~A50,
                 color = ~highlight,
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10)) %>%
    add_trace(y = ~A10, yend = ~A90, type = 'scatter', mode = 'lines', line = list(width = 1)) %>%
    layout(
      xaxis = list(autorange = "reversed"),
      yaxis = list(title = "",
                   tickvals = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                   ticktext = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, seq(15,51,3)),
                   range = c(0,18),
                   showgrid = TRUE,
                   zeroline = FALSE),
      shapes = list(
        list(type = "line",
             y0 = 6, y1 = 6,
             x0 = -Inf, x1 = Inf,
             line = list(dash = "solid")
        )
      ),
      title = "Plottitle"
    )

  # Highlight and bold certain rows (this is tricky in plotly compared to ggplot)
  # This is a workaround using a loop
  for(i in 1:nrow(data_sorted)){
    if(data_sorted$highlight[i] == "black"){
      fig$x$data[[i]]$marker$color <- "black"
    } else if(data_sorted$highlight[i] == "#e6550d"){
      fig$x$data[[i]]$marker$color <- "#e6550d"
    } else {
      fig$x$data[[i]]$marker$color <- "grey60"
    }

    if(data_sorted$bold[i] == TRUE){
      fig$x$data[[i]]$textfont$color <- "black"
    }
  }

  # Show the plot
  fig


