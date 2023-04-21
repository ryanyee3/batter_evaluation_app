
# shiny app to visualize model output from plate discipline paper

library(tidyverse)
library(shiny)

load("pitches.RData")
load("batters.RData")


# Plot Settings -----------------------------------------------------------

# define the strike zone
strike_zone <- data.frame(
  x = c(-.85, -.85, .85, .85, -.85),
  z = c(1.6, 3.5, 3.5, 1.6, 1.6)
)

# color scale for probabilities
probabiilty_color_scale <- scale_color_gradientn(
  colors = c("#364B9A", "#EAECCC", "#A50026"),
  values = scales::rescale(c(0, .5, 1)),
  limits = c(0, 1), 
  name = "P(xR_Optimal)"
)

# color scale for expected values
ev_color_scale <- scale_color_gradientn(
  colors = c("#364B9A", "#4A7BB7", "#98CAE1", "#EAECCC", "#FDB366", "#DD3D2D", "#A50026"),
  values = scales::rescale(c(-1.75, -.25, -.1, 0, .1, .25, 1.75)),
  limits = c(-1.75, 1.75), 
  name = "EV Diff"
)

# plot theme
plot_theme <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(hjust = 0.5),
  strip.background = element_rect(color="white", fill="white", size=0.5, linetype="solid")
)

# Data Functions ----------------------------------------------------------

# returns id of current_batter
get_batter_id <- function(current_batter) {
  return(batters %>% filter(player_name == current_batter) %>% pull(id))
}

# returns pitches faced by current batter
get_pitches <- function(current_batter) {
  return(pitches %>% filter(batter == get_batter_id(current_batter)))
}


# Plotting Functions ------------------------------------------------------

# plot with player decision faceted with xR-optimal decision
four_panel_evdiff_plot <- function(current_pitches) {
  xR_opt.labs <- c("xR_Optimal = Take", "xR_Optimal = Swing")
  names(xR_opt.labs) <- c("0", "1")
  swing.labs <- c("Take", "Swing")
  names(swing.labs) <- c("0", "1")
  return(
    current_pitches %>%
      mutate(
        swing = as.factor(swing),
        xR_opt = as.factor(xR_opt)
      ) %>%
      ggplot(aes(x = plate_x, y = plate_z)) +
      geom_point(aes(col = ev_diff), alpha = 0.8) +
      ev_color_scale +
      geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
      facet_grid(
        xR_opt~swing,
        labeller = labeller(xR_opt = xR_opt.labs, swing = swing.labs)
      ) +
      plot_theme +
      theme(axis.title = element_blank())
  )
}

four_panel_probsgt_plot <- function(current_pitches) {
  xR_opt.labs <- c("xR_Optimal = Take", "xR_Optimal = Swing")
  names(xR_opt.labs) <- c("0", "1")
  swing.labs <- c("Take", "Swing")
  names(swing.labs) <- c("0", "1")
  return(current_pitches %>%
           mutate(
             swing = as.factor(swing),
             xR_opt = as.factor(xR_opt)
           ) %>%
           ggplot(aes(x = plate_x, y = plate_z)) +
           geom_point(aes(col = probsgt), alpha = 0.8) +
           probabiilty_color_scale +
           geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
           facet_grid(
             xR_opt~swing,
             labeller = labeller(xR_opt = xR_opt.labs, swing = swing.labs)
           ) +
           plot_theme +
           theme(axis.title = element_blank())
  )
}

# Shiny App ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Batter Evaluation App"),
  
  # input batter
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "current_batter",
        label = "Batter",
        choices = batters$player_name,
        selected = "Trout, Mike"
      ),
      # selectInput(
      #   inputId = "color_stat",
      #   label = "Stat",
      #   choices = c("Expected Value Difference", "Probability xR-Optimal = Swing"),
      #   selected = "Expected Value Difference"
      # )
    ),
    
    # visualization
    mainPanel(
      plotOutput("ev_diff"),
      plotOutput("probsgt")
    )
  )
)

server <- function(input, output) {
  current_pitches <- reactive({get_pitches(input$current_batter)})
  output$ev_diff <- renderPlot({four_panel_evdiff_plot(current_pitches())})
  output$probsgt <- renderPlot({four_panel_probsgt_plot(current_pitches())})
}

shinyApp(ui, server)
