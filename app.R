
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
probability_color_scale <- scale_color_gradientn(
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
  strip.background = element_rect(color="white", fill="white", size=0.5, linetype="solid"),
  legend.position = "bottom"
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

# player decision faceted with xR-optimal decision shaded by ev_diff
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

# player decision faceted with xR-optimal decision shaded by probsgt
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
           probability_color_scale +
           geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
           facet_grid(
             xR_opt~swing,
             labeller = labeller(xR_opt = xR_opt.labs, swing = swing.labs)
           ) +
           plot_theme +
           theme(axis.title = element_blank())
  )
}

# count facet shaded by ev_diff
count_evdiff_plot <- function(current_pitches) {
  return(
    current_pitches %>%
      mutate(
        strikes = as.factor(strikes),
        balls = as.factor(balls)
      ) %>%
      ggplot(aes(x = plate_x, y = plate_z)) +
      geom_point(aes(col = ev_diff), alpha = 0.8) +
      ev_color_scale +
      geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
      facet_grid(strikes~balls) +
      plot_theme +
      labs(
        x = "balls",
        y = "strikes"
      )
  )
}

# count facet shaded by probsgt
count_probsgt_plot <- function(current_pitches) {
  return(
    current_pitches %>%
      mutate(
        strikes = as.factor(strikes),
        balls = as.factor(balls)
      ) %>%
      ggplot(aes(x = plate_x, y = plate_z)) +
      geom_point(aes(col = probsgt), alpha = 0.8) +
      probability_color_scale +
      geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
      facet_grid(strikes~balls) +
      plot_theme +
      labs(
        x = "balls",
        y = "strikes"
      )
  )
}

# situation facet shaded by ev_diff
situation_evdiff_plot <- function(current_pitches) {
  return(
    current_pitches %>%
      mutate(
        outs = as.factor(outs),
        n_baserunners = is_on_3b + is_on_2b + is_on_1b,
        n_baserunners = as.factor(n_baserunners)
      ) %>%
      ggplot(aes(x = plate_x, y = plate_z)) +
      geom_point(aes(col = ev_diff), alpha = 0.8) +
      ev_color_scale +
      geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
      facet_grid(outs~n_baserunners) +
      plot_theme +
      labs(
        x = "Number of Baserunners",
        y = "Outs"
      )
  )
}

# situation facet shaded by ev_diff
situation_probsgt_plot <- function(current_pitches) {
  return(
    current_pitches %>%
      mutate(
        outs = as.factor(outs),
        n_baserunners = is_on_3b + is_on_2b + is_on_1b,
        n_baserunners = as.factor(n_baserunners)
      ) %>%
      ggplot(aes(x = plate_x, y = plate_z)) +
      geom_point(aes(col = probsgt), alpha = 0.8) +
      probability_color_scale +
      geom_path(data = strike_zone, aes(x, z), col = "black", lwd = 1) +
      facet_grid(outs~n_baserunners) +
      plot_theme +
      labs(
        x = "Number of Baserunners",
        y = "Outs"
      )
  )
}


# Shiny App ---------------------------------------------------------------

ui <- navbarPage("Batter Evaluation App",
                 tabPanel("Four Panel Analysis",
                          fluidPage(
                            # input batter
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  inputId = "four_panel_batter",
                                  label = "Batter",
                                  choices = batters$player_name,
                                  selected = "Trout, Mike"
                                  )
                                ),
                              # visualization
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("EV Diff", plotOutput("four_panel_ev_diff")),
                                  tabPanel("Probsgt", plotOutput("four_panel_probsgt"))
                                )
                              )
                            )
                          )
                   ),
                 tabPanel("Count Analysis",
                          fluidPage(
                            # input batter
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  inputId = "count_batter",
                                  label = "Batter",
                                  choices = batters$player_name,
                                  selected = "Trout, Mike"
                                  )
                                ),
                              # visualization
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("EV Diff", plotOutput("count_ev_diff")),
                                  tabPanel("Probsgt", plotOutput("count_probsgt"))
                                  )
                                )
                              )
                          )
                 ),
                 tabPanel("Situation Analysis",
                          fluidPage(
                            # input batter
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  inputId = "situation_batter",
                                  label = "Batter",
                                  choices = batters$player_name,
                                  selected = "Trout, Mike"
                                )
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("EV Diff", plotOutput("situation_ev_diff")),
                                  tabPanel("Probsgt", plotOutput("situation_probsgt"))
                                  )
                                )
                              )
                            )
                          )
)

server <- function(input, output) {
  # four panel
  four_panel_pitches <- reactive({get_pitches(input$four_panel_batter)})
  output$four_panel_ev_diff <- renderPlot({four_panel_evdiff_plot(four_panel_pitches())})
  output$four_panel_probsgt <- renderPlot({four_panel_probsgt_plot(four_panel_pitches())})
  
  # count
  count_pitches <- reactive({get_pitches(input$count_batter)})
  output$count_ev_diff <- renderPlot({count_evdiff_plot(count_pitches())})
  output$count_probsgt <- renderPlot({count_probsgt_plot(situation_pitches())})
  
  # situations
  situation_pitches <- reactive({get_pitches(input$situation_batter)})
  output$situation_ev_diff <- renderPlot({situation_evdiff_plot(situation_pitches())})
  output$situation_probsgt <- renderPlot({situation_probsgt_plot(situation_pitches())})
}

shinyApp(ui, server)
