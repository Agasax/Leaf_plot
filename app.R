library(shiny)
library(tidyverse)
library(scales)
# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Leaf plot"),
  wellPanel(HTML(
    paste(
      "This leaf plot displays the relationship between pre-test and post-test probability, given selected sensitivity and specificity  ",
      "Upper curve shows post-test probabilities given positive test",
      "Lower curve shows post-test probabilities given negative test",
      "Dotted lines mark post-test probability given selected pre-test probability",
      sep = "<br/>"
    )
  )),
  # Sidebar with sliders and input boxes
  sidebarLayout(
    sidebarPanel(tabsetPanel(tabPanel(title = "Input",
    
        numericInput("sensitivity", value = 90, label = "Sensitivity (%):", min = 0, max = 100, step = .1),

        numericInput("specificity", value = 55, label = "Specificity (%):", min = 0, max = 100, step = .1),

        numericInput("preprob", value = 15, label = "Pretest probability (%):", min = 0, max = 100, step = .1)
      
    ),tabPanel(title = "Options",
               sliderInput("xlim",value = c(0,100), label = "Range of pre-test probability in %", post = "%",min = 0,max = 100,step = 1),
               sliderInput("ylim",value = c(0,100), label = "Range of post-test probability in %", post = "%",min = 0,max = 100,step = 1)
              ))),

    # Show a plot and
    mainPanel(plotOutput("leafPlot"))
  ),
  wellPanel(HTML(
    paste(
      'Based on:
Leaf plot v 0.9
Script by Fernando Zampieri
(fzampieri@hcor.com.br) from "When will less monitoring and diagnostic testing benefit the patient more?" FG Zampieri, S Einav. Intensive Care Med. 2019.',

      "Original concept by:
Coulthard MG, Coulthard T. The leaf plot: a novel way of presenting the value of tests. Br J Gen Pract. 2019 Apr;69(681):205-206.",
      'Made Shiny by Lars Mølgaard Saxhaug  <a href="https://twitter.com/load_dependent" >@load_dependent</a>',
      "Built on R 3.6.2 using tidyverse version 1.3.0 and shiny version 1.4.0.2",
      'Code available <a href="https://github.com/Agasax/Leaf_plot">here</a>, use and distribute freely!',
      sep = "<br/>"
    )
  ))
)


# Leafplotfunction
leafplot <- function(sens, spec, preprob, xlim, ylim) {
  db <- tibble(
    prob.pre = seq(0.001, 0.999, by = 0.001),
    odds.pre = prob.pre / (1 - prob.pre),
    plr = sens / (1 - spec),
    nlr = (1 - sens) / spec,
    odds.pos.p = odds.pre * plr,
    prob.pos.p = odds.pos.p / (1 + odds.pos.p),
    odds.pos.n = odds.pre * nlr,
    prob.pos.n = odds.pos.n / (1 + odds.pos.n)
  )
  # Post.prob. given selected pre.prob
  post.prob.p.print <- (preprob / (1 - preprob) * sens / (1 - spec) / (1 + preprob / (1 - preprob) * sens / (1 - spec)))
  post.prob.n.print <- (preprob / (1 - preprob) * (1 - sens) / spec / (1 + preprob / (1 - preprob) * (1 - sens) / spec))

  # Plot
  leafplot <- db %>%
    select(prob.pre, prob.pos.p, prob.pos.n) %>%
    ggplot(aes(x = prob.pre, y = prob.pos.p)) +
    geom_line() +
    geom_line(inherit.aes = FALSE, aes(x = prob.pre, y = prob.pos.n)) +
    geom_abline(
      slope = 1,
      intercept = 0,
      color = "brown",
      size = 1.2
    ) +
    geom_ribbon(
      aes(x = prob.pre, ymax = prob.pos.p, ymin = prob.pos.n),
      alpha = 0.5,
      fill = "darkgreen"
    ) +
    geom_segment(
      aes(
        x = preprob,
        y = preprob,
        xend = preprob,
        yend = post.prob.p.print
      ),
      arrow = arrow(length = unit(0.5, "cm"), ends = "last"),
      lty = "dotted"
    ) +
    geom_segment(
      aes(
        x = preprob,
        y = preprob,
        xend = preprob,
        yend = post.prob.n.print
      ),
      arrow = arrow(length = unit(0.5, "cm"), ends = "last"),
      lty = "dotted"
    ) +
    coord_cartesian(
      xlim = xlim,
      expand = TRUE,
      ylim = ylim
    ) +
    scale_y_continuous(
      labels = scales::percent,
      breaks = c(seq(from=ylim[[1]],to=ylim[[2]],length.out = 5), post.prob.p.print, post.prob.n.print)
    ) +
    scale_x_continuous( 
      labels = scales::percent,
      breaks = c(seq(from=xlim[[1]],to=xlim[[2]],length.out = 5), preprob)
    ) +
    labs(
      x = "Pre-test probability",
      y = "Post-test probability",
      title = "",
      subtitle = paste(
        "With a pre-test probability of",
        percent(preprob),
        "post-test probability is",
        percent(post.prob.p.print),
        "given positive test and",
        percent(post.prob.n.print),
        "given negative test",
        sep = " "
      )
    ) +
    annotate("text",
      x = 0.25,
      y = 0.875,
      label = "Positive Test"
    ) +
    annotate("text",
      x = 0.875,
      y = 0.25,
      label = "Negative Test"
    ) +
  theme_minimal()

  return(list(db, leafplot, post.prob.p.print, post.prob.n.print))
}

# Define server
server <- function(input, output, session) {
  # if Sensitivity

  output$leafPlot <-
    renderPlot({
      leafplot(
        input$sensitivity / 100,
        input$specificity / 100,
        input$preprob / 100,
        input$xlim / 100,
        input$ylim / 100
      )
    })
}

# Tibble



# Run the application
shinyApp(ui = ui, server = server)
