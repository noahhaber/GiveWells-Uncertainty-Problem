# Initialization and package loading
{
  library(ggplot2)
  library(ggdist)
  library(tidyr)
  library(distributional)
  library(shiny)
  library(shinyjs)
}

server <- function(input, output) {
  # Universal settings
  {
    rule.1.threshold <- 3
  }
  
  # Plot functions
  {

    
    # Distributions of true and false rejections
    plot.distributions <- function(program.mean.1,program.sd.1,
                                   program.mean.2,program.sd.2){
      
      plot <- ggplot()+
        theme_bw()+
        #coord_flip()+
        
        #stat_dotsinterval(side="left")+
        scale_x_continuous(expand=c(0,0),breaks=c(-2:6),limits = c(-2,6))+
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          #panel.grid = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank()
        ) +
        stat_slab(aes(xdist=dist_normal(program.mean.1,program.sd.1)),alpha=.2,fill="blue")+
        stat_slab(aes(xdist=dist_normal(program.mean.2,program.sd.2)),alpha=.2,fill="red")+
        geom_vline(xintercept=3,linetype=2)+
        xlab("Cost effectiveness")+
        ylab("Probability density")+
        annotate(geom="label",x=-1.5,y=.9,hjust = 0,vjust=1,
                 label=c(paste0("Program 1:\nProbability > 3: ",
                                round(100*(1-pnorm(3,mean=program.mean.1,sd=program.sd.1)),0),"%")),
                 fill="white",label.size = NA)+
        geom_rect(aes(xmin = -1.8, xmax = -1.6, ymin = .8, ymax = .9),alpha=.2,fill="blue") + 
        annotate(geom="label",x=-1.5,y=.7,hjust = 0,vjust=1,
                 label=c(paste0("Program 2:\nProbability > 3: ",
                                round(100*(1-pnorm(3,mean=program.mean.2,sd=program.sd.2)),0),"%")),
                 fill="white",label.size = NA)+
        geom_rect(aes(xmin = -1.8, xmax = -1.6, ymin = .6, ymax = .7),alpha=.2,fill="red")
      
      # Return plot
        plot
    }
  }
  
  
  output$plot.distributions <- renderPlot(plot.distributions(program.mean.1=input$program.mean.1,
                                                             program.sd.1=input$program.sd.1,
                                                             program.mean.2=input$program.mean.2,
                                                             program.sd.2=input$program.sd.2))

}

# UI
{
  ui <- pageWithSidebar(
    headerPanel("Two programs with uncertainty"),
    sidebarPanel(
      h4("Program 1"),

      sliderInput("program.mean.1", "True effect",
                  min = 0, max = 5, value = 2.8,step=.1),
      sliderInput("program.sd.1", "Maeasurement uncertainty (SD)",
                  min = 0, max = 5, value = 0.5,step=.1),
      h4("Program 2"),
      sliderInput("program.mean.2", "True effect",
                  min = 0, max = 5, value = 2.5,step=.1),
      sliderInput("program.sd.2", "Maeasurement uncertainty (SD)",
                  min = 0, max = 5, value = 1.5,step=.1),

    ),
    
    mainPanel(
      useShinyjs(),
      h4("Comparison of two programs"),
      
      plotOutput("plot.distributions")
    )
  )
}

shinyApp(ui, server)
