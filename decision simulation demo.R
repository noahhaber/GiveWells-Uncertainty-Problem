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
    plot.distributions <- function(df,rule.selected,
                                   rule2.threshold,rule2.alpha,
                                   rule3.p.threshold,
                                   rule4.p.threshold){
      
      plot <- ggplot(data=df,aes(x=CE,fill=program))+
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
        )
      
      if (rule.selected == "Rule1"){
        pt.estimate <- mean(df[df$program=="Candidate",]$CE)
        accept.reject <- ifelse(pt.estimate>=3,"Accept","Reject")
        plot <- plot +
          stat_slab(data=df[df$program=="Candidate",],aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.2)+
          geom_vline(xintercept=3,linetype=2)+
          geom_vline(xintercept=pt.estimate)+
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0("Point estimate: ",round(pt.estimate,1),"\nThreshold: 3\nResult: ",accept.reject)),fill="white",label.size = NA)
      } else if (rule.selected == "Rule2"){
        rule.2.lb <- quantile(df[df$program=="Candidate",]$CE, rule2.alpha/2)
        accept.reject <- ifelse(rule.2.lb>=rule2.threshold,"Accept","Reject")
        plot <- plot +
          stat_slab(data=df[df$program=="Candidate",],aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.2)+
          geom_vline(xintercept=rule2.threshold,linetype=2)+
          geom_vline(xintercept=rule.2.lb)+
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0(rule2.alpha*100,"% LB: ",round(rule.2.lb,1),"\nThreshold: ",rule2.threshold,"\nResult: ",accept.reject)),fill="white",label.size = NA)
      } else if (rule.selected == "Rule3"){
        iterations <- nrow(df[df$program=="Candidate",])
        p.greater.than.1 <- sum(df[df$program=="Candidate",]$CE >= 1)/(iterations)
        p.greater.than.1.label <- paste0(round(100*p.greater.than.1,1),"%")
        accept.reject <- ifelse(p.greater.than.1>=rule3.p.threshold,"Accept","Reject")
        plot <- plot +
          stat_slab(data=df[df$program=="Candidate",],aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.7)+
          geom_vline(xintercept=1,linetype=2)+
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0("Probability that candidate program is\nmore cost-effective than GiveDirectly\npoint estimate (i.e. 1): ",p.greater.than.1.label,"\nThreshold: ",rule3.p.threshold*100,"%\nResult: ",accept.reject)),fill="white",label.size = NA)
      } else if (rule.selected == "Rule4"){
        iterations <- nrow(df[df$program=="Candidate",])
        p.greater.than.comparator <- do.call(sum,lapply(1:(nrow(df)/2),
                                                        function(x) sum(df[df$program=="Candidate",]$CE[x] >= df[df$program=="GiveDirectly",]$CE)/(iterations^2)))
        p.greater.than.comparator.label <- paste0(round(100*p.greater.than.comparator,1),"%")
        accept.reject <- ifelse(p.greater.than.comparator>=rule4.p.threshold,"Accept","Reject")
        plot <- plot +
          stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.7)+
          
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0("Probability that candidate program is\nmore cost-effective than GiveDirectly: ",p.greater.than.comparator.label,"\nThreshold: ",rule3.p.threshold*100,"\nResult: ",accept.reject)),fill="white",label.size = NA)
      }

      # Return plot
        plot
    }
  }
  
  df <- reactive({
    # Simulation data generation
    {
      # Generate candidate program cost-effectiveness
        # Settings
          iterations <- input$iterations
          candidate.program.mean <- input$candidate.program.mean
          candidate.program.sd <- input$candidate.program.sd
          comparator.program.sd <- input$comparator.program.sd
          rule2threshold <- input$rule2threshold
          
      # # TEMP
          # iterations <- 8000
          # candidate.program.mean <- 2
          # candidate.program.sd <- 1.5
          # comparator.program.sd <- .5
          # rule2threshold <- 2
          # 
      # Generate programs
        iteration.number <- as.character(1:iterations)
        df <- data.frame(iteration.number)
        df$CE <- rnorm(iterations,mean=candidate.program.mean,sd=candidate.program.sd)
        df$program <- "Candidate"
        df2 <- df
        df2$CE <- rnorm(iterations,mean=1,sd=comparator.program.sd)
        df2$program <- "GiveDirectly"
        df <- rbind(df,df2)
        df$greater.than.threshold <- as.numeric(df$CE>=rule2threshold)
    }
    df
  })
  
  
  output$plot.distributions <- renderPlot(plot.distributions(df(),
                                                             rule.selected = input$ruleselect,
                                                             rule2.threshold = input$rule2threshold,
                                                             rule2.alpha = input$rule2alpha,
                                                             rule3.p.threshold = input$rule3pthreshold,
                                                             rule4.p.threshold = input$rule4pthreshold))

  observeEvent(input$ruleselect, {
    if(input$ruleselect == "Rule2"){
      shinyjs::show("rule2threshold")
      shinyjs::show("rule2alpha")
    }else{
      shinyjs::hide("rule2threshold")
      shinyjs::hide("rule2alpha")
    }
    if(input$ruleselect == "Rule3"){
      shinyjs::show("rule3pthreshold")
    }else{
      shinyjs::hide("rule3pthreshold")
    }
    if(input$ruleselect == "Rule4"){
      shinyjs::show("rule4pthreshold")
    }else{
      shinyjs::hide("rule4pthreshold")
    }
  })
}

# UI
{
  ui <- pageWithSidebar(
    headerPanel("Selection rules for individual candidate programs"),
    sidebarPanel(
      h4("Generate simulated candidate program"),
      numericInput("iterations", 
                   "# of iterations", 
                   value = 8000,
                   min=100,max=10000),
      h4("Candidate program uncertainty distribution"),
      sliderInput("candidate.program.mean", "Uncertainty distribution Mean",
                  min = 0, max = 5, value = 2,step=.1),
      sliderInput("candidate.program.sd", "Uncertainty distribution SD",
                  min = 0, max = 5, value = 1,step=.1),
      h4("GiveDirectly program uncertainty distribution"),
      sliderInput("comparator.program.sd", "Uncertainty distribution SD",
                  min = 0, max = 5, value = 0.2,step=.1)
      # h4("Comparator threshold rule"),
      # sliderInput("comparator.program.threshold", "Threshold",
      #             min = 0, max = 5, value = 3,step=.1)
    ),
    
    mainPanel(
      useShinyjs(),
      h4("Selection rule settings"),
      fluidRow(
        column(6,
               radioButtons("ruleselect", "Rule:",
                            c("Default (point estimate >= 3x GiveDirectly)" = "Rule1",
                              "Uncertainty interval LB >= threshold" = "Rule2",
                              "Probability CE > GiveDirectly point estimate" = "Rule3",
                              "Probability CE > GiveDirectly distribution" = "Rule4"))
        ),
        column(6,
               (sliderInput("rule2threshold", "Threshold value for certainty interval LB",
                                    min = 1, max = 4, value = 2,step=.1)),
               (sliderInput("rule2alpha", "Alpha level for certainty interval rule (i.e. alpha = 0.2 corresponds to a 60% certainty interval)",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .2,step=.01)),
               (sliderInput("rule3pthreshold", "Minimum acceptable probability that candidate CE > point estimate of GiveDirectly's CE",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .8,step=.05)),
               (sliderInput("rule4pthreshold", "Minimum acceptable probability that candidate CE > GiveDirectly's CE",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .8,step=.05))
        ),
      ),
      plotOutput("plot.distributions")
    )
  )
}

shinyApp(ui, server)
