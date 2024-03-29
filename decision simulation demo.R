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
          axis.text.y = element_blank(),
          axis.title = element_text(size=14),
          axis.title.y = element_blank(),
          legend.text = element_text(size=12)
        )+
        xlab("Cost-effectiveness (expressed in multiples of cash transfer's cost-effectiveness)")
      
      if (rule.selected == "Rule1"){
        pt.estimate <- mean(df[df$program=="Candidate",]$CE)
        accept.reject <- ifelse(pt.estimate>=3,"Accept","Reject")
        plot <- plot +
          stat_slab(data=df[df$program=="Candidate",],aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.2)+
          geom_vline(xintercept=3,linetype=2)+
          geom_vline(xintercept=pt.estimate)+
          theme(legend.position = "none")+
          annotate(geom="label",x=3,y=.90,hjust = 0.5,
                   label=c(paste0("Decision threshold")),fill="white",label.size = NA)+
          annotate(geom="label",x=pt.estimate,y=.95,hjust = 0.5,
                   label=c(paste0("Point estimate")),fill="white",label.size = NA)+
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0("Point estimate: ",round(pt.estimate,1),"\nThreshold: 3\nResult: ",accept.reject)),fill="white",label.size = NA)
      } else if (rule.selected == "Rule2"){
        rule.2.lb <- quantile(df[df$program=="Candidate",]$CE, rule2.alpha)
        accept.reject <- ifelse(rule.2.lb>=rule2.threshold,"Accept","Reject")
        plot <- plot +
          stat_slab(data=df[df$program=="Candidate",],aes(thickness = stat(pdf*n), alpha = 0.2+0.5*stat(x > rule.2.lb)),
                    scale = 0.7)+
          theme(legend.position = "none")+
          geom_vline(xintercept=rule2.threshold,linetype=2)+
          geom_vline(xintercept=rule.2.lb)+
          annotate(geom="label",x=rule2.threshold,y=.90,hjust = 0.5,
                   label=c(paste0("Decision threshold")),fill="white",label.size = NA)+
          annotate(geom="label",x=rule.2.lb,y=.95,hjust = 0.5,
                   label=c(paste0((1-rule2.alpha)*100,"% CI LB")),fill="white",label.size = NA)+
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0((1-rule2.alpha)*100,"% CI LB: ",round(rule.2.lb,1),"\nThreshold: ",rule2.threshold,"\nResult: ",accept.reject)),fill="white",label.size = NA)
      } else if (rule.selected == "Rule3"){
        iterations <- nrow(df[df$program=="Candidate",])
        p.greater.than.1 <- sum(df[df$program=="Candidate",]$CE >= 1)/(iterations)
        p.greater.than.1.label <- paste0(round(100*p.greater.than.1,1),"%")
        accept.reject <- ifelse(p.greater.than.1>=rule3.p.threshold,"Accept","Reject")
        plot <- plot +
          stat_slab(data=df[df$program=="Candidate",],aes(thickness = stat(pdf*n), alpha = 0.2+0.5*stat(x > 1)),
                    scale = 0.7)+
          theme(legend.position = "none")+
          geom_vline(xintercept=1,linetype=2)+
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0("Probability that candidate program is\nmore cost-effective than cash transfer\npoint estimate (i.e. 1): ",p.greater.than.1.label,"\nThreshold: ",rule3.p.threshold*100,"%\nResult: ",accept.reject)),fill="white",label.size = NA)
      } else if (rule.selected == "Rule4"){
        iterations <- nrow(df[df$program=="Candidate",])
        p.greater.than.comparator <- do.call(sum,lapply(1:(nrow(df)/2),
                                                        function(x) sum(df[df$program=="Candidate",]$CE[x] >= df[df$program=="Cash transfer",]$CE)/(iterations^2)))
        p.greater.than.comparator.label <- paste0(round(100*p.greater.than.comparator,1),"%")
        accept.reject <- ifelse(p.greater.than.comparator>=rule4.p.threshold,"Accept","Reject")
        plot <- plot +
          stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.7)+
          
          annotate(geom="label",x=-2,y=.9,hjust = 0,
                   label=c(paste0("Probability that candidate program is\nmore cost-effective than cash transfer: ",p.greater.than.comparator.label,"\nThreshold: ",rule4.p.threshold*100,"%\nResult: ",accept.reject)),fill="white",label.size = NA)
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
          iterations <- 5000
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
        df2$program <- "Cash transfer"
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
      shinyjs::show("comparator.program.sd")
    }else{
      shinyjs::hide("rule4pthreshold")
      shinyjs::hide("comparator.program.sd")
    }
  })
}

# UI
{
  ui <- pageWithSidebar(
    headerPanel("Selection rules for individual candidate programs"),
    sidebarPanel(
      h4("Generate simulated candidate program"),
      # numericInput("iterations", 
      #              "# of iterations", 
      #              value = 5000,
      #              min=100,max=10000),
      h4("Candidate program uncertainty distribution"),
      sliderInput("candidate.program.mean", "Uncertainty distribution Mean",
                  min = 0, max = 5, value = 2,step=.1),
      sliderInput("candidate.program.sd", "Uncertainty distribution SD",
                  min = 0, max = 5, value = 1,step=.1),
      
      helpText("Note: this simulation samples from 5,000 iterations from the uncertainty intervals. In this case, these are normal distributions for simplicity, but would be expected to be drawn non-parametrically from a probabilistic sensitivity analysis with an arbitrary distribution."),
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
                            c("Default (point estimate >= 3x cash transfer)" = "Rule1",
                              "Uncertainty interval LB >= threshold" = "Rule2",
                              "Probability CE > cash transfer point estimate" = "Rule3",
                              "Probability CE > cash transfer distribution" = "Rule4"))
        ),
        column(6,
               (sliderInput("rule2threshold", "Threshold value for certainty interval LB",
                                    min = 1, max = 4, value = 2,step=.1)),
               (sliderInput("rule2alpha", "Alpha level for certainty interval rule (one-tailed i.e. alpha = 0.2 corresponds to 80% of the probability mass being to the right of the lower bound)",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .2,step=.01)),
               (sliderInput("rule3pthreshold", "Minimum acceptable probability that candidate CE > point estimate of cash transfer's CE",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .8,step=.05)),
               (sliderInput("comparator.program.sd", "Cash transfer uncertainty distribution (SD)",
                           min = 0, max = 2, value = 0.2,step=.05)),
               (sliderInput("rule4pthreshold", "Minimum acceptable probability that candidate CE > cash transfer's CE",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .8,step=.05))
        ),
      ),
      plotOutput("plot.distributions")
    )
  )
}

shinyApp(ui, server)
