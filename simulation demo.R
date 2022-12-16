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
    dots.binwidth <- .012
    n.programs <- 2000
  }
  
  # Plot functions
  {
    # Scatterplot uncertainty vs. bias
      plot.uncertainty.vs.bias <- function(df,rule.selected){
        # Apply rules
        df$CE.estimated.meets.rule.1 <- factor(ifelse(df$CE.estimated>=rule.1.threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        df$CE.estimated.rule.2.CI.LB <- qnorm(input$rule2alpha,mean=df$CE.estimated,sd=df$CE.estimation.se)
        df$CE.estimated.meets.rule.2 <- factor(ifelse(df$CE.estimated.rule.2.CI.LB>=input$rule2threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        
        if (rule.selected=="Rule1"){
          threshold <- rule.1.threshold
          df$rule.for.chart <- df$CE.estimated.meets.rule.1
        } else if (rule.selected=="Rule2") {
          threshold <- input$rule2threshold
          df$rule.for.chart <- df$CE.estimated.meets.rule.2
        }
        
        ggplot(data=df,aes(x=CE.estimation.se,y=CE.estimated.bias,group=rule.for.chart))+
          theme_bw()+
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            #panel.grid = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.title = element_text(size=14),
            axis.text.y = element_text(angle = 90,hjust=0),
            legend.text = element_text(size=12),
            
            axis.ticks = element_blank()
          )+
          #geom_point(aes(color=rule.for.chart),alpha=.2,shape=16,size=threshold)+
          geom_point(aes(color=rule.for.chart),alpha=.2,shape=19,size=3.5,stroke=0)+
          xlab("Measurement and estimation uncertainty (SD)")+
          ylab("Bias amount (estimated CE - true CE)")+
          scale_fill_manual(values=c("orchid4", "orange"))+
          scale_color_manual(values=c("orchid4", "orange"))
      }
    # Distributions bias among selected and rejected
      plot.dists.bias.selected <- function(df,rule.selected){
        
        
        # Apply rules
        df$CE.estimated.meets.rule.1 <- factor(ifelse(df$CE.estimated>=rule.1.threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        df$CE.estimated.rule.2.CI.LB <- qnorm(input$rule2alpha,mean=df$CE.estimated,sd=df$CE.estimation.se)
        df$CE.estimated.meets.rule.2 <- factor(ifelse(df$CE.estimated.rule.2.CI.LB>=input$rule2threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        
        if (rule.selected=="Rule1"){
          threshold <- rule.1.threshold
          df$rule.for.chart <- df$CE.estimated.meets.rule.1
        } else if (rule.selected=="Rule2") {
          threshold <- input$rule2threshold
          df$rule.for.chart <- df$CE.estimated.meets.rule.2
        }
        ggplot(data=df,aes(x=rule.for.chart,y=CE.estimated.bias,fill=rule.for.chart))+
          theme_bw()+
          coord_flip()+
          stat_slab(aes(thickness = stat(pdf*n)), scale = .7,alpha=.7)+
          scale_y_continuous(expand=c(0,0),limits = c(-4,4))+
          stat_dotsinterval(side="bottom",binwidth=dots.binwidth,show_interval=FALSE)+
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            #panel.grid = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.title = element_text(size=14),
            axis.text.y = element_text(angle = 90,hjust=0,size=12),
            legend.text = element_text(size=12),
            axis.ticks = element_blank()
          )+
          geom_segment(yend=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                       y=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                       x=.8,xend=2.5,color="black",linetype=2)+
          geom_segment(yend=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                       y=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                       x=2,xend=2.5,color="black",linetype=2)+
          annotate(geom="label",x=0.4,y=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                   label=c("Mean bias\namong\nrejected"),fill="white",label.size = NA)+
          annotate(geom="label",x=1.7,y=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                   label=c("Mean bias\namong\nselected"),fill="white",label.size = NA)+
          geom_segment(yend=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                       y=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                       x=2.3,xend=2.3,color="black",linetype=1,arrow=arrow(ends="both"))+
          xlab("Selected for inclusion")+
          ylab("Bias relative to true CE (point estimate of CE - true CE)")+
          scale_fill_manual(values=c("orchid4", "orange"))
      }
    # Distributions of true and false rejections
      plot.dists.true.v.false.rejection <- function(df,rule.selected){
        # Apply rules
        df$CE.estimated.meets.rule.1 <- factor(ifelse(df$CE.estimated>=rule.1.threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        df$CE.estimated.rule.2.CI.LB <- qnorm(input$rule2alpha,mean=df$CE.estimated,sd=df$CE.estimation.se)
        df$CE.estimated.meets.rule.2 <- factor(ifelse(df$CE.estimated.rule.2.CI.LB>=input$rule2threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        
        if (rule.selected=="Rule1"){
          threshold <- rule.1.threshold
          df$rule.for.chart <- df$CE.estimated.meets.rule.1
        } else if (rule.selected=="Rule2") {
          threshold <- input$rule2threshold
          df$rule.for.chart <- df$CE.estimated.meets.rule.2
        }
        df.long <- gather(df, type, CE, CE.true,CE.estimated, factor_key=TRUE)
        df.long$type <- factor(df.long$type,levels=c("CE.estimated","CE.true"),labels=c("Estimated CE","True CE"))
        df.long <- rbind(df.long,df.long)
        df.long$rule.for.chart <- as.character(df.long$rule.for.chart)
        #df.long$rule.for.chart[(input$n.programs*2+1):(input$n.programs*2*2)] <- "All"
        df.long$rule.for.chart[(n.programs*2+1):(n.programs*2*2)] <- "All"
        
        true.pos <- nrow(df[df$CE.true>=1 & df$rule.for.chart=="Selected",])
        false.pos <- nrow(df[df$CE.true<1 & df$rule.for.chart=="Selected",])
        true.neg <- nrow(df[df$CE.true<1 & df$rule.for.chart=="Rejected",])
        false.neg <- nrow(df[df$CE.true>=1 & df$rule.for.chart=="Rejected",])
        
        ggplot(data=df.long,aes(x=rule.for.chart,y=CE,fill=type))+
          theme_bw()+
          coord_flip()+
          stat_dotsinterval(side="bottom",binwidth=dots.binwidth,show_interval=FALSE)+
          stat_slab(aes(thickness = stat(pdf*n)), scale = .7,alpha=.7)+
          
          scale_y_continuous(expand=c(0,0),breaks=c(-2:6),limits = c(-2,6))+
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            #panel.grid = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.title = element_text(size=14),
            axis.text.y = element_text(angle = 90,hjust=0,size=12),
            legend.text = element_text(size=12),
            axis.ticks = element_blank()
          )+
          geom_hline(yintercept=1,linetype=1)+
          geom_hline(yintercept=threshold,linetype=2,color="darkgrey")+
          annotate(geom="label",x=3.5,y=0,
                   label=c(paste0("Falsely selected, n=",
                                  format(false.pos, big.mark = ",", scientific = FALSE),
                                  "/",format(false.pos+true.pos, big.mark = ",", scientific = FALSE))),
                   fill="white",label.size = NA)+
          annotate(geom="label",x=2.5,y=3.9,
                   label=c(paste0("Falsely rejected, n=",
                                  format(false.neg, big.mark = ",", scientific = FALSE),
                                  "/",format(false.neg+true.neg, big.mark = ",", scientific = FALSE))),
                   fill="white",label.size = NA)+
          geom_segment(yend=-1.5,y=1,x=3.3,xend=3.3,linetype=1,color="grey",arrow=arrow())+
          geom_segment(y=1,yend=5.5,x=2.35,xend=2.35,linetype=1,color="grey",arrow=arrow())+
          xlab("Selected for funding")+
          ylab("Cost-effectiveness (expressed in multiples of cash transfer's cost-effectiveness)")+
          annotate(geom="label",x=0.35,y=threshold,label=c("Threshold for\nselection"),fill="white",label.size = NA,vjust=1)+
          annotate(geom="label",x=0.35,y=1,label=c("Cash transfer\nCE"),fill="white",label.size = NA,vjust=1)
      }
  }
  
  df <- reactive({
    # Simulation data generation
    {
      # Generate a set of candidate programs and their true cost-effectiveness
        # Settings
          #n.programs <- input$n.programs
          CE.programs.distribution.true.mean <- input$CE.programs.distribution.true.mean # The mean of all programs' cost-effectiveness
          CE.programs.distribution.true.sd <- input$CE.programs.distribution.true.sd # The standard deviation of programs' true cost-effectiveness
        # Generate programs
          program.names <- as.character(1:n.programs)
          df <- data.frame(program.names)
          df$CE.true <- rnorm(n.programs,mean=CE.programs.distribution.true.mean,sd=CE.programs.distribution.true.sd)
        # Apply uncertainty to the measurements
          CE.estimation.LB <- input$CE.estimation.SD[1] # Lower bound of how much uncertainty (expressed as a SD) is applied to each program
          CE.estimation.UB <- input$CE.estimation.SD[2] # Lower bound of how much uncertainty (expressed as a SD) is applied to each program
          CE.programs.certainty.reduction.factor <- input$CE.programs.certainty.reduction.factor # A factor applied such that "better" programs have less uncertainty (e.g. better/higher ranked programs tend to be tested more)
        # Add uncertainty
          df$CE.estimation.se <- runif(n.programs,min=CE.estimation.LB,max=CE.estimation.UB) # generates initial uncertainty
        # Adjust uncertainty as a function of true effect
          df$CE.rank.demeaned <- 2*((rank(df$CE.true)-mean(rank(df$CE.true)))/nrow(df))
      
          df$CE.estimation.adjustment <- -1*CE.programs.certainty.reduction.factor*df$CE.rank.demeaned*df$CE.estimation.se
          df$CE.estimation.se <- df$CE.estimation.se+df$CE.estimation.adjustment
        # Generate estimated cost-effectiveness
          df$CE.estimated <- do.call(c,lapply(1:n.programs,function(x) rnorm(1,mean=df$CE.true[x],sd=df$CE.estimation.se[x])))
        # Generate variable that shows "bias" as the difference between the true and measured CE
          df$CE.estimated.bias <- df$CE.estimated - df$CE.true
    }
    df
  })
  
  
  output$plot.uncertainty.vs.bias <- renderPlot(plot.uncertainty.vs.bias(df(),input$ruleselect))
  output$plot.dists.bias.selected <- renderPlot(plot.dists.bias.selected(df(),input$ruleselect))
  output$plot.dists.true.v.false.rejection <- renderPlot(plot.dists.true.v.false.rejection(df(),input$ruleselect))
  
  
  observeEvent(input$ruleselect, {
    if(input$ruleselect == "Rule2"){
      shinyjs::enable("rule2threshold")
      shinyjs::enable("rule2alpha")
    }else{
      shinyjs::disable("rule2threshold")
      shinyjs::disable("rule2alpha")
    }
  })
}

# UI
{
  ui <- pageWithSidebar(
    headerPanel("Conceptual demonstration of overall selection problem"),
    sidebarPanel(
      h4("Generate simulated candidate programs"),
      # sliderInput("n.programs", 
      #              "# candidate programs to randomly generate", 
      #              #value = 10000,
      #              
      #              value = 2000,
      #              min=100,max=4000,step=100),
      h4("True cost-effectiveness"),
      helpText("Distribution of programs' true cost-effectiveness."),
      sliderInput("CE.programs.distribution.true.mean", "Average of programs' true cost-effectiveness",
                  min = 0, max = 5, value = 1,step=.1),
      sliderInput("CE.programs.distribution.true.sd", "Range of programs' true cost-effectiveness (SD)",
                  min = 0, max = 5, value = 1.5,step=.1),
      h4("Measured cost-effectiveness"),
      helpText("Add random uncertainty to the measurement of the programs' cost-effectiveness."),
      sliderInput("CE.estimation.SD", "Range of programs' measurement uncertainty (SD)",
                  min = 0, max = 5, value = c(0,2),step=.1),
      helpText("Add a factor that reduces the uncertainty for more effective programs (e.g. programs that are more cost-effective are more likely to be better tested), where 0 is no adjustment."),
      sliderInput("CE.programs.certainty.reduction.factor", "Program certainty reduction factor",
                  min = 0, max = 1, value = 0,step=.1),
    ),
    
    mainPanel(
      useShinyjs(),
      h4("Selection rule settings"),
      fluidRow(
        column(6,
               radioButtons("ruleselect", "Rule:",
                           c("Default (point estimate >= 3x GiveDirectly)" = "Rule1",
                             "Uncertainty interval LB >= threshold" = "Rule2"))
               ),
        column(6,
               disabled(sliderInput("rule2threshold", "Threshold value for certainty interval LB",
                                    min = 1, max = 4, value = 2,step=.1)),
               disabled(sliderInput("rule2alpha", "Alpha level for certainty interval rule (one-tailed, i.e. alpha = 0.1 corresponds to a 90% certainty interval)",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .2,step=.01))
        ),
        
      ),
      tabsetPanel(type = "tabs",
                  tabPanel("True vs. false rejections",
                           plotOutput("plot.dists.true.v.false.rejection"),
                           helpText("Note: The distributions above are smoothed for readability, but show the distribution of the 2,000 programs generated in the settings on the left.")
                  ),
                  tabPanel("Selection Bias",
                           plotOutput("plot.dists.bias.selected"),
                           helpText("Note: The distributions above are smoothed for readability, but show the distribution of the 2,000 programs generated in the settings on the left.")
                  ),
                  tabPanel("Uncertainty vs Bias",
                           plotOutput("plot.uncertainty.vs.bias")
                  )
                  
      )
    )
  )
}


  
shinyApp(ui, server)
