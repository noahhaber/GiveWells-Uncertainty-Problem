# Initialization and package loading
{
  library(ggplot2)
  library(ggdist)
  library(tidyr)
  library(distributional)
  library(shiny)
  library(shinyjs)
}



# Distributions demo
if(0==1){
  # Initial settings
  candidate.1.mean <- 2.5
  candidate.1.sd <- 1
  candidate.2.mean <- 2
  candidate.2.sd <- 3
  
  # Generate data frame
  df <- data.frame(group=c("Candidate 1","Candidate 2"),
                   mean=c(candidate.1.mean,candidate.2.mean),
                   sd=c(candidate.1.sd,candidate.2.sd))
  
  ggplot(data=df,aes(xdist=dist_normal(mean,sd),fill=group))+
    stat_slab(alpha=.4)+
    theme_bw()+
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      #panel.grid = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )+
    geom_vline(xintercept=3,linetype=2)
  
  print("Current rule (point estimate >= 3x GiveDirectly)")
  print(paste0("Candidate 1 selected ",round((1-pnorm(3, mean = candidate.1.mean, sd = candidate.1.sd))*100,1),"% of the time"))
  print(paste0("Candidate 2 selected ",round((1-pnorm(3, mean = candidate.2.mean, sd = candidate.2.sd))*100,1),"% of the time"))
  
}

# Decision rule sim demo
if(0==1){
  # Initial settings
  iterations <- 10000
  CE.true.c1 <- 3.2
  CE.sd.true.c1 <- 2
  n.c1 <- 250
  CE.true.c2 <- 2.8
  CE.sd.true.c2 <- 2
  n.c2 <- 50
  
  sim <- function(mean,sd,n){
    # Simulate
      CE.measured <- rnorm(n,mean=mean,sd=sd)
      CE.measured.mean <- mean(CE.measured)
      CE.measured.sd <- sd(CE.measured)
    
    # Rule 1: Point estimate >= threshold
      rule.1 <- ifelse(CE.measured.mean>=rule.1.threshold,TRUE,FALSE)
    # Rule.2: LB CI >= threshold
      rule.2 <- ifelse(CE.measured.mean-qt(1-input$rule2alpha/2,df=n-1)*CE.measured.sd/sqrt(n) >= input$rule2threshold, TRUE,FALSE)
    # Rule.2: Construct difference in distributions
      p.distn.1.g.c.mean <- CE.measured.mean-rule.3.comparator.mean
      p.distn.1.g.c.sd <- sqrt(CE.measured.sd^2+rule.3.comparator.sd^2)
      p.1.g.c <- 1-pnorm(0, mean = p.distn.1.g.c.mean, sd = p.distn.1.g.c.sd)
      rule.3 <- ifelse(p.1.g.c>=rule.3.threshold.p,TRUE,FALSE)
    
    # Output
      df <- data.frame(rule.1,rule.2,rule.3)
      return(df)
  }
  results.c1 <- colMeans(do.call(rbind,replicate(iterations,sim(mean=CE.true.c1,sd=CE.sd.true.c1,n=n.c1),simplify = FALSE)))
  results.c2 <- colMeans(do.call(rbind,replicate(iterations,sim(mean=CE.true.c2,sd=CE.sd.true.c2,n=n.c2),simplify = FALSE)))
  results.c1
  results.c2
}


# 
# # Generate data frame
# df <- data.frame(group=c("Candidate 1","Comparator"),
#                  mean=c(CE.measured.mean,rule.3.comparator.mean),
#                  sd=c(CE.measured.sd,rule.3.comparator.sd))
# 
# ggplot(data=df,aes(xdist=dist_normal(mean,sd),fill=group))+
#   stat_slab(alpha=.4)+
#   theme_bw()+
#   theme(
#     legend.position = "bottom",
#     legend.title = element_blank(),
#     #panel.grid = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks = element_blank()
#   )+
#   geom_vline(xintercept=3,linetype=2)
# 
# p.distn.1.g.c.mean <- distn.1.mean-rule.3.comparator.mean
# p.distn.1.g.c.sd <- sqrt(distn.1.sd^2+rule.3.comparator.sd^2)
# 1-pnorm(0, mean = p.distn.1.g.2.mean, sd = p.distn.1.g.2.sd)
# 
# 
# 
# 
# 
# print("Current rule (point estimate >= 3x GiveDirectly)")
# print(paste0("Candidate 1 selected ",round((1-pnorm(3, mean = candidate.1.mean, sd = candidate.1.sd))*100,1),"% of the time"))
# print(paste0("Candidate 2 selected ",round((1-pnorm(3, mean = candidate.2.mean, sd = candidate.2.sd))*100,1),"% of the time"))
# 
# 

# UI
{
  ui <- pageWithSidebar(
    headerPanel("Selection rules"),
    sidebarPanel(
      h4("Simulated programs settings"),
      numericInput("n.programs", 
                   "N programs", 
                   value = 10000,
                   min=100,max=10000),
      helpText("Distribution of programs' true cost-effectiveness."),
      sliderInput("CE.programs.distribution.true.mean", "Mean",
                  min = 0, max = 5, value = 1,step=.1),
      sliderInput("CE.programs.distribution.true.sd", "SD",
                  min = 0, max = 5, value = 1.5,step=.1),
      helpText("Add random uncertainty to the measurement of the programs' cost-effectiveness."),
      sliderInput("CE.estimation.SD", "Measurement uncertainty bounds",
                  min = 0, max = 5, value = c(0,2),step=.1),
      helpText("Add a factor that reduces the uncertainty for more effective programs (e.g. programs that are more cost-effective are more likely to be better tested), where 1 is no adjustment."),
      sliderInput("CE.programs.certainty.reduction.factor", "Program certainty reduction factor",
                  min = 0, max = 1, value = 0,step=.05),
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
               disabled(sliderInput("rule2alpha", "Alpha level for certainty interval rule (i.e. alpha = 0.2 corresponds to a 60% certainty interval)",
                                    #min = 0, max = .5, value = .2,step=.01))
                                    min = 0, max = 1, value = .2,step=.01))
        ),
        
      ),
      tabsetPanel(type = "tabs",
                  tabPanel("True vs. false rejections",
                           plotOutput("plot.dists.true.v.false.rejection")
                  ),
                  tabPanel("Selection Bias",
                           plotOutput("plot.dists.bias.selected")
                  ),
                  tabPanel("Uncertainty vs Bias",
                           plotOutput("plot.uncertainty.vs.bias")
                  )
      )
    )
  )
}

server <- function(input, output) {
  # Universal settings
  {
    rule.1.threshold <- 3
  }
  
  # Plot functions
  {
    # Scatterplot uncertainty vs. bias
    plot.uncertainty.vs.bias <- function(df,rule.selected){
      # Apply rules
        df$CE.estimated.meets.rule.1 <- factor(ifelse(df$CE.estimated>=rule.1.threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        df$CE.estimated.rule.2.CI.LB <- qnorm(input$rule2alpha/2,mean=df$CE.estimated,sd=df$CE.estimation.se)
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
          axis.ticks = element_blank()
        )+
        geom_point(aes(color=rule.for.chart),alpha=.2,shape=16,size=threshold)+
        xlab("Measurement and estimation uncertainty (SD)")+
        ylab("Bias amount (estimated CE - true CE)")
    }
    
    # Distributions bias among selected and rejected
    plot.dists.bias.selected <- function(df,rule.selected){
      # Apply rules
        df$CE.estimated.meets.rule.1 <- factor(ifelse(df$CE.estimated>=rule.1.threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
        df$CE.estimated.rule.2.CI.LB <- qnorm(input$rule2alpha/2,mean=df$CE.estimated,sd=df$CE.estimation.se)
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
        stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.7)+
        scale_y_continuous(expand=c(0,0),limits = c(-4,4))+
        #stat_dotsinterval(side="left")+
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          #panel.grid = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank()
        )+
        geom_segment(yend=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                     y=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                     x=1,xend=2.5,color="black",linetype=2)+
        geom_segment(yend=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                     y=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                     x=2,xend=2.5,color="black",linetype=2)+
        annotate(geom="label",x=0.7,y=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                 label=c("Mean bias\namong\nrejected"),fill="white",label.size = NA)+
        annotate(geom="label",x=1.7,y=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                 label=c("Mean bias\namong\nselected"),fill="white",label.size = NA)+
        geom_segment(yend=mean(df[df$rule.for.chart=="Selected",]$CE.estimated.bias),
                     y=mean(df[df$rule.for.chart=="Rejected",]$CE.estimated.bias),
                     x=2.3,xend=2.3,color="black",linetype=1,arrow=arrow(ends="both"))+
        xlab("Selected for inclusion")+
        ylab("Bias relative to true CE (point estimate of CE - true CE)")
    }
    # Distributions of true and false rejections
    plot.dists.true.v.false.rejection <- function(df,rule.selected){
      # Apply rules
      df$CE.estimated.meets.rule.1 <- factor(ifelse(df$CE.estimated>=rule.1.threshold,1,0),levels = c(0,1),labels=c("Rejected","Selected"))
      df$CE.estimated.rule.2.CI.LB <- qnorm(input$rule2alpha/2,mean=df$CE.estimated,sd=df$CE.estimation.se)
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
      df.long$rule.for.chart[(input$n.programs*2+1):(input$n.programs*2*2)] <- "All"
      
      true.pos <- nrow(df[df$CE.true>=1 & df$rule.for.chart=="Selected",])
      false.pos <- nrow(df[df$CE.true<1 & df$rule.for.chart=="Selected",])
      true.neg <- nrow(df[df$CE.true<1 & df$rule.for.chart=="Rejected",])
      false.neg <- nrow(df[df$CE.true>=1 & df$rule.for.chart=="Rejected",])
      
      ggplot(data=df.long,aes(x=rule.for.chart,y=CE,fill=type))+
        theme_bw()+
        coord_flip()+
        stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.7)+
        #stat_dotsinterval(side="left")+
        scale_y_continuous(expand=c(0,0),breaks=c(-2:6),limits = c(-2,6))+
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          #panel.grid = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
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
        ylab("Cost-effectiveness (expressed in multiples of GiveDirectly's cost-effectiveness)")+
        annotate(geom="label",x=0.70,y=threshold,label=c("Threshold for\nselection"),fill="white",label.size = NA)+
        annotate(geom="label",x=0.70,y=1,label=c("GiveDirectly\nCE"),fill="white",label.size = NA)
    }
  }
  
  df <- reactive({
  
    # Simulation data generation
    {
      # Generate a set of candidate programs and their true cost-effectiveness
        # Settings
          n.programs <- input$n.programs
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
  
shinyApp(ui, server)
