# Initialization and package loading
{
  library(openxlsx)
  library(googlesheets4)
  library(pbapply)
  library(ggplot2)
  library(ggridges)
  library(ggdist)
  library(here)
  library(tidyr)
}

# Settings and initialization
{
  workbook.address <- "1x1nj_79t0OhauyfN9HANRmgSt6wAQA__cCNOyqA1Ohw"
  iterations <- 30
  suppress.gs4.messages <- TRUE
  
  df.sens.in <- read_sheet(workbook.address,sheet="Sensitivity input")
  df.sens.in$SD <- as.numeric(df.sens.in$SD)
  df.sens.in$`Original value` <- as.numeric(df.sens.in$`Original value`)
  df.sens.in$`New value` <- as.numeric(df.sens.in$`New value`)
  df.sens.in$`CI LB` <- as.numeric(df.sens.in$`CI LB`)
  df.sens.in$`CI UB` <- as.numeric(df.sens.in$`CI UB`)
  df.sens.out <- read_sheet(workbook.address,sheet="Sensitivity output")
}

# Functions
{
  # Read model inputs
  read.inputs <- function(){
    cell.values <- range_read_cells(
      ss= workbook.address,
      sheet = "Sensitivity input",
      range = paste0("B2:B",nrow(df.sens.in)+1)
    )$cell
    value.inputs <- do.call(c,lapply(1:nrow(df.sens.in),
                                         function(x) as.numeric(cell.values[[x]]$effectiveValue)))
    names(value.inputs) <- df.sens.in$Description
    df.value.inputs <- as.data.frame(t(as.matrix(value.inputs)))
    return(df.value.inputs)
  }
  
  # Read the original model inputs
  read.inputs.original <- function(){
    cell.values <- range_read_cells(
      ss= workbook.address,
      sheet = "Sensitivity input",
      range = paste0("E2:E",nrow(df.sens.in)+1)
    )$cell
    value.inputs <- do.call(c,lapply(1:nrow(df.sens.in),
                                     function(x) as.numeric(cell.values[[x]]$effectiveValue)))
    names(value.inputs) <- df.sens.in$Description
    df.value.inputs <- as.data.frame(t(as.matrix(value.inputs)))
    return(df.value.inputs)
  }
  
  # Read the model outputs (expressed as multiples of GiveDirectly)
  read.outputs <- function(){
    # Read original result values
    cell.values <- range_read_cells(
      ss= workbook.address,
      sheet = "Sensitivity output",
      range = paste0("B2:B",nrow(df.sens.out)+1)
    )$cell
    
    value.outputs <- do.call(c,lapply(1:nrow(df.sens.out),
                                           function(x) cell.values[[x]]$effectiveValue$numberValue))
    names(value.outputs) <- df.sens.out$Description
    df.value.outputs <- as.data.frame(t(as.matrix(value.outputs)))
    return(df.value.outputs)
  }
  
  # Writes new values to the model inputs
  write.inputs <- function(new.value.inputs,orig=FALSE){
    df.new.value.inputs <- data.frame(t(as.matrix(new.value.inputs)))
    range_write(
      ss= workbook.address,
      data = df.new.value.inputs,
      sheet = "Sensitivity input",
      range = paste0("F2:F",nrow(df.sens.in)+1),
      col_names = FALSE
    )
  }
  
  # Generates a probabilisticly generated value from a given distribution
  generate.random.value <- function(type,mean,SD=NA,CI.95.LB=NA,CI.95.UB=NA,n=NA){
    if(type=="proportion"){
      rbinom(n=1,size=n, prob=mean)/n
    } else if(type=="normal"){
      if(is.na(SD)){
        SD <- (((mean-CI.95.LB) + (CI.95.UB-mean))/2)/1.96
      }
      rnorm(n=1,mean=mean,sd=SD)
    } else if(type=="RR"){
      if(is.na(SD)){
        SD <- exp((((log(mean)-log(CI.95.LB)) + (log(CI.95.UB)-log(mean)))/2)/1.96)
      }
      exp(rnorm(n=1,mean=log(mean),sd=log(SD)))
    }
    
  }
  
}

# Run PSA
{
  # Store original values
    orig.value.outputs <- read.outputs()
    orig.value.inputs <- read.inputs.original()
    
  # Function for generating a new set of probabilisticly generated model inputs
    generate.new.inputs <- function(){
      new.values <- do.call(c,lapply(1:nrow(df.sens.in),function(x){
        generate.random.value(
          type=df.sens.in$Type[x],
          mean=df.sens.in$`Original value`[x],
          SD=df.sens.in$SD[x],
          CI.95.LB=df.sens.in$`CI LB`[x],
          CI.95.UB=df.sens.in$`CI UB`[x],
          n=df.sens.in$n[x])
      }))
      output <- orig.value.inputs
      output[1,] <- new.values
      return(output)
    }

  # Function running a single iteration of the PSA
    single.iteration <- function(){
        #time.start <-Sys.time()
        new.value.inputs <- generate.new.inputs()
        write.inputs(new.value.inputs)
        new.value.outputs <- read.outputs()
        #time.elapsed <- Sys.time()-time.start
        #if(time.elapsed<1.5){Sys.sleep(1.5-time.elapsed)}
        new.value.outputs
    }
  
  # Run the PSA
    if(suppress.gs4.messages==TRUE){
      results <- do.call(rbind,pbreplicate(iterations,with_gs4_quiet(single.iteration()),simplify=FALSE))
    } else {
      results <- do.call(rbind,pbreplicate(iterations,single.iteration(),simplify=FALSE))
    }
    
  
  # Reset models back to original inputs
    write.inputs(orig.value.inputs)
}


results.long <- gather(results, Program, CE, factor_key=TRUE)

# ggplot(results.long, aes(x = CE, y = Program)) + 
#   geom_density_ridges()

ggplot(results.long, aes(x = CE, y = Program,fill=Program)) + 
  #stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7,alpha=.5)+
  #stat_slab(scale = 0.7,alpha=.5)+
  #stat_dots(side = "bottom", scale = 2, slab_size = NA)+
  stat_slab(aes(thickness = stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_size = NA) +
  #stat_gradientinterval()+
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
  scale_x_continuous(limits=c(-10,40),breaks=seq(from=-10,to=40,by=10))
  