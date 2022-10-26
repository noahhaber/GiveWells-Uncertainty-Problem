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
  library(cowplot)
}

# Settings and initialization
{
  workbook.address <- "1x1nj_79t0OhauyfN9HANRmgSt6wAQA__cCNOyqA1Ohw"
  iterations <- 1000
  batch.size <- 10
  suppress.gs4.messages <- TRUE
  filename.for.storage <- "Results data/simulation results.RData"
  run.PSA <- TRUE
  update.PSA <- FALSE
  
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
    } else if(type=="normal nonneg"){ # note: recursively repeats until non-negative value selected
      if(is.na(SD)){
        SD <- (((mean-CI.95.LB) + (CI.95.UB-mean))/2)/1.96
      }
      val <- rnorm(n=1,mean=mean,sd=SD)
      if(val>=0){
        val
      } else {
        generate.random.value(type=type,mean=mean,SD=SD,CI.95.LB=CI.95.LB,CI.95.UB=CI.95.UB,n=n)
      }
    } else if(type=="RR"){
      if(is.na(SD)){
        SD <- exp((((log(mean)-log(CI.95.LB)) + (log(CI.95.UB)-log(mean)))/2)/1.96)
      }
      exp(rnorm(n=1,mean=log(mean),sd=log(SD)))
    }
    
  }
  
}

# Run PSA
if (run.PSA==TRUE){
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
        time.start <-Sys.time()
        new.value.inputs <- generate.new.inputs()
        write.inputs(new.value.inputs)
        new.value.outputs <- read.outputs()
        time.elapsed <- Sys.time()-time.start
        if(time.elapsed<1.1){Sys.sleep(1.1-time.elapsed)}
        new.value.outputs
    }
    
    batch.iteration <- function(i.batch){
      if(suppress.gs4.messages==TRUE){
        results.internal <- do.call(rbind,replicate(batch.size,with_gs4_quiet(single.iteration()),simplify=FALSE))
      } else {
        results.internal <- do.call(rbind,replicate(batch.size,single.iteration(),simplify=FALSE))
      }
      
      if (i.batch == 1){
        results <<- results.internal
      } else {
        results <<- rbind(results,results.internal)
      }
      save(results,file=filename.for.storage)
    }
  
  # Run the PSA in batches (note: iterative saving is inefficient, but ensures not losing all data in case of error)
    pblapply(1:(ceiling(iterations/batch.size)),function(x) batch.iteration(x))
  # Reset models back to original inputs
    write.inputs(orig.value.inputs)
}

# Output results
{
  # Gather data
    load(file=filename.for.storage)
    results.long <- gather(results, Program, CE, factor_key=TRUE)
  # Charts
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

    
    plotlist <- list()
    
    for (i in 1:2){
        plotlist[[ paste0("histsubplot.",i) ]] <- ggplot(results.long[results.long$Program==levels(results.long$Program)[i],], aes(CE,fill=Program)) + 
          geom_density(alpha=.5)+
          theme_bw()+
          theme(
            legend.title = element_blank(),
            #panel.grid = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank()
          )+
          scale_x_continuous(limits=c(-10,40),breaks=seq(from=-10,to=40,by=10))+
          ylab(levels(results.long$Program)[i])
    }
    
    plot_grid(plotlist[[1]],plotlist[[2]],plotlist[[3]],plotlist[[4]],plotlist[[5]],
              nrow=5,align = "v")
    
    plot.2 <- ggplot(results.long[results.long$Program==levels(results.long$Program)[2],], aes(CE,fill=Program)) + 
      geom_density(alpha=.5)+
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
}




# ggplot(results.long, aes(x = CE, y = Program)) + 
#   geom_density_ridges()


  