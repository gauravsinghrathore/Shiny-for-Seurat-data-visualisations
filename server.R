
library(shiny)
library(Seurat)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  ################load respective seurat object to use for plotting########
  load_data <- eventReactive(input$timepoint,{
    
    if(input$timepoint == "day0 ES"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day0/d0es.rds")
      
    }

    else if(input$timepoint == "day1 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day1/d1d.rds")
      
    }
    
    else if(input$timepoint == "day1 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day1/d1v.rds")
      
    }
    
    else if(input$timepoint == "day2 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day2/d2d.rds")
      
    }
    
    else if(input$timepoint == "day2 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day2/d2v.rds")
      
    }
    
    else if(input$timepoint == "day5 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day5/d5d.rds")
      
    }
    
    else if(input$timepoint == "day5 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day5/d5v.rds")
      
    }
    
    else if(input$timepoint == "day9 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day9/d9d.rds")
      
    }
    
    else if(input$timepoint == "day9 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day9/d9v.rds")
      
    }
    
    
    else if(input$timepoint == "day14 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day14/d14d.rds")
      
    }
    
    else  if(input$timepoint == "day14 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day14/d14v.rds")
    }
    
    else  if(input$timepoint == "day21 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day21/d21d.rds")
    }
    
    else  if(input$timepoint == "day21 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day21/d21v.rds")
    }
    
    else if(input$timepoint == "day35 dorsal"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day35/d35d.rds")
      
    }
    
    else if(input$timepoint == "day35 ventral"){
      q <- readRDS(file = "/scratch/mistr-atlas/new/day35/d35v.rds")
      
    }
    
    
    return(q)
    
  })
  
  ##############load cluster marker table#########################
  load_marker <- eventReactive(input$timepoint,{
    if(input$timepoint == "day0 ES"){
      a <- read.table("/scratch/mistr-atlas/new/day0/d0es.markers")
    }
    
    else if(input$timepoint == "day1 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day1/d1d.markers")
    }
    
    else if(input$timepoint == "day1 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day1/d1v.markers")
    }
    
    else if(input$timepoint == "day2 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day2/d2d.markers")
    }
    
    else if(input$timepoint == "day2 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day2/d2v.markers")
    }
    
    else if(input$timepoint == "day5 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day5/d5d.markers")
    }     
    
    else if(input$timepoint == "day5 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day5/d5v.markers")
    }     
    
    else if(input$timepoint == "day9 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day9/d9d.markers")
    }  
    
    else if(input$timepoint == "day9 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day9/d9v.markers")
    }  
    
    else if(input$timepoint == "day14 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day14/d14d.markers")
    } 
    
    else if(input$timepoint == "day14 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day14/d14v.markers")
    } 
    
    else if(input$timepoint == "day21 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day21/d21d.markers")
    }   
    
    else if(input$timepoint == "day21 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day21/d21v.markers")
    } 
    
    else if(input$timepoint == "day35 dorsal"){
      a <- read.table("/scratch/mistr-atlas/new/day35/d35d.markers")
    } 
    
    else if(input$timepoint == "day35 ventral"){
      a <- read.table("/scratch/mistr-atlas/new/day35/d35v.markers")
    } 
    
    return(a)
  })
  
  ###########render marker table########
  output$marker_table =  DT::renderDataTable({
    load_marker()
  })
  
  #####################render first cluster plot##############
  output$clusterplot <- renderPlot({
    
    rds <- load_data()
    
    x <-   DimPlot(rds, reduction = "umap",label = T)
    
    return(x)
  })
  
  #####################render first feature plot##############
  graph <- eventReactive(input$GO,{
    
    rds <- load_data()
    y<- as.vector(unlist(strsplit(input$gene," ")))
    x <-   FeaturePlot(rds, features = y,order = T)
    
    return(x)
  })
  
  output$featureplot <- renderPlot( graph() )
  ############render dotplot################
  output$dotplot <- renderPlot({
    rds <- load_data()
    m <- load_marker()
    top_markers <- m %>% group_by(cluster) %>% top_n(n = 5, wt = avg_log2FC)
    marker_list <- unique(top_markers$gene)
    
    plot <- DotPlot(rds, features = marker_list) + RotatedAxis()
    
    
    return(plot)
    
  })
  
  ###########render violin plot#########
  #####################render first feature plot##############
  graph_violin <- eventReactive(input$GO,{
    
    rds <- load_data()
    y<- as.vector(unlist(strsplit(input$gene," ")))
    x <-   VlnPlot(rds, features = y)
    
    return(x)
  })
  
  output$violinplot <- renderPlot(graph_violin())
  
})


#output$dl <- downloadHandler( load_marker())

