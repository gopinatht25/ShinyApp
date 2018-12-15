

shinyServer(function(input, output) {
  
  Dataset <- reactive({
    
    if (is.null(input$file1)) {   # locate 'file1' from ui.R
      
                  return(NULL) } else{
      
      Data <- as.data.frame(
        read.csv(input$file1$datapath, header=TRUE, sep = ","))
      
      rownames(Data) = Data[,1]
      Data1 = Data[,2:ncol(Data)]
      return(Data1)
    }
  })

# Calc and render plot    
output$plot1 = renderPlot({ 
  
    data.pca <- prcomp(Dataset(), center = TRUE, scale. = TRUE)
    plot(data.pca, type = "l"); abline(h=1)    
  
       })

clusters <- reactive({
  kmeans(Dataset(), input$clusters)
})

output$clust_summary = renderTable({
  out = data.frame(Cluser = row.names(clusters()$centers),clusters()$centers)
  out
})

output$clust_data = renderDataTable({
  out = data.frame(row_name = row.names(Dataset()),Dataset(),Cluster = clusters()$cluster)
  out
})
  
})
