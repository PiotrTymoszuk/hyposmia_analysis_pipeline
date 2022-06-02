# Launches the complete pipeline 

  library(soucer)
  
  print(source_all(c('import.R', 
                     'exploration.R', 
                     'analysis.R', 
                     'paper.R'), 
                   message = TRUE, 
                   crash = FALSE))
  
  save.image()