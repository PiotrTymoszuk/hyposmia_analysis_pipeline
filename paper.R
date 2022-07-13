# Paper tables, figures and manuscript text

  library(plyr)
  library(tidyverse)
  library(cowplot)
  library(figur)
  library(soucer)
  library(exda)
  library(writexl)
  library(rmarkdown)
  library(knitr)
  library(bookdown)
  library(flextable)
  library(ggtext)
  library(glue)
  library(stringi)
  
  insert_head()
  
  source_all('./tools/project_tools.R', 
             message = TRUE, crash = TRUE)
  
# Paper scripts ----
  
  insert_msg('Paper scripts')
  
  c('./paper scripts/tables.R', 
    './paper scripts/figures.R', 
    './paper scripts/supplements.R', 
    './paper scripts/manuscript.R'
    ) %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ----
  
  insert_tail()
  