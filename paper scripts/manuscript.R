# Renders the manuscript figures/tables and the supplementary material

  insert_head()

# Rendering the paper ------
  
  insert_msg('Rendering the paper')
  
  render('./paper/markdown/figures_and_tables.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper')
  
# Rendering the supplementary material -----
  
  insert_msg('Rendering the supplementary material')
  
  render('./paper/markdown/supplementary_material.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper')
  
# END ----
  
  insert_tail()