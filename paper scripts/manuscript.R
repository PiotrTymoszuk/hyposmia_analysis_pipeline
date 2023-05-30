# Renders the manuscript figures/tables and the supplementary material

  insert_head()

# Reading the bibliography ------
  
  insert_msg('Reading the bibiography')
  
  od_bib <- read_bib('./paper/markdown/hyposmia_biblio.bib')
  
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
  
# Rendering the rebuttal letter -------
  
  insert_msg('Rendering the rebuttal letter')
  
  render('./paper/markdown/rebuttal_letter.Rmd', 
         output_format = word_document2(number_sections = FALSE, 
                                        reference_docx = 'ms_template.docx'), 
         output_dir = './paper')
  
# END ----
  
  insert_tail()