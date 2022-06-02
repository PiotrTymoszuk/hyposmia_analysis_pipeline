# Renders the manuscript figures/tables and the supplementary material

  insert_head()
  
# Exporting the figure chunks -----
  
  insert_msg('Figure chunks')
  
  insert_figure(figures$consort, 
                figures$sympt_recovery, 
                figures$sympt_mds, 
                figures$apriori, 
                figures$clustering, 
                figures$sympt_clusters, 
                figures$base_clusters, 
                figures$recovery_clusters, 
                file = './paper/markdown/figure_chunks.Rmd', 
                ref_names = stri_replace_all(names(figures), 
                                             fixed = '_', 
                                             replacement = '-'), 
                captions = c('Flow diagram of the analysis inclusion process for the CovILD and HACT studies.', 
                             'Symptom-specific recovery times in ambulatory COVID-19.', 
                             'Self-reported smell and taste disorders are isolated symptoms of long COVID and PASC.', 
                             'High extent of co-occurrence of smell and taste disorders in long COVID and PASC.', 
                             'Clustering of ambulatory COVID-19 individuals by symptom-specific recovery times.', 
                             'Duration of neurocognitive and respiratory symptoms, fatigue, smell and taste disorders differs between the COVID-19 recovery clusters.', 
                             'COVID-19 recovery clusters differ in sex distribution, comorbidity and daily medication rates.', 
                             'Good clinical, mental and psychosocial recovery in individuals with persistent smell and taste disorders'))
  
# Exporting the supplement chunks -----
  
  insert_msg('Supplement chunks')
  
  insert_figure(suppl$hact_freq, 
                suppl$hact_recovery, 
                suppl$covild_freq, 
                suppl$covild_recovery, 
                suppl$hyposmia_rater100, 
                suppl$hyposmia_rater360, 
                suppl$mds_acute, 
                suppl$clust_devel, 
                suppl$cov_clusters,
                suppl$fatigue_clsters, 
                suppl$neurocog_clusters, 
                suppl$anosmia_clusters, 
                suppl$rec_clusters, 
                suppl$psych_clusters, 
                file = './paper/markdown/supplement_chunks.Rmd', 
                ref_names = stri_replace_all(names(suppl), 
                                             fixed = '_', 
                                             replacement = '-'), 
                captions = c('Frequency of COVID-19 symptoms in acute COVID-19, long COVID and PASC in the HACT study.', 
                             'Kinetic of recovery from leading acute COVID-19 symptoms in the HACT study.', 
                             'Symptom frequency in ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
                             'Kinetic of recovery from smell disorders, reduced performance and dyspnea in ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
                             'Rates of self-reported hyposmia and hyposmia in sniffing stick test at 3-month post COVID-19 follow-up in the ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
                             'Rates of self-reported hyposmia and hyposmia in sniffing stick test at 1-year post COVID-19 follow-up in the ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
                             'Multi-dimensional scaling analysis of acute COVID-19 symptoms in the HACT study.', 
                             'Definition of the COVID-19 recovery clusters and clustering feature importance in the HACT study.',
                             'Numbers of acute COVID-19 and long COVID symptoms in the HACT study recovery clusters.', 
                             'Recovery from fatigue, tiredness and tachypnea in the HACT study recovery clusters.', 
                             'Recovery from neurocognitive symptoms of COVID-19 in the HACT study recovery clusters.', 
                             'Recovery from smell and taste disorders in the HACT study recovery clusters', 
                             'Physical performance loss, impairment of quality of life and self-perceived complete convalescence in the HACT study recovery clusters.', 
                             'Rating of anxiety, depression, overall mental health impairment and stress in the HACT study recovery clusters.'))
  
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