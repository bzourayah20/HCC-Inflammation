#Venn list
{
  inflammationGenes <- read_delim("inflammation genes (GeneCards).csv")
  sigDegs <- read_delim("degsSig.csv")
  x <- list(`Inflammation Genes` = inflammationGenes$Gene.Symbol,
            `HCC DEGs` = sigDegs$...1)
  library(ggVennDiagram)
  ggvenn(x,fill_color = c("red","blue"), fill_alpha = 0.5,
         stroke_color = NA, stroke_size = 3,
         set_name_size = 5,text_size = 5)
  intersect(inflammationGenes$Gene.Symbol, sigDegs$...1) -> t
  
}
