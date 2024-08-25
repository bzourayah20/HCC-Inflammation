#Heatmap Inflammation DEGs 
{
  exprAll <- read_delim("TCGA Expression Normals & HCC.csv")
  exprAll <- exprAll %>% filter(!is.na(exprAll$...1))
  exprAll <- exprAll %>% remove_rownames() %>%
    column_to_rownames(var = "...1")
  inflammationDEGs <- exprAll[rownames(exprAll) %in% t,]
  metaAll <- read_delim("Meta TCGA HCC & normal.csv")
  metaAll <- metaAll %>% remove_rownames() %>%
    column_to_rownames(var = "Patient_ID")
  all(metaAll$Patient_ID == colnames(inflammationDEGs))
  inflammationDEGs <- inflammationDEGs[,order(colnames(inflammationDEGs))]
  metaAll <- metaAll[order(metaAll$Patient_ID),]
  library(pheatmap)
  
  colorsHM <- list(`Diagnosis` = c("HCC" = "#FC5C52", 
                                   "Normal" = "#5283FC"))
  breaksList = seq(-10, 10, by = 0.5)
  
  pheatmap(log(1+degs.expr,2), show_colnames = F,
           fontsize_row = 3,scale = "row",show_rownames = T,
           color = colorRampPalette(c("blue", "white", "red"))(50),
           main = "Inflammation DEGs",border_color = NA,
           cluster_cols = F, annotation_colors = colorsHM,
           annotation_col = metaAll)
  
  ggsave("Figure 2C.jpeg", width = 6.5,height = 4.5, units = "in", dpi = 600)
}
