#Volcano plot HCC vs. Normal
{
  degslistall <- read_delim("DEGs list (not filtered).csv")
  degslistall <- degslistall %>% filter(!is.na(degslistall$...1))
  degslistall <- degslistall %>% 
    remove_rownames() %>% 
    column_to_rownames(var = "...1")
  
  keyvals <- ifelse(degslistall$log2FoldChange <= -0.5 & degslistall$padj<0.05, 'blue',
                    ifelse(degslistall$log2FoldChange > 0.5 & degslistall$padj<0.05, 'red','#979797'))
  keyvals[is.na(keyvals)] <- '#979797'
  names(keyvals)[keyvals == 'red'] <- 'Upregulated'
  names(keyvals)[keyvals == '#979797'] <- 'NS'
  names(keyvals)[keyvals == 'blue'] <- 'Downregulated'
  
  EnhancedVolcano(degslistall,
                  lab = rownames(degslistall),
                  x = 'log2FoldChange',
                  y = 'padj',
                  selectLab = rownames(degslistall)[which(names(keyvals) %in% c('Upregulated', 'Downregulated'))],
                  xlab = bquote(~Log[2]~ 'fold change'),
                  colCustom = keyvals,
                  pCutoff = 0.05,
                  FCcutoff = 0.05,
                  pointSize = 2,
                  labSize = 2,
                  labCol = 'black',
                  labFace = 'bold',
                  boxedLabels = T,colAlpha = 0.7,
                  legendPosition = 'top',
                  #legendLabSize = 13,
                  legendIconSize = 3,max.overlaps = 20,
                  drawConnectors = TRUE,
                  widthConnectors = 1.0,xlim = c(-5.5, 6),
                  gridlines.major = F, gridlines.minor = F, caption = "",
                  colConnectors = 'black',
                  legendLabSize = 10.5, title = "", subtitle = "Normal vs. HCC") +
    theme(axis.text.x = element_text(colour = "black", size =10),
          axis.text.y = element_text(colour = "black", size = 10),
          axis.title.x = element_text(colour = "black", size =10),
          axis.title.y = element_text(colour = "black", size =10))
  
  
  
  
}