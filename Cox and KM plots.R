#Cox
{
  dependent_os <- "Surv(OS_time, OS_status)"
  explanatory <- colnames(multicoxdata)[1:6]
  
  colnames(new_expr_clinical) <- gsub("\.", "", colnames(new_expr_clinical))
  
  coxdata %>%
    hr_plot(dependent_os, explanatory, remove_ref = T, 
            plot_opts = list(xlab("HR, 95% CI"), 
                             theme(axis.title = element_text(size=12)))) 
  
  cox$p_val <- gsub(".\,", "", cox$HR (univariable))
  cox$p_val <- gsub("p<", "", cox$p_val)
  cox$p_val <- gsub("p=", "", cox$p_val)
  cox$p_val <- gsub("p>", "", cox$p_val)
  cox$p_val <- gsub(")", "", cox$p_val)
  
  cox$p_val <- gsub(".\,", "", cox$HR (multivariable))
  cox$p_val <- gsub("p<", "", cox$p_val)
  cox$p_val <- gsub("p=", "", cox$p_val)
  cox$p_val <- gsub("p>", "", cox$p_val)
  cox$p_val <- gsub(")", "", cox$p_val)
  
  
  cox$p_val <- as.numeric(cox$p_val)
  cox <- na.omit(cox)
  
  cox_filtered <- filter(cox, p_val < 0.05) %>%
    as.data.frame()
}
#cutpointr 
{
  library(cutpointr)
  gene <- metaRisk[,c(2,3:4)]
  results <- cutpointr(gene, S100A9, OS_status)
  summary(results)
  
}
#KM Plots
{
  fit <- survfit(Surv(DFS_time,DFS_status)~risk_group, data = metaRisk)
  ggsurvplot(fit, data = metaRisk, 
             palette = c("red", "blue"), 
             pval = T, ggtheme =   theme_classic()+
               theme(text = element_text(face = "bold", size = 10,
                                         family = "serif", colour = "black"),
                     axis.text  = element_text(face = "bold",
                                               size = 12, color = "black"),
                     legend.text = element_text(size = 8,face = "bold"),
                     legend.title = element_blank(),
                     axis.title   = element_text(face = "bold", size = 10), 
                     line = element_line(size = 1)))+
    ylab("Disease-free survival (DFS)")+
    xlab("Time in Months")
  
  ggsave("DFS geo.jpeg", width = 6, height = 5, dpi = 600, units = "in")
  
  library(patchwork)
  p1 + p2
}