#DATA SUMMARY and VISUALIZATION
library(tibble)
library(officer)
library(flextable)
library(reshape2)
library(ggplot2)
library(car)
library(pastecs)
library(ggsignif)


#summary stats----------------------------------------------------------------------------------------------------------------
#in24 #create vector withh stats summarised
In24_s <- round(summary(summary_df$In24_mean),2)
In24_s2 <- round(stat.desc(summary_df$In24_mean),2)
In24_ss <- c(In24_s[c(1,2,3,5,6)], In24_s2[c(6,9, 13)])

#Libro
Libro_s <- round(summary(summary_df$Libro_mean),2)
Libro_s2 <- round(stat.desc(summary_df$Libro_mean),2)
Libro_ss <- c(Libro_s[c(1,2,3,5,6)], Libro_s2[c(6,9, 13)])

ss_df <- data.frame(In24 = In24_ss, Libro = Libro_ss)
ss_df <- rownames_to_column(ss_df, var = "summary statistics")

#export
ss_docx <- read_docx()
ss_table <- flextable(ss_df) %>% 
  set_table_properties(layout = "autofit") %>%
  theme_zebra()

ss_docx <- ss_docx %>%
  body_add_flextable(value = ss_table)

print(ss_docx, target = "/Users/bm00900/OneDrive - University of Surrey/Desktop/Ph.D/AVANA study/Data/Data.R/ss_docx.docx")


#visualization---------------------------------------------------------------------------------------------------------------
#melt df in a longer format
ss_df_melt <- summary_df %>%
  melt(id.vars = "ID", measure.vars = c("In24_mean", "Libro_mean"))
colnames(ss_df_melt) <- c("ID", "Assessment method", "EI(Kcal)")
ss_df_melt$`Assessment method` <- gsub("_mean", "", ss_df_melt$`Assessment method`)
ss_df_melt$`Assessment method` <- gsub("In24", "Intake24", ss_df_melt$`Assessment method`)


#violin plot
EI_p <- ggplot(ss_df_melt, aes(`Assessment method`, `EI(Kcal)`, fill = `Assessment method`)) +
     geom_violin(aes(color = `Assessment method`)) +
     scale_fill_manual(values = c("orange", "light blue"), guide = NULL) +
     geom_boxplot(aes(fill = NULL), width = 0.2, outlier.shape = 25, outlier.size = 3, outlier.fill = "black", show.legend = FALSE) +
     stat_summary(geom = "point", fun = "mean", show.legend = FALSE) +
     theme_classic() +
     labs(x = NULL, y = "EI(Kcal)") +
     theme(axis.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 14, face = "bold")) +
     geom_signif(comparisons = list(c("Intake24", "Libro")), map_signif_level = TRUE)
  
EI_p    

  
#merge group analysis figures in one unique fig
group_analysis <- ggarrange(BA_p_med, EI_p, nrow = 1, labels = c("A.", "B."))
group_analysis



