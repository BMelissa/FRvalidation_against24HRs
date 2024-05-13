setwd("C:/Users/bm00900/OneDrive - University of Surrey/Desktop/Ph.D/AVANA study/Data/Data.R")

#LIBRARIES

library(magrittr)
library(dplyr)
library(car)
library(ggplot2)
library(tidyverse)
library(irr)
library(ggpubr)
library(tidyr)
library(lme4)
library(networkD3)


#-----IMPORT AND FILTER DATA, CREATE DF for subsequent analysis----------------------------------------------------------------------------------------

#import data
In24 <- read.delim("Results_24HRs1.txt")
In24 <- In24[-(295:480),]
Libro <- read.delim("Results_Libro1.txt")

#INTAKE24
#filter days - remove Kcal; >5000 as might be due to technical errors 
In24_D <- In24[grep("day", In24$Day), c("ID", "Day", "Energy..kcal.")]
In24_D <- In24_D[- (c(which((In24_D$Energy..kcal. > 5000)))),] 

#create new rows for mean and median values per each participant
In24_D <- In24_D %>%
  group_by(ID) %>%
  mutate(Kcal_mean = mean(Energy..kcal., na.rm = TRUE), Kcal_median = median(Energy..kcal., na.rm = TRUE))


#LIBRO
Libro_D <- Libro[grep("day", Libro$Day), c("ID", "Day", "Energy.kcal.")]
Libro_D <- Libro_D[- (c(which((Libro_D$Energy.kcal. > 5000)))),]

Libro_D <- Libro_D %>%
  group_by(ID) %>%
  mutate(Kcal_mean = mean(Energy.kcal., na.rm = TRUE), Kcal_median = median(Energy.kcal., na.rm = TRUE))


#check intersection between libro and In24
intersect((unique(In24_D$ID)), (unique(Libro_D$ID)))

#P50 is missing so exclude it
Libro_D <- Libro_D[- (c(which(Libro_D$ID == "P50"))), ]



#create a df with unique row per ID and summary measure

summary_df <- data.frame ("ID" = c(1:33,35:49), "In24_mean" = unique(In24_D$Kcal_mean), "In24_med" = unique(In24_D$Kcal_median), 
                             "Libro_mean" = unique(Libro_D$Kcal_mean), "Libro_med" = unique(Libro_D$Kcal_median))

#P19 is 13 years old so has to be excluded
In24_D <- In24_D[- (c(which(In24_D$ID == "P19"))), ]
Libro_D <- Libro_D[- (c(which(Libro_D$ID == "P19"))), ]
summary_df <- summary_df[- (c(which(summary_df$ID == 19))), ]

#check for normality and correlation among means and medians
qqPlot(unique(In24_D$Kcal_median))
qqPlot(unique(In24_D$Kcal_mean))
cor.test(In24_D$Kcal_mean, In24_D$Kcal_median, method = "pearson")

qqPlot(unique(Libro_D$Kcal_median))
qqPlot(unique(Libro_D$Kcal_mean))
cor.test(Libro_D$Kcal_mean, Libro_D$Kcal_median, method = "pearson")

#qqplot are ok so we can now work with means of single recalls


#-----DAY TO DAY VARIATION----------------------------------------------------------------------------------------------

#INTAKE24
#calculate individual and mean Kcal ranges (medians based on qqplot results)
In24_D <- In24_D %>% group_by(ID) %>% mutate(range_Kcal = max(Energy..kcal.,na.rm = TRUE) - min(Energy..kcal.,na.rm = TRUE))
qqPlot(unique(In24_D$range_Kcal)) #not normally distributed so use median for summary stats
median(unique(In24_D$range_Kcal)) #calculate median
quantile(unique(In24_D$range_Kcal), probs = c(0.25, 0.75)) #calculate Q1 and Q3


#scatterplot
range_in24_p <- ggplot(In24_D, aes(x=ID, y = Energy..kcal., color = ID)) +
  geom_point() +
  stat_summary(fun = "identity", geom = "line") +
  scale_y_continuous(breaks = seq(0, 7000, by = 500)) +
  coord_cartesian(ylim = c(0, 7000)) +
  guides(color = "none") +
  theme_classic() +
  labs(x  = "Participants", y = "EI (Kcal)") +
  theme(axis.title = element_text(size = 14, face = "bold")) +
  annotate(geom = "text", x= 9, y= 6500, label = "975.9 [583.1 - 1577.4]") + annotate(geom = "text", x= 9.2, y= 6900, label = "Intake24 - Median [IQR]:") +
  scale_x_discrete(breaks = NULL) 

range_in24_p

#LIBRO
Libro_D <- Libro_D %>% group_by(ID) %>% mutate(range_Kcal = max(Energy.kcal.,na.rm = TRUE) - min(Energy.kcal.,na.rm = TRUE))         
qqPlot(unique(Libro_D$range_Kcal))
median(unique(Libro_D$range_Kcal))
quantile(unique(Libro_D$range_Kcal), probs = c(0.25, 0.75))

range_Libro_p <- ggplot(Libro_D, aes(x=ID, y = Energy.kcal., color = ID)) +
  geom_point() +
  stat_summary(fun = "identity", geom = "line") +
  scale_y_continuous(breaks = seq(0, 7000, by = 500)) +
  coord_cartesian(ylim = c(0, 7000)) +
  guides(color = "none") +
  theme_classic() +
  labs(x  = "Participants", y = "EI (Kcal)") +
  theme(axis.title = element_text(size = 14, face = "bold")) +
  annotate(geom = "text", x= 9, y= 6500, label = "754.8.8 [539.7 - 1360.4]") + annotate(geom = "text", x= 9.2, y= 6900, label = "Libro - Median [IQR]:") +
  scale_x_discrete(breaks = NULL) 

range_Libro_p

#arrange two plots in one figure
ggarrange(range_in24_p, range_Libro_p,ncol = 1, labels = c("A.", "B."))

#t test # np as ranges are not normally distributed
wilcox.test(unique(In24_D$range_Kcal), unique(Libro_D$range_Kcal), paired = T)



#----INTRACLASS CORRELATION COEFFICIENT-----------------------------------------------------------------------------------

#INTAKE24
#clean and organize dataframe
In24_D$Day <- gsub(" ", "", In24_D$Day)
In24_wD <- pivot_wider(In24_D, names_from = Day, values_from = Energy..kcal.)

#ICC
icc_in <- icc(In24_wD[5:8], model = "twoway", type = "agreement", unit ="average") 

#LIBRO
Libro_D$Day <- gsub(" ", "", Libro_D$Day)
Libro_wD <- pivot_wider(Libro_D, names_from = Day, values_from = Energy.kcal.)

icc_L <- icc(Libro_wD[5:8], model = "twoway", type = "agreement", unit ="average") 


#----DAY CORRELATION WITHIN METHODS-------------------------------------------------------------------------------------

cor.test(In24_wD$day1, In24_wD$day2, method = "spearman") #0.60
cor.test(In24_wD$day1, In24_wD$day3, method = "spearman") #0.55
cor.test(In24_wD$day1, In24_wD$day4, method = "spearman") #0.62
cor.test(In24_wD$day2, In24_wD$day3, method = "spearman") #0.62
cor.test(In24_wD$day2, In24_wD$day4, method = "spearman") #0.71
cor.test(In24_wD$day3, In24_wD$day4, method = "spearman") #0.57


cor.test(Libro_wD$day1, Libro_wD$day2, method = "spearman") #0.59
cor.test(Libro_wD$day1, Libro_wD$day3, method = "spearman") #0.58
cor.test(Libro_wD$day1, Libro_wD$day4, method = "spearman") #0.48
cor.test(Libro_wD$day2, Libro_wD$day3, method = "spearman") #0.61
cor.test(Libro_wD$day2, Libro_wD$day4, method = "spearman") #0.50
cor.test(Libro_wD$day3, Libro_wD$day4, method = "spearman") #0.64

#-----CORRELATION COEFFICIENT-----------------------------------------------------------------------------------------------


#test corr #pearson with means is ok based on QQ plot
cor_test_means <- cor.test(summary_df$In24_mean, summary_df$Libro_mean, method = "pearson") #0.31 
#cor_test_med <- cor.test(summary_df$In24_med, summary_df$Libro_med, method = "spearman") #0.32

#deattenuated corr coeff
corR <- cor_test_means[["estimate"]]/sqrt(icc_in[["value"]]*icc_L[["value"]]) #0.37 #0.39 if medians are used
corR

#corr plot
corr_plot <- ggplot(summary_df, aes(In24_mean, Libro_mean)) + geom_point() + stat_cor() + geom_smooth(method = "lm", se = F) + 
  theme_classic() + labs( y = "Libro (Kcal)", x = "Intake24 (Kcal)") + theme(axis.title = element_text(size = 14, face = "bold"))
corr_plot 

#-----BLAND ALTMAN PLOT-------------------------------------------------------------------------------------------------------

#create columns for variables
summary_df$mean_avg <- rowMeans(summary_df[c(2,4)])
summary_df$mean_diff <- summary_df$Libro_mean - summary_df$In24_mean
#check normality 
qqPlot(summary_df$mean_diff)
#as the qqplot doesn't look good at all, I would stick to MEDIANS. indeed:
#qqPlot((summary_df$Libro_med)-(summary_df$In24_med)) #looks better

#create columns for avg and diff of median values
summary_df$med_avg <- rowMeans(summary_df[c(3,5)])
summary_df$med_diff <- summary_df$Libro_med - summary_df$In24_med
qqPlot(summary_df$med_diff)

#calculate the bias
bias <- mean(summary_df$med_diff)
bias
#find the confidence interval
lower_CI <- bias - 1.96 * sd(summary_df$med_diff) / sqrt(length(summary_df$med_diff))
upper_CI <- bias + 1.96 * sd(summary_df$med_diff) / sqrt(length(summary_df$med_diff))

#find the limits of agreement
lower_loa <- bias - 1.96 * sd(summary_df$med_diff)
upper_loa <- bias + 1.96 * sd(summary_df$med_diff)

#plot
BA_p_med <- ggplot(summary_df, aes(med_avg, med_diff)) +
  geom_point(size = 2) +
  geom_hline(yintercept = bias, label = "Bias") +
  #geom_hline place a horizontal line with y value = bias and label = bias
  geom_hline(yintercept = lower_loa, color = "red", linetype = "dashed") +
  geom_hline(yintercept = upper_loa, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "1F5FAC", linetype = "dashed") +
  ggtitle("Bland-Altman plot") +
  ylab("Difference between medians") +
  coord_cartesian(ylim = c(-4000, 2000)) + #gives min and max values to the y line
  xlab("Average medians") + 
  geom_smooth(method = "lm", se = FALSE) + #add a regression line
  theme(axis.title = element_text(size = 14, face = "bold")) +
  geom_text(aes(x = Inf, y = bias, label = sprintf("Bias: %.2f", bias)), hjust = 1.1, vjust = - 0.2) +   
                #geom_text adds text annotations 
                #x = inf places the text at the right edge of the plot
                #y = bias set the y value position
                #label = creates a string that starts with "Bias:" followed by the value of the bias variable formatted to two decimal places. 
                #The %.2f is a format specifier where % indicates that a variable's value will be inserted, 
                #.2 specifies two decimal places, and f stands for floating-point number.
                #hjust = adjust the horizontal position to avoid overlap. 1.1 pushes the text slightly further to the right, 
                #beyond the plot's boundary by 10% of the text's width. 
                #same for vjust
  geom_text(aes(x = Inf, y = lower_loa, label = sprintf("Lower LOA: %.2f", lower_loa)), hjust = 1.1, vjust = - 0.5, color = "red") +
  geom_text(aes(x = Inf, y = upper_loa, label = sprintf("Upper LOA: %.2f", upper_loa)), hjust = 1.1, vjust = - 0.5, color = "red")
BA_p_med

#-----t TEST---------------------------------------------------------------------------------------------------------------------------------

#given that qqplots for means look better than for medians, I'll use means and parametric stat
test <- t.test(summary_df$In24_med, summary_df$Libro_med, alt = "two.sided", paired = TRUE) 


#-----CROSS CLASSIFICATION------------------------------------------------------------------------------------------------------

#add column to identify quartile categories
summary_df$In24_mean_Q <- ntile(summary_df$In24_mean, 4)
summary_df$Libro_mean_Q <- ntile(summary_df$Libro_mean, 4)

#count % of sample that fall within the same cat
count <- 0

for (i in 1:length(summary_df$ID)) {
  if (summary_df$In24_mean_Q[i] == summary_df$Libro_mean_Q[i]) {
    count = count + 1} 
}

count_sameQ <- round((count/length(summary_df$ID)) * 100, digits = 2)

#count % of sample that fall within opposite cat
count <- 0

for (i in 1:length(summary_df$ID)) {
  if ((summary_df$In24_mean_Q[i] == 1 & summary_df$Libro_mean_Q[i] == 4) | (summary_df$In24_mean_Q[i] == 4 & summary_df$Libro_mean_Q[i] == 1)) {
    count = count + 1}
}

countoppQ <- round((count/length(summary_df$ID)) * 100, digits = 2)


#Based on this results, 27% of the sample is correctly classified, 4.2% falls in opposite cat.


#VISUALIZE
#df where you store SOURCE (IN24 Q), TARGET (LIBRO Q) AND WEIGHT (NUMBER OF ID flowing)
link_df <- data.frame(
  source = c(rep(0,4), rep(1,4), rep(2,4), rep(3,4)), # 0-3 for Method 1
  target = c(4,5,6,7, 4,5,6,7, 4,5,6,7, 4,5,6,7),     # 4-7 for Method 2
  weight = c(
    sum(summary_df$In24_mean_Q == 1 & summary_df$Libro_mean_Q ==1),
    sum(summary_df$In24_mean_Q == 1 & summary_df$Libro_mean_Q ==2),
    sum(summary_df$In24_mean_Q == 1 & summary_df$Libro_mean_Q ==3),
    sum(summary_df$In24_mean_Q == 1 & summary_df$Libro_mean_Q ==4),
    sum(summary_df$In24_mean_Q == 2 & summary_df$Libro_mean_Q ==1),
    sum(summary_df$In24_mean_Q == 2 & summary_df$Libro_mean_Q ==2),
    sum(summary_df$In24_mean_Q == 2 & summary_df$Libro_mean_Q ==3),
    sum(summary_df$In24_mean_Q == 2 & summary_df$Libro_mean_Q ==4),
    sum(summary_df$In24_mean_Q == 3 & summary_df$Libro_mean_Q ==1),
    sum(summary_df$In24_mean_Q == 3 & summary_df$Libro_mean_Q ==2),
    sum(summary_df$In24_mean_Q == 3 & summary_df$Libro_mean_Q ==3),
    sum(summary_df$In24_mean_Q == 3 & summary_df$Libro_mean_Q ==4),
    sum(summary_df$In24_mean_Q == 4 & summary_df$Libro_mean_Q ==1),
    sum(summary_df$In24_mean_Q == 4 & summary_df$Libro_mean_Q ==2),
    sum(summary_df$In24_mean_Q == 4 & summary_df$Libro_mean_Q ==3),
    sum(summary_df$In24_mean_Q == 4 & summary_df$Libro_mean_Q ==4)
  )
)

#df where you store names of source and target
nodes_df <- data.frame(
  name = c('Q1 Intake24', 'Q2 Intake24', 'Q3 Intake24', 'Q4 Intake24',
           'Q1 Libro', 'Q2 Libro', 'Q3 Libro', 'Q4 Libro')
)

#  The 'source' and 'target' columns must be numeric indices, not factors
link_df$source <- as.numeric(link_df$source)
link_df$target <- as.numeric(link_df$target)



sankey <- sankeyNetwork(Links = link_df, Nodes = nodes_df,
                        Source = "source", Target = "target", Value = "weight", NodeID = "name",
                        units = "Participants", fontSize = 16, nodeWidth = 30)


sankey

#arrange indiv analysis on the same image
ind_analysis <- ggarrange(corr_plot, sankey, nrow = 1, labels = c("A.", "(B.)"))

#-----WEIGHTED KAPPA STATS------------------------------------------------------------------------------------------------------------
kappa_stats <- kappa2(summary_df[,8:9], weight = "squared") 

#kappa stats for cat (Q) derived from means is not good (0.27) and not significant (p = 0.06).
#kappa stats for cat (Q) derived from medians is slightly higher (0.37) and significant (p=0.01)




#MIXED MODEL------------------------------------------------------------------------------------------------------------------------------

library(lmerTest)

T <- read.delim("Table_mix_models.txt")
T <- T[-7]
colnames(T)[1] <- "ID"
T <- T[-(c(97:100, which(T$ID == "P19"))), ]

T$ID <- as.factor(T$ID)
T$Meth <- as.factor(T$Meth)
T$Adh <- as.factor(T$Adh)
T$Order <- as.factor(T$Order)

lm1 <- lmer(Mean ~ Order + Meth + (1|ID), data = T)
summ_lm1 <- summary(lm1)


# Check model assumptions

library(performance)

check_model(lm1)


------------------------
  # Calculate the bias and its confidence interval


lower_CI <- bias - 1.96 * sd(summary_df$med_diff) / sqrt(length(summary_df$med_diff))
upper_CI <- bias + 1.96 * sd(summary_df$med_diff) / sqrt(length(summary_df$med_diff))

# Calculate the limits of agreement
lower_loa <- bias - 1.96 * sd(summary_df$med_diff)
upper_loa <- bias + 1.96 * sd(summary_df$med_diff)




