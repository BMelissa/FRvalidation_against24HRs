
#LIBRARIES

library(magrittr)
library(dplyr)
library(car)
library(tidyverse)
library(robustbase)
library(officer)
library(flextable)
library(irr)
library(ggpubr)


#-----IMPORT AND FILTER DATA---------------------------------------------------------------------------------------------------

#import data
In24 <- read.delim("Results_24HRs1.txt") 
In24 <- In24[-(295:480),]
Libro <- read.delim("Results_Libro1.txt")
Avana <- read.delim("AVANA_study_Masterfile.txt") 

#-----INTAKE24
#filter days - remove >5000kcal as might be due to technical errors, filter columns of N you want to analyse
In24_Dn <- In24[grep("day", In24$Day), c(1,2,9,6,7,8,12,16,36,11)]
In24_Dn <- In24_Dn[- (c(which((In24_Dn$Energy..kcal. > 5000)))), ]
#clean
In24_Dn$Day <- gsub(" ", "", In24_Dn$Day)
#create columns for individual means and medians of nutrients 
In24_Dn <- In24_Dn %>%
  group_by(ID) %>%
  mutate(across(2:9, ~mean(., na.rm = TRUE), .names = "{.col}_mean"),       #across starts from 0 I think
         across(2:9, ~median(., na.rm = TRUE), .names = "{.col}_med"))        #based on qqplot etc means and medians distributions look equally problematic so I'll use average measures across recall
#create a separate df with mean and median values
In24_means <- unique(In24_Dn[, c(1, 11:18)]) #means
In24_med <- unique(In24_Dn[, c(1, 19:26)]) #medians

#add sex factor
#In24_means$sex <- as.factor(Avana$Gender[-(c(34, 50:55))])

#-----LIBRO
Libro_Dn <- Libro[grep("day", Libro$Day), c(1,2,3,6,7,5,13,16,30,10)]
Libro_Dn <- Libro_Dn[- (c(which((Libro_Dn$Energy.kcal. > 5000)))),]

Libro_Dn$Day <- gsub(" ", "", Libro_Dn$Day)

Libro_Dn <- Libro_Dn %>%
  group_by(ID) %>%
  mutate(across(2:9, ~mean(., na.rm = TRUE), .names ="{.col}_mean")   ,  #across starts from 0
         across(2:9, ~median(., na.rm = TRUE), .names = "{.col}_med"))

Libro_means <- unique(Libro_Dn[ , c(1,11:18)]) #means
Libro_means <- Libro_means[-49,]

Libro_med <- unique(Libro_Dn[, c(1, 19:26)]) #medians
Libro_med <- Libro_med[-49,]

#Libro_means$sex <- as.factor(Avana$Gender[-(c(34, 50:55))])


#-----check for normality of mean and median values--------------------------------------------------------------------------------------------------------------------------------------------
qqPlot(Libro_means$Protein_mean) #no
qqPlot(log(Libro_means$Protein_mean)) #no
qqPlot(Libro_means$Fat_mean)
qqPlot(Libro_means$Carbohydrate_mean) #no
qqPlot(log(Libro_means$Carbohydrate_mean)) #no
qqPlot(Libro_means$Fibre_mean) #no
qqPlot(log(Libro_means$Fibre_mean)) #no
qqPlot(Libro_means$Free.Sugars_mean) #no
qqPlot(Libro_means$Trans.fatty.acids_mean) #no
qqPlot(Libro_means$Alcohol_mean) #no

qqPlot(Libro_med$Protein_med) #ish
qqPlot(Libro_med$Fat_med)
qqPlot(Libro_med$Carbohydrate_med) #ish 
qqPlot(Libro_med$Fibre_med) #no
qqPlot(log(Libro_med$Fibre_med)) #no
qqPlot(Libro_med$Free.Sugars_med) #no
qqPlot(Libro_med$Trans.fatty.acids_med) #no
qqPlot(Libro_med$Alcohol_med)

qqPlot(In24_means$Protein_mean)
qqPlot(In24_means$Fat_mean) #no
qqPlot(In24_means$Carbohydrate_mean)
qqPlot(In24_means$Englyst.fibre_mean) #no
qqPlot(In24_means$Non.milk.extrinsic.sugars_mean)
qqPlot(In24_means$Trans.FA_mean) #no
qqPlot(In24_means$Alcohol_mean) #no

qqPlot(In24_med$Protein_med) #no
qqPlot(In24_med$Fat_med)
qqPlot(In24_med$Carbohydrate_med) #ish
qqPlot(In24_med$Englyst.fibre_med) #no
qqPlot(In24_med$Non.milk.extrinsic.sugars_med)
qqPlot(In24_med$Trans.FA_med)
qqPlot(In24_med$Alcohol_med) #no

#-----DESCRIPTIVE STATISTICS----------------------------------------------------------------------------------------
#TABLE1

#for all nutrients both means and medians are presented
#as the aim is comparing our results to the NDNS and we have all participants + 20 but one who's 13, the latter will be excluded
In24_means <- In24_means[-19,]
Libro_means <- Libro_means[-19,]
In24_med <- In24_med[-19,]
Libro_med <- Libro_med[-19,]

#STEP1: CALCULATE NUTRIENT DENSITY---------

# % of total EI derived from each nutrient. 
#FORMULA to do so --> ((N(g) x Multiplication factor)/total EI)*100

#INTAKE24------
#create table with EI from each N
#multiplication factors:
#P x4
#F x9
#C x3.75
#F x2
#NMES x3.87
#ALCOHOL x7

In24_perc <- data.frame( "ID" = In24_means$ID, "EI(Kcal)" = In24_means$Energy..kcal._mean, "Protein" = In24_means$Protein_mean*4, 
                         "Fat" = In24_means$Fat_mean*9, "Carbohydrate" = In24_means$Carbohydrate_mean*3.75, 
                         "Fibers" = In24_means$Englyst.fibre_mean*2, "NMES" = In24_means$Non.milk.extrinsic.sugars_mean*3.87,
                         "TFA" = In24_means$Trans.FA_mean*9, "Alcohol" = In24_means$Alcohol_mean*7) #,"sex" = In24_means$sex)
#transform in percentage
In24_perc[3:9] <- In24_perc %>% 
  transmute(across(3:9, function(x) x/(In24_perc$Protein + In24_perc$Fat + In24_perc$Carbohydrate + In24_perc$Alcohol))*100) %>% round(digits = 2)

#create a summary table with EI(Kcal) and % means and medians 

#mean(sd)
Perc_summary <- In24_perc[2:9] %>%
  transmute(`Energy(Kcal)` = paste0(round(mean(In24_perc$EI.Kcal.),2), "(", round(sd(In24_perc$EI.Kcal.),2), ")"),
            `Protein(%)` = paste0(round(mean(In24_perc$Protein),2), "(", round(sd(In24_perc$Protein),2), ")"),
            `Fat(%)` = paste0(round(mean(In24_perc$Fat),2), "(", round(sd(In24_perc$Fat),2), ")"),
            `Carbohydrate(%)` = paste0(round(mean(In24_perc$Carbohydrate),2), "(", round(sd(In24_perc$Carbohydrate),2), ")"),
            `Fibers(%)` = paste0(round(mean(In24_perc$Fibers),2), "(", round(sd(In24_perc$Fibers),2), ")"),
            `Free sugars(%)` = paste0(round(mean(In24_perc$NMES),2), "(", round(sd(In24_perc$NMES),2), ")"),
            `Trans Fatty-Acids(%)` = paste0(round(mean(In24_perc$TFA),2), "(", round(sd(In24_perc$TFA),2), ")"),
            `Alcohol(%)` = paste0(round(mean(In24_perc$Alcohol),2), "(", round(sd(In24_perc$Alcohol),2), ")")) %>% unique()

#median(lower-upper 2.5 percentile) --> for comparison with NDND
Perc_summary[2,] <- In24_perc[2:9] %>%
  transmute(`Energy(Kcal)` = paste0(round(median(In24_perc$EI.Kcal.),2), "(", round(quantile(In24_perc$EI.Kcal., 0.025),2), "-", round(quantile(In24_perc$EI.Kcal., 0.975),2), ")"),
            `Protein(%)` = paste0(round(median(In24_perc$Protein),2), "(", round(quantile(In24_perc$Protein, 0.025),2), "-", round(quantile(In24_perc$Protein, 0.975),2),")"),
            `Fat(%)` = paste0(round(median(In24_perc$Fat),2), "(", round(quantile(In24_perc$Fat, 0.025),2), "-", round(quantile(In24_perc$Fat, 0.975),2), ")"),
            `Carbohydrate(%)` = paste0(round(median(In24_perc$Carbohydrate),2), "(", round(quantile(In24_perc$Carbohydrate, 0.025),2), "-", round(quantile(In24_perc$Carbohydrate, 0.975),2),")"),
            `Fibers(%)` = paste0(round(median(In24_perc$Fibers),2), "(", round(quantile(In24_perc$Fibers, 0.025),2), "-", round(quantile(In24_perc$Fibers, 0.975),2), ")"),
            `Free sugars(%)` = paste0(round(median(In24_perc$NMES),2), "(", round(quantile(In24_perc$NMES, 0.025),2), "-", round(quantile(In24_perc$NMES, 0.975),2), ")"),
            `Trans Fatty-Acids(%)` = paste0(round(median(In24_perc$TFA),2), "(", round(quantile(In24_perc$TFA, 0.025),2), "-", round(quantile(In24_perc$TFA, 0.975),2), ")"),
            `Alcohol(%)` = paste0(round(median(In24_perc$Alcohol),2), "(", round(quantile(In24_perc$Alcohol, 0.025),2), "-", round(quantile(In24_perc$Alcohol, 0.975),2),")")) %>% unique()
                                    

  
#LIBRO------
Libro_perc <- data.frame( "ID" = Libro_means$ID, "EI(Kcal)" = Libro_means$Energy.kcal._mean, "Protein" = Libro_means$Protein_mean*4, 
                         "Fat" = Libro_means$Fat_mean*9, "Carbohydrate" = Libro_means$Carbohydrate_mean*3.75, 
                         "Fibers" = Libro_means$Fibre_mean*2, "NMES" = Libro_means$Free.Sugars_mean*3.87,
                         "TFA" = In24_means$Trans.FA_mean*9, "Alcohol" = In24_means$Alcohol_mean*7) #,"sex" = Libro_means$sex)

Libro_perc[3:9] <- Libro_perc %>% 
  transmute(across(3:9, function(x) x/(Libro_perc$Protein+ Libro_perc$Fat + Libro_perc$Carbohydrate + Libro_perc$Alcohol))*100) %>% round(digits = 2)

Perc_summary[3,] <- Libro_perc[2:9] %>%
  transmute(`Energy(Kcal)` = paste0(round(mean(Libro_perc$EI.Kcal.),2), "(", round(sd(Libro_perc$EI.Kcal.),2), ")"),
            `Protein(%)` = paste0(round(mean(Libro_perc$Protein),2), "(", round(sd(Libro_perc$Protein),2), ")"),
            `Fat(%)` = paste0(round(mean(Libro_perc$Fat),2), "(", round(sd(Libro_perc$Fat),2), ")"),
            `Carbohydrate(%)` = paste0(round(mean(Libro_perc$Carbohydrate),2), "(", round(sd(Libro_perc$Carbohydrate),2), ")"),
            `Fibers(%)` = paste0(round(mean(Libro_perc$Fibers),2), "(", round(sd(Libro_perc$Fibers),2), ")"),
            `Free sugars(%)` = paste0(round(mean(Libro_perc$NMES),2), "(", round(sd(Libro_perc$NMES),2), ")"),
            `Trans Fatty-Acids(%)` = paste0(round(mean(Libro_perc$TFA),2), "(", round(sd(Libro_perc$TFA),2), ")"),
            `Alcohol(%)` = paste0(round(mean(Libro_perc$Alcohol),2), "(", round(sd(Libro_perc$Alcohol),2), ")")) %>% unique()

Perc_summary[4,] <- Libro_perc[2:9] %>%
  transmute(`Energy(Kcal)` = paste0(round(median(Libro_perc$EI.Kcal.),2), "(", round(quantile(Libro_perc$EI.Kcal., 0.025),2), "-", round(quantile(Libro_perc$EI.Kcal., 0.975),2), ")"),
            `Protein(%)` = paste0(round(median(Libro_perc$Protein),2), "(", round(quantile(Libro_perc$Protein, 0.025),2), "-", round(quantile(Libro_perc$Protein, 0.975),2),")"),
            `Fat(%)` = paste0(round(median(Libro_perc$Fat),2), "(", round(quantile(Libro_perc$Fat, 0.025),2), "-", round(quantile(Libro_perc$Fat, 0.975),2), ")"),
            `Carbohydrate(%)` = paste0(round(median(Libro_perc$Carbohydrate),2), "(", round(quantile(Libro_perc$Carbohydrate, 0.025),2), "-", round(quantile(Libro_perc$Carbohydrate, 0.975),2),")"),
            `Fibers(%)` = paste0(round(median(Libro_perc$Fibers),2), "(", round(quantile(Libro_perc$Fibers, 0.025),2), "-", round(quantile(Libro_perc$Fibers, 0.975),2), ")"),
            `Free sugars(%)` = paste0(round(median(Libro_perc$NMES),2), "(", round(quantile(Libro_perc$NMES, 0.025),2), "-", round(quantile(Libro_perc$NMES, 0.975),2), ")"),
            `Trans Fatty-Acids(%)` = paste0(round(median(Libro_perc$TFA),2), "(", round(quantile(Libro_perc$TFA, 0.025),2), "-", round(quantile(Libro_perc$TFA, 0.975),2), ")"),
            `Alcohol(%)` = paste0(round(median(Libro_perc$Alcohol),2), "(", round(quantile(Libro_perc$Alcohol, 0.025),2), "-", round(quantile(Libro_perc$Alcohol, 0.975),2),")")) %>% unique()


#ADD ROW FOR NDNS VAlUES------------------
Perc_summary[5,] <-  c("1882(628)","16.5(4.2)","32.9(6.6)","45.5(7.7)","-","11.6(6.2)","0.7(0.3)","-")
Perc_summary[6,] <- c("1815(864-3176)", "16.0(10.3-25.6)", "33.4(18.9-44.9)", "45.5(30.0-60.5)", "-", "10.7(2.4-25.0)", "0.6(0.2-1.3)", "-")

#order------------------------------
Perc_summary <- Perc_summary[c(5,6,1:4),]
rownames(Perc_summary) <- c("NDNS\n Mean(sd)", "NDNS\n Median (lower-upper 2.5 percentile)","Intake24\n Mean(sd)", "Intake24\n Median (lower-upper 2.5 percentile)","Libro\n Mean(sd)", "Libro\n Median (lower-upper 2.5 percentile)") 
#SEE HOW TO ADD THE LINE BREAK AS \n it's not working

Perc_summary <- t(Perc_summary)
Perc_summary <- Perc_summary[-8,] #not interested in alcohol, but needed to calculate EI


#-----ENERGY-ADJUSTED NI: RESIDUAL MODEL-----------------------
#DV = NI(g) #mean of SRs
#IV = total EI #mean of SRs 
#adjust the NI(g) with residuals (fitted values i.e., expected NI - observed NI) --> add residuals to mean NI value
#--> if the residual is positive, it means a deficiency after accounting for EI (so you add the residual and adjust for such deficiency),
#if the residuals is negative, it means an excess after adjusting for EI so you subtract it to adjust for such excess 


#INTAKE24---------------------
#apply lm on columns
In24_lm_res_NI <- lapply(In24_means[3:8], function(x) lm(x ~ In24_means$Energy..kcal._mean))
#retrieve df with residuals
In24_res_NI <- sapply(In24_lm_res_NI, function(x) x[["residuals"]]) %>% as.data.frame()
#add to residuals a constant i.e., mean NI values
#1create an empty df  
In24_Nadj <- data.frame(matrix(nrow = 47, ncol = 6)) 
colnames(In24_Nadj) = c("Proteins", "Fats", "Carbohydrates", "Fibers", "Free.Sugars", "TFA")
#loop for the operation across columns of the two diff df
for (i in 1:length(In24_res_NI)) {
    In24_Nadj[i]<- In24_res_NI[i] + colMeans(In24_means[i+2])
  }
  
In24_Nadj[In24_Nadj < 0] <- 0

#LIBRO---------------------------
Libro_lm_res_NI <- lapply(Libro_means[3:8], function(x) lm(x ~ Libro_means$Energy.kcal._mean))
Libro_res_NI <- sapply(Libro_lm_res_NI, function(x) x[["residuals"]]) %>% as.data.frame()

Libro_Nadj <- data.frame(matrix(nrow = 47, ncol = 6)) 
colnames(Libro_Nadj) = c("Proteins", "Fats", "Carbohydrates", "Fibers", "Free.sugars", "TFA")

for (i in 1:length(Libro_res_NI)) {
  Libro_Nadj[i]<- Libro_res_NI[i] + colMeans(Libro_means[i+2])
}

#transform to 0 negative values (just a couple of TFA)
Libro_Nadj[Libro_Nadj < 0] <- 0

#check for normality-----------------------------------------------------------------------------------------------------------------------------------------------------------
qqPlot(Libro_Nadj$Protein.Intake) #1 out
qqPlot(Libro_Nadj$Fat.Intake) #no
qqPlot(Libro_Nadj$Carbohydrate.Intake) #1 out

qqPlot(In24_Nadj$Carbohydrate.Intake) 
qqPlot(In24_Nadj$Protein.Intake) 
qqPlot(In24_Nadj$Fat.Intake)


#-----MEAN AND DIFF---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#med NI-adj (Q1-Q3) INTAKE24
Table2 <- In24_Nadj %>%
  transmute(`Protein(g)` = paste0(round(median(In24_Nadj$Protein),2), "(", round(quantile(In24_Nadj$Protein, 0.25),2), "-", round(quantile(In24_Nadj$Protein, 0.75),2),")"),
            `Fat(g)` = paste0(round(median(In24_Nadj$Fat),2), "(", round(quantile(In24_Nadj$Fat, 0.25),2), "-", round(quantile(In24_Nadj$Fat, 0.75),2), ")"),
            `Carbohydrate(g)` = paste0(round(median(In24_Nadj$Carbohydrate),2), "(", round(quantile(In24_Nadj$Carbohydrate, 0.25),2), "-", round(quantile(In24_Nadj$Carbohydrate, 0.75),2),")"),
            `Fibers(g)` = paste0(round(median(In24_Nadj$Fibers),2), "(", round(quantile(In24_Nadj$Fibers, 0.25),2), "-", round(quantile(In24_Nadj$Fibers, 0.75),2), ")"),
            `Free sugars(g)` = paste0(round(median(In24_Nadj$`Free.Sugars`),2), "(", round(quantile(In24_Nadj$`Free.Sugars`, 0.25),2), "-", round(quantile(In24_Nadj$Free.sugars, 0.75),2), ")"),
            `Trans Fatty-Acids(g)` = paste0(round(median(In24_Nadj$TFA),2), "(", round(quantile(In24_Nadj$TFA, 0.25),2), "-", round(quantile(In24_Nadj$TFA, 0.75),2), ")")) %>% unique()
#med NI-adj (Q1-Q3) LIBRO
Table2[2,] <- Libro_Nadj %>%
  transmute(`Protein(g)` = paste0(round(median(Libro_Nadj$Protein),2), "(", round(quantile(Libro_Nadj$Protein, 0.25),2), "-", round(quantile(Libro_Nadj$Protein, 0.75),2),")"),
            `Fat(g)` = paste0(round(median(Libro_Nadj$Fat),2), "(", round(quantile(Libro_Nadj$Fat, 0.25),2), "-", round(quantile(Libro_Nadj$Fat, 0.75),2), ")"),
            `Carbohydrate(g)` = paste0(round(median(Libro_Nadj$Carbohydrate),2), "(", round(quantile(Libro_Nadj$Carbohydrate, 0.25),2), "-", round(quantile(Libro_Nadj$Carbohydrate, 0.75),2),")"),
            `Fibers(g)` = paste0(round(median(Libro_Nadj$Fibers),2), "(", round(quantile(Libro_Nadj$Fibers, 0.25),2), "-", round(quantile(Libro_Nadj$Fibers, 0.75),2), ")"),
            `Free sugars(g)` = paste0(round(median(Libro_Nadj$Free.sugars),2), "(", round(quantile(Libro_Nadj$Free.sugars, 0.25),2), "-", round(quantile(Libro_Nadj$Free.sugars, 0.75),2), ")"),
            `Trans Fatty-Acids(g)` = paste0(round(median(Libro_Nadj$TFA),2), "(", round(quantile(Libro_Nadj$TFA, 0.25),2), "-", round(quantile(Libro_Nadj$TFA, 0.75),2), ")")) %>% unique()



#-----TEST STATISTICS--------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#SPEARMAN CORR (r value)------------------------------------------

#create a cor.test function
cor.f <- function(x,y) {
  cor.r <- cor.test(x,y, method = "spearman", exact = FALSE)
  return(cor.r[["estimate"]])
}

#use mapply on multiple df with cor.f 
Table2[3,] <-round(mapply(cor.f, Libro_Nadj, In24_Nadj),2)

#SPEARMAN CORR (p value)-------------------------------------------
#create function
p.f <- function(x,y) {
  p.f <- cor.test(x,y, method = "spearman", exact = FALSE)
  return(p.f[["p.value"]])
}

#use mapply to multiple df with p.f
Table2[4,] <- round(mapply(p.f, Libro_Nadj, In24_Nadj), 2)


#replace p value with traditional 0.01 and 0.05
#for (i in 1:length(Table2[4,])) {
 # if  (Table2[4,i] > 0.05) {
  #  Table2[4,i]  <- "> 0.05"
  #} else if (Table2[4,i] <= 0.01) {
   # Table2[4,i] <- "< 0.01"
  #}
#}


#MEDIAN DIFFERENCE # BIAS -------------------------------------------
#create row with median diff Libro - In24 and CIs
#CIs for medians are calculated with the bootstrap method.
#Bootstrap is a statistical procedure that resamples a single dataset to create many simulated samples and allows you 
#to calculate standard errors, construct confidence intervals, and perform hypothesis testing for numerous types of sample statistics

library(boot)

#use apply function on a df = Libro - In24.
Table2[5,] <- apply((Libro_Nadj - In24_Nadj), 2, function(x) {
  
  #create a first function for median. We need this instead of median(x) as this is the more complex function designed to work with the boot function. median(x) would only work with vectors.
  #instead, median(data[indices]) takes 2 arguments : 1) data: the dataset, 2) indices : indices for iterative resampling. 
  
  med_function <- function(data, indices) {
    median(data[indices])
  }
  
  #the boot function perform a bootstrap resampling per each given index
  #The function doesn't inherently know whether it's working column-wise or on the entire dataset; 
  #The context of how it's applied (column-wise or on the entire dataset) is determined by how you call the boot function 
  #(usually within an apply loop for column-wise processing).
  #the outcome is a boot object with multiple components. One of them is T, the matrix of bootstrap stats. 
  #Each row corresponds to a bootstrap replicate, and each column corresponds to the statistic(s) calculated in each replicate. 
  #For example, if you perform 1000 bootstrap replicates and calculate a single statistic (like the median) in each replicate, 
  #T will be a matrix with 1000 rows and 1 column.
  
  bootstrap_result <- boot(x, statistic = med_function, R = 1000)
  
  #calculate the percentile bootstrap ci for each column
  #the boot.ci function calculates the CIs based on specified arguments
  
  ci_result <- boot.ci(bootstrap_result, type = "perc", conf = 0.95)
  ci <- ci_result$percent[4:5]
  
  #extract values of interest
  
  lower_ci <- round(ci[1], 2)
  upper_ci <- round(ci[2], 2)
  med <- round(quantile(x, 0.5), 2)
  
  #paste them 
  
  paste0(med, " (", lower_ci, ",", upper_ci, ")")
})



#WILCOXON SIGNED RANK TEST)(p value)-----------------------------

#create t test function
wilcox.f <- function(x,y, digits = 15) {
  wilcox.r <- wilcox.test(x,y,paired = T)
  return(round(wilcox.r[["p.value"]], digits = digits))
}

#use mapply on multiple df with t.f
Table2[6, ] <- mapply(wilcox.f, Libro_Nadj, In24_Nadj)


#replace p value with traditional 0.01 and 0.05
#for (i in 1:length(Table2[6,])) {
#  if (Table2[6,i] < 0.01) {
 #   Table2[6,i] <- "< 0.01" 
 # }    else if (Table2[6,i] > 0.01) {
  #  Table2[6,i] <- "< 0.05"    
  #    }
  #  }

#CROSS CLASSIFICATION (quartiles)---------------------------------------------------

#create df with quartile cat per each NI
In24_Nadj_Q <- sapply(In24_Nadj, function(x) ntile(x, 4))
Libro_Nadj_Q <- sapply(Libro_Nadj, function(x) ntile(x, 4)) 

# % SAME Q------

#create an empty vector for count of same Q
count1 <- rep(0,6)

#create a loop to iterate over the columns of the data frame and that compares corresponding elements of Libro_Nadj_Q and In24_Nadj_Q
for (i in 1:ncol(In24_Nadj_Q)) { #loop over columns
  for (j in 1:nrow(In24_Nadj_Q)) { #loop over rows
    if (Libro_Nadj_Q[j,i] == In24_Nadj_Q[j,i]) { #compare between df each element 
      count1[i] = count1[i] +1
    }
  }
}

#create a column with % values
Table2[7,] <- sapply(count1, function(x) x/nrow(In24_Nadj_Q)*100) %>% round(digits = 2)


# % OPPOSITE Q-------
count2 <- rep(0,6)

for (i in 1:ncol(In24_Nadj_Q)) {
  for (j in 1:nrow(In24_Nadj_Q)) {
    if ((Libro_Nadj_Q[j,i] == 1 & In24_Nadj_Q[j,i] == 4) | (Libro_Nadj_Q[j,i] == 4 & In24_Nadj_Q[j,i] == 1)) {
      count2[i] = count2[i] +1
    }
  }
}

Table2[8,] <- sapply(count2, function(x) x/nrow(In24_Nadj_Q)*100) %>% round(digits = 2)
                                                                            

#KAPPA STAT (value)------------------------------------------------------------------------

#create function for k
kappa.f <- function(x) {
  kappa.r <- kappa2(x, weight = "squared")
  return(kappa.r[["value"]])
}

#create empty row
Table2[9,] <- rep(0,6)

#apply loop to calculate kappa per each pair of columns 
for (i in 1:ncol(In24_Nadj_Q)) {
  Table2[9,i] <- kappa.f(as.matrix(cbind(In24_Nadj_Q[,i], Libro_Nadj_Q[,i]))) %>% round(digit = 2) #the function works with matrices 
}

#KAPPA STAT (p value)------------------------------------------------------------------------
#create function
pk.f <- function(x) {
  pk.f <- kappa2(x, weight = "squared")
  return(pk.f[["p.value"]])
}

#create row
Table2[10,] <- rep(0,6)

#use loop for to retrieve p values
for (i in 1:ncol(In24_Nadj_Q)) {
  Table2[10,i] <- pk.f(as.matrix(cbind(In24_Nadj_Q[,i], Libro_Nadj_Q[,i]))) %>% round(digit = 2) 
}

#replace p value with traditional 0.01 and 0.05
#for (i in 1:length(Table2[10,])) {
 # if  (Table2[10,i] > 0.05) {
   # Table2[10,i]  <- "> 0.05"
 # } else if (Table2[10,i] <= 0.05) {
  #  Table2[10,i] <- "< 0.05"
 # }
#}


#TIDY----------------------------------------------------------------------------------------------
rownames(Table2) <- c("Intake24\n median (IQR)", "Libro\n median (IQR)", "Spearman (r)", "Spearman (p value)", "Median difference (95% CI)",
                      "Wilcoxon signed rank (p value)", "Cross-classification\n % same quartiles", "Cross-classification\n % opposite quartiles", "Weighted Kappa stat (value)", "Weighted Kappa stat (p value)")
Table2 <- t(Table2)


#export

#creat empty doc
ss_docx1 <- read_docx()

#TABLE1 Perc_summary
#transform the table in df
Perc_summary_df <- as.data.frame(Perc_summary)
#add rownames and order
Perc_summary_df$Rowname <-  rownames(Perc_summary_df)
Perc_summary_df <- Perc_summary_df[, c("Rowname", setdiff(names(Perc_summary_df), "Rowname"))]
#create the table to export
ss_table1 <- flextable(Perc_summary_df) %>% 
  set_table_properties(layout = "autofit") %>%
  theme_zebra()
#export
ss_docx1<- ss_docx1 %>%
  body_add_flextable(value = ss_table1)
#PRINT
print(ss_docx1, target = "/Users/bm00900/OneDrive - University of Surrey/Desktop/Ph.D/AVANA study/Data/Data.R/ss_docx1.docx")


#TABLE 2
Table2_df <- as.data.frame(Table2)
Table2_df$Rownames <- rownames(Table2)
Table2_df <- Table2_df[, c("Rownames", setdiff(names(Table2_df), "Rownames"))]


ss_table2 <- flextable(Table2_df) %>% 
  set_table_properties(layout = "autofit") %>%
  theme_zebra()

ss_docx2 <- read_docx()
ss_docx2 <- ss_docx2 %>%
  body_add_flextable(value = ss_table2)


print(ss_docx2, target = "/Users/bm00900/OneDrive - University of Surrey/Desktop/Ph.D/AVANA study/Data/Data.R/ss_docx2.docx")


#----SCATTERPLOTS--------------------------------------------------------------------------------------------------------------
DF <- cbind(Libro_Nadj, In24_Nadj)

DF <- DF %>%
  rename_with(~ paste(.,"_In24"), .cols = 7:12)

#PROTEINS
corr_plot_PROT <- ggplot(DF, aes(Proteins,`Proteins _In24`)) + geom_point() + stat_cor(method = "spearman") + geom_smooth(method = "lm", se = F) + theme_classic() + labs( y = "Libro (g)", x = "Intake24 (g)", title = "Proteins")
corr_plot_PROT
#FATS
corr_plot_FATS <- ggplot(DF, aes(Fats,`Fats _In24`)) + geom_point() + stat_cor(method = "spearman") + geom_smooth(method = "lm", se = F) + theme_classic() + labs( y = "Libro (g)", x = "Intake24 (g)", title = "Fats")
corr_plot_FATS
#CARBS
corr_plot_CARBS <- ggplot(DF, aes(Carbohydrates,`Carbohydrates _In24`)) + geom_point() + stat_cor(method = "spearman") + geom_smooth(method = "lm", se = F) + theme_classic() + labs( y = "Libro (g)", x = "Intake24 (g)", title = "Carbohydrates")
corr_plot_CARBS
#FIBERS
corr_plot_FIBERS <- ggplot(DF, aes(Fibers,`Fibers _In24`)) + geom_point() + stat_cor(method = "spearman") + geom_smooth(method = "lm", se = F) + theme_classic() + labs( y = "Libro (g)", x = "Intake24 (g)", title = "Fibers")
corr_plot_FIBERS
#FREE SUGARS
corr_plot_SUG <- ggplot(DF, aes(Free.sugars,`Free.Sugars _In24`)) + geom_point() + stat_cor(method = "spearman") + geom_smooth(method = "lm", se = F) + theme_classic() + labs( y = "Libro (g)", x = "Intake24 (g)", title = "Free sugars")
corr_plot_SUG
#TFA
corr_plot_TFA <- ggplot(DF, aes(TFA,`TFA _In24`)) + geom_point() + stat_cor(method = "spearman") + geom_smooth(method = "lm", se = F) + theme_classic() + labs( y = "Libro (g)", x = "Intake24 (g)", title = "Trans-fatty acids")
corr_plot_TFA

COMB_PLOTS_NI <- ggarrange(corr_plot_PROT, corr_plot_FATS, corr_plot_CARBS, corr_plot_FIBERS, corr_plot_SUG, corr_plot_TFA, ncol = 3, nrow = 2, labels = c("A.", "B.", "C.", "D.", "E.", "F."))
COMB_PLOTS_NI
