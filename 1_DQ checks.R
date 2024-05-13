#QUALITY CHECKS AND DATA CLEANING

#recalls for less than 400kcal
#recalls for more than 4500kcal

library(ggplot2)
library(ggpubr)
library(pastecs)


#IMPORT DATA
In24 <- read.delim("Results_24HRs1.txt")
In24 <- In24[-(295:480),]
Libro <- read.delim("Results_Libro1.txt")

In24 <- In24[!(In24$ID %in% "P19"),]
Libro <- Libro[!(Libro$ID %in% "P19"),]

#--------------------------------------------------------------------------------------------------------------------------------------------
#BOXPLOTS HIGH/LOW ENERGY RECALLS - days-wise - 

#INTAKE24

#crate a subset with days only
In24_d <- In24[-(grep("average", In24$Day)), ]
In24_d <- In24_d[-(grep("median", In24_d$Day)), ]
In24_d$Day <- gsub(" ", "", In24_d$Day)

#boxplot
In24_dp <- ggplot(In24_d, aes(x = "Intake24", y = In24_d$Energy..kcal., fill = Day)) +
  geom_boxplot(width = 0.3, outlier.shape = 25, outlier.size = 1, outlier.fill = "black", show.legend = FALSE) +
  labs(x= NULL, y = "Kcal per day") +
  theme_classic()+
  coord_cartesian(ylim = c(0, 12500)) +
  theme( 
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

In24_dp 


#individualize outliers
#outlier = > Q3/1+ 1.5xIQR
#extreme outliers = >Q3+3xIQR
In24_ds <- summary(In24_d$Energy..kcal.)
Q3 <- In24_ds[5]
IQR <- IQR(In24_d$Energy..kcal., na.rm = T)
Q1 <- In24_ds[2]

out_in <- In24_d[In24_d$Energy..kcal. > (Q3 + 1.5*IQR), c(1,9)]
extr_out_in <- out[out$Energy..kcal.> Q3+3*IQR, ]

#individualize energy intake <400
low_in <- In24_d[In24_d$Energy..kcal. < 400, c(1,9)]

#----------------------------------------------------------------------------------------------------------------------------------------------

#LIBRO

#create a subset with days only
Libro_d <- Libro[-(grep("average", Libro$Day)), ]
Libro_d <- Libro_d[-(grep("median", Libro_d$Day)), ]

Libro_d$Day[Libro_d$Day == "day 1"] <- "Monday"
Libro_d$Day[Libro_d$Day == "day 2"] <- "Wednesday"
Libro_d$Day[Libro_d$Day == "day 3"] <- "Friday"
Libro_d$Day[Libro_d$Day == "day 4"] <- "Saturday"


#boxplot
Libro_dp <- ggplot(Libro_d, aes(x = "Libro", y = Libro_d$Energy.kcal., fill = Day)) +
  geom_boxplot(width = 0.3, outlier.shape = 25, outlier.size = 1, outlier.fill = "black") +
  labs(x= NULL, y = "Kcal per day") +
  theme_classic() +
  coord_cartesian(ylim = c(0, 12500)) +
  theme( 
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

Libro_dp

#arrange two plots in one figure
ggarrange(In24_dp, Libro_dp,ncol = 2, labels = c("A.", "B."))

#outliers and EI < 400
Libro_ds <- summary(Libro_d$Energy.kcal.)
Q3_L <- Libro_ds[5]
IQR_L <- IQR(Libro_d$Energy.kcal., na.rm = TRUE)
Q1_L <- Libro_ds[2]

out_L <-Libro_d[Libro_d$Energy.kcal. > (Q3_L + 1.5*IQR_L), c(1,3)]
extr_out_L <- out_L[out_L$Energy.kcal.> Q3_L+3*IQR_L, ]

low_L <- Libro_d[Libro_d$Energy.kcal. < 400, c(1,3)]

#----------------------------------------------------------------------------------------------------------------------------------------------
#% < 400 

#LIBRO - days 
t <- table(Libro_d$Energy.kcal. < 400) %>% as.vector()
low_L_p <- (t[2]/sum(t))*100

#INTAKE24 - days
t2 <- table(In24_d$Energy..kcal. < 400) %>% as.vector()
low_in_p <- (t2[2]/sum(t2))*100

#percentage of average SRs per each method that are < 400 
Libro_avg <- Libro[grep("average", Libro$Day), ]
In24_avg <- In24[grep("average", In24$Day), ]
#LIBRO - avg 
t3 <- table(Libro_avg$Energy.kcal. < 400) %>% as.vector()
low_Lavg_p <- (t3[2]/sum(t3))*100

#INTAKE24 - avg
t4 <- table(In24_avg$Energy..kcal. < 400) %>% as.vector()
low_inavg_p <- (t4[2]/sum(t4))*100


Under_in24 <- In24_avg$Energy..kcal. < 400
Under_libro <- Libro_avg$Energy.kcal. < 400

contingency_table <- matrix(c(
  sum(Under_in24 & Under_libro),        # Both
  sum(Under_in24 & !Under_libro),       # 24HRs only
  sum(!Under_in24 & Under_libro),       # Libro only
  sum(!Under_in24 & !Under_libro)),     # Neither
  nrow = 2, byrow = TRUE)

mcnemar_result <- mcnemar.test(contingency_table) 




