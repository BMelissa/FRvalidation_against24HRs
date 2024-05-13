
#IMPORT DATA
In24 <- read.delim("Results_24HRs1.txt")
In24 <- In24[-(295:480),]
Libro <- read.delim("Results_Libro1.txt")

In24 <- In24[!(In24$ID %in% "P19"),]
Libro <- Libro[!(Libro$ID %in% "P19"),]

#FILTER DATA
In24_D <- In24[grep("day", In24$Day), c("ID", "Day", "Energy..kcal.")]
In24_D <- In24_D[- (c(which((In24_D$Energy..kcal. > 5000)))),] 

Libro_D <- Libro[grep("day", Libro$Day), c("ID", "Day", "Energy.kcal.")]
Libro_D <- Libro_D[- (c(which((Libro_D$Energy.kcal. > 5000)))),]

#GROUP AND SUMMARISE SRs

In24_D <- In24_D %>%
  group_by(ID) %>%
  mutate(Kcal_mean = mean(Energy..kcal., na.rm = TRUE), Kcal_median = median(Energy..kcal., na.rm = TRUE))

Libro_D <- Libro_D %>%
  group_by(ID) %>%
  mutate(Kcal_mean = mean(Energy.kcal., na.rm = TRUE), Kcal_median = median(Energy.kcal., na.rm = TRUE))

Libro_D <- Libro_D[- (c(which(Libro_D$ID == "P50"))), ]

summary_df <- data.frame ("ID" = c(1:18, 20:33,35:49), "In24_mean" = unique(In24_D$Kcal_mean), 
                          "Libro_mean" = unique(Libro_D$Kcal_mean))

#CALCULATE STATS

summary(summary_df$Libro_mean)

summary(summary_df$In24_mean)
