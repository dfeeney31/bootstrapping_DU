###### Make the generic BPM calculations for agility #######
library(ez)
rm(list=ls())

dat <- read.csv("C:/Users/Daniel.Feeney/Dropbox (Boa)/2018 BOA Nonlinear Segment Performance/Analysis/AllSubResults.csv")
names(dat)[1]<-"Subject"
skate_total <- subset(dat, dat$Movement == 'Skater')
cmj_total <- subset(dat, dat$Movement == 'CMJ')


#######################

config_avg <- aggregate(skate_total$Time.on.FP ~ skate_total$Subject + skate_total$Condition + skate_total$Movement, data = skate_total, FUN = mean)
names(config_avg) <- c('Subject', 'Config', 'Movement','TimeonFP')
config_avg


T_differences <- numeric(20)
M_differences <- numeric(20)
L_differences <- numeric(20)
counter <- 1
for (subNo in unique(config_avg$Subject)) {
  tmp_var <- subset(config_avg, config_avg$Subject == subNo)
  T_differences[counter] <- subset(tmp_var$TimeonFP, tmp_var$Subject == subNo & tmp_var$Config == 'T') - subset(tmp_var$TimeonFP, tmp_var$Subject == subNo & tmp_var$Config == 'L')
  M_differences[counter] <- subset(tmp_var$TimeonFP, tmp_var$Subject == subNo & tmp_var$Config == 'M') - subset(tmp_var$TimeonFP, tmp_var$Subject == subNo & tmp_var$Config == 'L')
  L_differences[counter] <- subset(tmp_var$TimeonFP, tmp_var$Subject == subNo & tmp_var$Config == 'S') - subset(tmp_var$TimeonFP, tmp_var$Subject == subNo & tmp_var$Config == 'L')
  
  counter <- counter +1
}

diferences_mat <- as.data.frame(cbind(T_differences, M_differences, L_differences))
avg_diff <- apply(diferences_mat, 2 ,FUN = mean)

results <- as.data.frame(cbind(as.vector(c('T','M','S')), as.vector(cbind(avg_diff[1], avg_diff[2], avg_diff[3]))))
names(results) <- c('Config', 'DiffToLR')                                        

p<-ggplot(data=results, aes(x=results$Config, y=results$DiffToLR)) +
  geom_bar(stat="identity")
p
########################




# Time on FP
config_avg <- aggregate(skate_total$Time.on.FP ~ skate_total$Subject + skate_total$Condition + skate_total$Movement, data = skate_total, FUN = mean)
names(config_avg) <- c('Subject', 'Config', 'Movement','TimeonFP')
config_avg

tapply(config_avg$TimeonFP, config_avg$Config, FUN = mean)

rt_anova = ezANOVA(
  data = config_avg
  , dv = .(TimeonFP)
  , wid = .(Subject),
  , within = .(Config)
)
rt_anova$ANOVA

# GRF avg
grf_avg <- aggregate(skate_total$COM.Work.Eccentric ~ skate_total$Subject + skate_total$Condition + skate_total$Movement, data = skate_total, FUN = mean)
names(grf_avg) <- c('Subject', 'Config', 'Movement','Eccwk')
grf_avg

tapply(grf_avg$Eccwk, grf_avg$Config, FUN = mean)


rt_anova = ezANOVA(
  data = grf_avg
  , dv = .(Eccwk)
  , wid = .(Subject),
  , within = .(Config)
)
rt_anova$ANOVA



#################################
skater <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/2018 BOA Nonlinear Segment Performance/Analysis/DU_20subject_summary_skater.csv')
cmj <- read.csv('C:/Users/Daniel.Feeney/Dropbox (Boa)/2018 BOA Nonlinear Segment Performance/Analysis/DU_20subject_summary_CMJ.csv')

### Time on FP ###
length(subset(cmj$TimeonFP, cmj$TimeonFP >= 1 & cmj$Config == 'M'))
length(subset(cmj$TimeonFP, cmj$TimeonFP >= 1 & cmj$Config == 'T')) 
length(subset(cmj$TimeonFP, cmj$TimeonFP >= 1 & cmj$Config == 'L')) 

length(subset(skater$TimeonFP, skater$TimeonFP >= 1 & cmj$Config == 'M')) 
length(subset(skater$TimeonFP, skater$TimeonFP >= 1 & cmj$Config == 'T')) 
length(subset(skater$TimeonFP, skater$TimeonFP >= 1 & cmj$Config == 'L')) 

# RFD eccentric
length(subset(cmj$GRF_RFD_Ecc, cmj$GRF_RFD_Ecc >= 1 & cmj$Config == 'M')) 
length(subset(cmj$GRF_RFD_Ecc, cmj$GRF_RFD_Ecc >= 1 & cmj$Config == 'T')) 
length(subset(cmj$GRF_RFD_Ecc, cmj$GRF_RFD_Ecc >= 1 & cmj$Config == 'L')) 

length(subset(skater$GRF_RFD_Ecc, skater$GRF_RFD_Ecc >= 1 & cmj$Config == 'M')) 
length(subset(skater$GRF_RFD_Ecc, skater$GRF_RFD_Ecc >= 1 & cmj$Config == 'T')) 
length(subset(skater$GRF_RFD_Ecc, skater$GRF_RFD_Ecc >= 1 & cmj$Config == 'L')) 

# Peak GRF
length(subset(cmj$Peak_GRF, cmj$Peak_GRF >= 1 & cmj$Config == 'M')) 
length(subset(cmj$Peak_GRF, cmj$Peak_GRF >= 1 & cmj$Config == 'T')) 
length(subset(cmj$Peak_GRF, cmj$Peak_GRF >= 1 & cmj$Config == 'L')) 

length(subset(skater$Peak_GRF, skater$Peak_GRF >= 1 & cmj$Config == 'M')) 
length(subset(skater$Peak_GRF, skater$Peak_GRF >= 1 & cmj$Config == 'T')) 
length(subset(skater$Peak_GRF, skater$Peak_GRF > 1 & cmj$Config == 'L')) 

# Peak power COM
length(subset(cmj$Peak_pwr_com, cmj$Peak_pwr_com >= 1 & cmj$Config == 'M')) 
length(subset(cmj$Peak_pwr_com, cmj$Peak_pwr_com >= 1 & cmj$Config == 'T')) 
length(subset(cmj$Peak_pwr_com, cmj$Peak_pwr_com >= 1 & cmj$Config == 'L')) 

length(subset(skater$Peak_pwr_com, skater$Peak_pwr_com >= 1 & cmj$Config == 'M')) 
length(subset(skater$Peak_pwr_com, skater$Peak_pwr_com >= 1 & cmj$Config == 'T')) 
length(subset(skater$Peak_pwr_com, skater$Peak_pwr_com >= 1 & cmj$Config == 'L')) 

# eccentric work
length(subset(cmj$Work_ecc, cmj$Work_ecc >= 1 & cmj$Config == 'M')) 
length(subset(cmj$Work_ecc, cmj$Work_ecc >= 1 & cmj$Config == 'T')) 
length(subset(cmj$Work_ecc, cmj$Work_ecc >= 1 & cmj$Config == 'L')) 

length(subset(skater$Work_ecc, skater$Work_ecc >= 1 & cmj$Config == 'M')) 
length(subset(skater$Work_ecc, skater$Work_ecc >= 1 & cmj$Config == 'T')) 
length(subset(skater$Work_ecc, skater$Work_ecc >= 1 & cmj$Config == 'L')) 

# concentric work
length(subset(cmj$Work_prop, cmj$Work_prop >= 1 & cmj$Config == 'M')) 
length(subset(cmj$Work_prop, cmj$Work_prop >= 1 & cmj$Config == 'T')) 
length(subset(cmj$Work_prop, cmj$Work_prop >= 1 & cmj$Config == 'L')) 

length(subset(skater$Work_prop, skater$Work_prop >= 1 & cmj$Config == 'M')) 
length(subset(skater$Work_prop, skater$Work_prop >= 1 & cmj$Config == 'T')) 
length(subset(skater$Work_prop, skater$Work_prop >= 1 & cmj$Config == 'L')) 

# Total score
length(subset(cmj$Total_Score, cmj$Total_Score >= 1 & cmj$Config == 'M')) 
length(subset(cmj$Total_Score, cmj$Total_Score >= 1 & cmj$Config == 'T')) 
length(subset(cmj$Total_Score, cmj$Total_Score >= 1 & cmj$Config == 'L')) 

length(subset(skater$Total_Score, skater$Total_Score >= 1 & cmj$Config == 'M')) 
length(subset(skater$Total_Score, skater$Total_Score >= 1 & cmj$Config == 'T')) 
length(subset(skater$Total_Score, skater$Total_Score >= 1 & cmj$Config == 'L')) 


#### Plots for birdbrain ### 

s5 <- read.csv("C:/Users/Daniel.Feeney/Dropbox (Boa)/2018 BOA Nonlinear Segment Performance/Analysis/AllSubResults.csv")
names(s5)[1]<-"Subject"

confidenceDifferences_m <- function(df, metric, num_boots) {
  C1_ap <- subset(df[,metric], df$Condition == 'M' & df$Movement == 'CMJ')
  LC_ap <- subset(df[,metric], df$Condition == 'S' & df$Movement == 'CMJ')
  
  mean_differences <- vector('numeric', 10000)
  
  for (i in 1:10000) {
    C1_tmp <- sample(C1_ap, length(C1_ap), replace=TRUE)
    LC_tmp <- sample(LC_ap, length(C1_ap), replace=TRUE)
    mean_differences[i] <- mean(C1_tmp - LC_tmp)
  }
  
  foo <- quantile(mean_differences,probs=c(0.125,0.875))
  bar <- quantile(mean_differences,probs=c(0.05, 0.95))
  
  a <- ggplot(data.frame(mean_differences), aes(x=mean_differences)) +
    geom_histogram(bins = 100) + xlab('Time on FP') + ylab('Count') + theme_classic()
    
  b <- mean(mean_differences <= 0) * 100
  
  newList <- list("low" = foo, "high" = bar, "b" = b, a)
  return(newList)
}

tmp_sub <- subset(s5, s5$Subject == 16)
mVals <- confidenceDifferences_m(tmp_sub, 6, 10000)
mVals
