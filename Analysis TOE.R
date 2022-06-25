library(mice)
library(ggplot2)
library(car)
library(readxl)
library(naniar)
library(dplyr)
library(psych)
library(gvlma)
library(lmtest)

#load data
data_TOE <- read_excel('data_TOE.xlsx')
View(data_TOE)
summary(data_TOE)

#delete empty comlumns
data_TOE <- subset(data_TOE, select = -c(...4,...6,...8,...10,...12))

#look at missing data
md.pattern(data_TOE, rotate.names = TRUE)

#look at descriptive statistics
summary(data_TOE)
mean(data_TOE$Tentamen, na.rm = TRUE)
SD(data_TOE$Tentamen, na.rm = TRUE)

data_TOE %>% group_by(data_TOE$Opleiding) %>% summarize(count_study = n(),
                                                        mean_evaluation_grade = mean(Tentamen, na.rm = TRUE))

#regression model
cor(data_TOE$Grasple28feb, data_TOE$Grasple21mrt, use = "pairwise.complete.obs")
cor(data_TOE$Grasple28feb, data_TOE$Grasple8apr, use = "pairwise.complete.obs")
cor(data_TOE$Grasple8apr, data_TOE$Grasple21mrt, use = "pairwise.complete.obs")
cor(data_TOE$KOM, data_TOE$Tentamen, use = "pairwise.complete.obs")

data_TOE %>% 
  pivot_longer(cols = c(Grasple28feb, Grasple21mrt, Grasple8apr, KOM),
               names_to='All_variables',
               values_to='All_Values') %>%
  ggplot(aes(x = All_variables, y = All_Values)) + 
  geom_boxplot(outlier.colour="red", width = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.title.x = element_blank()) + ylab("value")

toe_model_gras <- lm(data = data_TOE, Tentamen ~ Grasple28feb + Grasple21mrt + Grasple8apr)
summary(toe_model_gras)

toe_model_gras_kom <- lm(data = data_TOE, Tentamen ~ KOM + Grasple28feb + Grasple21mrt + Grasple8apr)
summary(toe_model_gras_kom)

#shapiro wilk test for normality of residuals
shapiro.test(toe_model_gras$residuals)
shapiro.test(toe_model_gras_kom$residuals)
hist(toe_model_gras$residuals, xlab = "Residuals", main = "")
hist(toe_model_gras_kom$residuals, xlab = "Residuals", main = "")
# not violated

#Goldfeld-Quandt test for homoscedasticity
bptest(toe_model_gras)
bptest(toe_model_gras_kom)
# not violated

#check multicollinearity
vif(toe_model_gras)      
vif(toe_model_gras_kom)
# violated