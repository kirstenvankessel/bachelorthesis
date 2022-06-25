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
data_incomplete <- read_excel('data_evaluatie.xlsx')
View(data_incomplete)

#make dataset for ISW regression
data_ISW <- subset(data_incomplete, select = -c(Wgdocent, Wgdcoent, Uitlegduidelijk, Stimulerend,
                                                `Paper leerzaam`, Ondersteuning,
                                                Deskundig, Uitlegduidelijk, `Ondersteuning opdracht`, 
                                                Duidelijk))

# delete all people that have not filled in Moeilijkheid, as in the case of ISW, they will not have filled in any other variable
data_ISW <- data_ISW[!(is.na(data_ISW$Moeilijkheid)),] 
data_ISW <- data_ISW[data_ISW$Studie == "ISW", ] # select everyone from ISW
data_ISW$`Uren per week`[data_ISW$`Uren per week` == '<12'] <- '<13'
View(data_ISW)

#look at missing data
md.pattern(data_ISW, rotate.names = TRUE)

#make Wgdocent a categorical variable
data_ISW$`WG docent` <- as.character(data_ISW$`WG docent`)

#explore descriptive statistics
summary(data_ISW)

#make regression model ISW
model_ISW <- lm(data = data_ISW, Cijfer ~ Moeilijkheid + `Veel geleerd?` + Leuk + Organisatie +
                  `In staat voelen` + `Eigen keuzes` + `Nut inhoud` + Duidelijkverwachting + `Uren per week` +
                  `Binding medestudenten` + Voldaanverwachting + `WG docent` + `Steun leerstof` + `Steun opdracht` +
                  WGLeerzaam + Hoorcolleges)
summary(model_ISW)

#check assumptions
as_model <- gvlma(model_ISW, alphalevel = 0.05)
summary(as_model)
plot(as_model, onepage = FALSE)

#shapiro wilk test for normality of residuals
shapiro.test(model_ISW$residuals)
hist(model_ISW$residuals, xlab = "Residuals", main = "")
# not violated

#Goldfeld-Quandt test for homoscedasticity
bptest(model_ISW)
# not violated

#check outliers for SOC specific variables
ggplot(data_ISW, aes(x = "", y = `WG docent`)) + geom_boxplot(outlier.colour="red")
ggplot(data_ISW, aes(x = "", y = `Steun leerstof`)) + geom_boxplot(outlier.colour="red")
ggplot(data_ISW, aes(x = "", y = `Steun opdracht`)) + geom_boxplot(outlier.colour="red")
ggplot(data_ISW, aes(x = "", y = WGLeerzaam)) + geom_boxplot(outlier.colour="red")

data_ISW %>% 
  pivot_longer(cols = c(`Steun leerstof`, `Steun opdracht`, WGLeerzaam, Hoorcolleges),
               names_to='All_variables',
               values_to='All_Values') %>%
  ggplot(aes(x = All_variables, y = All_Values)) + 
  geom_boxplot(outlier.colour="red", width = 0.6) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.title.x = element_blank()) + ylab("value") + scale_fill_grey() +
  scale_x_discrete(labels= c("Perceived support lectures", "Learning support by teacher", 
                             "Support for paper by teacher", "Perceived learning in workgroup"))  
# violated

#check multicollinearity
vif(model_ISW)      
# violated for WG docent and Uren per week
