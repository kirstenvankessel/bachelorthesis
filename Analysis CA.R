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

#make dataset for ca regression
data_CA <- subset(data_incomplete, select = -c(Wgdocent, `WG docent`, Stimulerend,
                                               `Steun opdracht`, `Paper leerzaam`, `Steun leerstof`, Ondersteuning,
                                               WGLeerzaam, Duidelijk, Hoorcolleges))

# delete all people that have not filled in Studie, as in the case of CA, they will not have filled in any other variable
data_CA <- data_CA[!(is.na(data_CA$Studie)),] 
data_CA <- data_CA[data_CA$Studie == "CA", ] # select everyone from ca
View(data_CA)

#look at missing data
md.pattern(data_CA, rotate.names = TRUE)

#make Wgdocent a categorical variable
data_CA$Wgdcoent <- as.character(data_CA$Wgdcoent)

#explore descriptive statistics
summary(data_CA)

#make regression model ca
model_CA <- lm(data = data_CA, Cijfer ~ Moeilijkheid + `Veel geleerd?` + Leuk + Organisatie +
                  `In staat voelen` + `Eigen keuzes` + `Nut inhoud` + Duidelijkverwachting + `Uren per week` +
                  `Binding medestudenten` + Voldaanverwachting + Wgdcoent + Deskundig + Uitlegduidelijk + `Ondersteuning opdracht`)
summary(model_CA)

#check assumptions
as_model <- gvlma(model_CA, alphalevel = 0.05)
summary(as_model)
plot(as_model, onepage = FALSE)

#shapiro wilk test for normality of residuals
shapiro.test(model_CA$residuals)
hist(model_CA$residuals, xlab = "Residuals", main = "")
# not violated

#Goldfeld-Quandt test for homoscedasticity
bptest(model_CA)
# not violated

#check outliers for ca specific variables
data_CA %>% 
  pivot_longer(cols = c(Deskundig, Uitlegduidelijk, `Ondersteuning opdracht`),
               names_to='All_variables',
               values_to='All_Values') %>%
  ggplot(aes(x = All_variables, y = All_Values)) + 
  geom_boxplot(outlier.colour="red", width = 0.6) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.title.x = element_blank()) + ylab("value") + scale_fill_grey() +
  scale_x_discrete(labels= c("Expertise teacher", "Support for paper by teacher", 
                             "Clear explanation by teacher")) 
# violated

#check multicollinearity
vif(model_CA)      
# violated