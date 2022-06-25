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

#make dataset for sociology regression
data_SOC <- subset(data_incomplete, select = -c(`WG docent`, Wgdcoent, Uitlegduidelijk,
                                                     `Steun opdracht`, `Steun leerstof`,
                                                     WGLeerzaam, Deskundig, Uitlegduidelijk, `Ondersteuning opdracht`, 
                                                     Hoorcolleges))

# delete all people that have not filled in Moeilijkheid, as in the case of SOC, they will not have filled in any other variable
data_SOC <- data_SOC[!(is.na(data_SOC$Moeilijkheid)),] 
data_SOC <- data_SOC[data_SOC$Studie == "SOC", ] # select everyone from sociology

View(data_SOC)

#look at missing data
md.pattern(data_SOC, rotate.names = TRUE)

#make Wgdocent a categorical variable
data_SOC$Wgdocent <- as.character(data_SOC$Wgdocent)

#explore descriptive statistics
summary(data_SOC)

#make regression model SOC
model_SOC <- lm(data = data_SOC, Cijfer ~ Moeilijkheid + `Veel geleerd?` + Leuk + Organisatie +
                       `In staat voelen` + `Eigen keuzes` + `Nut inhoud` + Duidelijkverwachting + `Uren per week` +
                       `Binding medestudenten` + Voldaanverwachting + Wgdocent + Duidelijk + Stimulerend + 
                        Ondersteuning + `Paper leerzaam`)
summary(model_SOC)

#check assumptions
as_model <- gvlma(model_SOC, alphalevel = 0.05)
summary(as_model)
plot(as_model, onepage = FALSE)

#shapiro wilk test for normality of residuals
shapiro.test(model_SOC$residuals)
hist(model_SOC$residuals, xlab = "Residuals", main = "")
# not violated

#Goldfeld-Quandt test for homoscedasticity
bptest(model_SOC)
# not violated

#check outliers per study
data_algemeen_drie  <- data_algemeen[!data_algemeen$Studie == "LAS",]
data_algemeen_drie <- data_algemeen_drie[!(is.na(data_algemeen$Studie)),]
data_algemeen_drie %>% 
  pivot_longer(cols = c(Moeilijkheid, `Veel geleerd?`, Leuk, 
                        Organisatie, `In staat voelen`, `Eigen keuzes`,
                        `Nut inhoud`, Duidelijkverwachting, 
                        `Binding medestudenten`, Voldaanverwachting),
               names_to='All_variables',
               values_to='All_Values') %>%
  ggplot(aes(x = All_variables, y = All_Values, fill = Studie)) + 
  geom_boxplot(outlier.colour="red", width = 0.6) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.title.x = element_blank()) + ylab("value") + scale_fill_grey() +
  scale_x_discrete(labels= c("Relatedness", "Clarity of instructions", "Autonomy",
                             "Competence", "Intrinsic motivation", "Perceived course difficulty",
                             "Perceived usefulness", "Perceived course organisation",
                             "Perceived learning achievement", "Accuracy of course expectations")) 

#check outliers for SOC specific variables
data_SOC %>% 
  pivot_longer(cols = c(Duidelijk, Stimulerend, Ondersteuning, `Paper leerzaam`),
               names_to='All_variables',
               values_to='All_Values') %>%
  ggplot(aes(x = All_variables, y = All_Values)) + 
  geom_boxplot(outlier.colour="red", width = 0.6) + 
  stat_boxplot(geom = "errorbar", width = 0.6) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.title.x = element_blank()) + ylab("value") + scale_fill_grey() +
  scale_x_discrete(labels= c("Clear explanation by teacher", 
                             "Support for paper by teacher", 
                             "Instructiveness research paper",
                             "Stimulation of active participation by teacher")) 

# violated

#check multicollinearity
vif(model_SOC)      
# violated


