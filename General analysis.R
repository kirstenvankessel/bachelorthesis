library(mice)
library(ggplot2)
library(car)
library(readxl)
library(naniar)
library(dplyr)
library(psych)
library(gvlma)
library(lmtest)
library(GPArotation)
library(tidyr)

#load data
data_incomplete <- read_excel('data_evaluatie.xlsx')
View(data_incomplete)

#make dataset for general (not workgroup specific) regression
data_algemeen <- subset(data_incomplete, select = -c(Wgdocent, `WG docent`, Wgdcoent, Uitlegduidelijk, Stimulerend,
                                                     `Steun opdracht`, `Paper leerzaam`, `Steun leerstof`, Ondersteuning,
                                                     WGLeerzaam, Deskundig, Uitlegduidelijk, `Ondersteuning opdracht`, 
                                                     Duidelijk, Hoorcolleges))
data_algemeen <- data_algemeen[!(is.na(data$Studie) & is.na(data$Moeilijkheid)),]
View(data_algemeen)

#look at missing data
md.pattern(data_algemeen, rotate.names = TRUE)

#explore descriptive statistics
summary(data_algemeen)
describe(data_algemeen)

data_algemeen %>% group_by(data_algemeen$Studie) %>% summarize(count_study = n(), 
                                                      mean_evaluation_grade = mean(Cijfer, na.rm = TRUE),
                                                      sd_study = sd(Cijfer, na.rm = TRUE))
data_algemeen$`Uren per week`[data_algemeen$`Uren per week` == '<12'] <- '<13'

data_algemeen %>% group_by() %>% summarize(count = n())

mean(data_algemeen$Cijfer, na.rm = TRUE)
sd(data_algemeen$Cijfer, na.rm = TRUE)

#make regression model
model_algemeen <- lm(data = data_algemeen, Cijfer ~ Studie + Moeilijkheid + Leuk +`Veel geleerd?` + Organisatie +
                       `In staat voelen` + `Eigen keuzes` + `Nut inhoud` + Duidelijkverwachting + `Uren per week` +
                       `Binding medestudenten` + Voldaanverwachting)
summary(model_algemeen)

#check assumptions
as_model <- gvlma(model_algemeen, alphalevel = 0.05)
?gvlma
summary(as_model)
plot(as_model, onepage = FALSE)

#shapiro wilk test for normality of residuals
shapiro.test(model_algemeen$residuals)
hist(model_algemeen$residuals, xlab = "Residuals", main = "")
# not violated

#Goldfeld-Quandt test for homoscedasticity
gqtest(model_algemeen)
# not violated

#check outliers
data_algemeen %>% 
pivot_longer(cols = c(Moeilijkheid, `Veel geleerd?`, Leuk, 
                                        Organisatie, `In staat voelen`, `Eigen keuzes`,
                                        `Nut inhoud`, Duidelijkverwachting, 
                                        `Binding medestudenten`, Voldaanverwachting),
                    names_to='All_variables',
                    values_to='All_Values') %>%
ggplot(aes(x = All_variables, y = All_Values)) + 
  geom_boxplot(outlier.colour="red", width = 0.3) + 
  stat_boxplot(geom = "errorbar", width = 0.3) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), 
        axis.title.x = element_blank()) + ylab("value") +
  scale_x_discrete(labels= c("Relatedness", "Clarity of instructions", "Autonomy",
                             "Competence", "Intrinsic motivation", "Perceived course difficulty",
                             "Perceived usefulness", "Perceived course organisation",
                             "Perceived learning achievement", "Accuracy of course expectations")) 

ggplot(data_algemeen, aes(x = "", y = Studie)) + geom_boxplot(outlier.colour="red")
ggplot(data_algemeen, aes(x = "", y = `Uren per week`)) + geom_boxplot(outlier.colour="red")
# violated, only 1 outlier

#check multicollinearity
vif(model_algemeen)      
# not violated

# check normality for self-determination (S-D) variables
ggplot(data = data_incomplete, aes(x = `Eigen keuzes`)) + 
  geom_histogram(binwidth = 1, color="black", fill="gray") + 
  theme_classic() + xlab("Autonomy")
ggplot(data = data_incomplete, aes(x = `In staat voelen`)) +
  geom_histogram(binwidth = 1, color="black", fill="gray") + 
  theme_classic() + xlab("Competence")
ggplot(data = data_incomplete, aes(x = `Binding medestudenten`)) + 
  geom_histogram(binwidth = 1, color="black", fill="gray") + 
  theme_classic() + xlab("Relatedness")

shapiro.test(data_incomplete$`Eigen keuzes`)
shapiro.test(data_incomplete$`In staat voelen`)
shapiro.test(data_incomplete$`Binding medestudenten`)

# check linearity for S-D variables
ggplot(data_incomplete, aes(x = `Eigen keuzes`, y = Cijfer)) + 
  geom_point() + geom_smooth(method=lm) + xlab("Autonomy") + 
  ylab("Global evaluation") + theme_classic()
ggplot(data_incomplete, aes(x = `In staat voelen`, y = Cijfer)) + 
  geom_point() + geom_smooth(method=lm) + xlab("Competence") + 
  ylab("Global evaluation") + theme_classic()
ggplot(data_incomplete, aes(x = `Binding medestudenten`, y = Cijfer)) + 
  geom_point() + geom_smooth(method=lm) + xlab("Relatedness") + 
  ylab("Global evaluation") + theme_classic()

# factor analysis for S-D variables
self_deter_vars <- data.frame(data_incomplete$`Eigen keuzes`, 
                              data_incomplete$`In staat voelen`, 
                              data_incomplete$`Binding medestudenten`)
self_deter_vars <- na.omit(self_deter_vars)
View(self_deter_vars)

(matrix <- cor(self_deter_vars, use = "pairwise.complete.obs"))

fa.parallel(matrix)
(efa <- fa(self_deter_vars, nfactors = 1))
efa$loadings
qplot(y = efa$values) + geom_path() + 
  ylab("Eigenvalue") + theme_classic()
# efa suggests either 1 or 0 factors, to much missing data if we would add it to one variable

