average_controle <- mean(Data_ois_v3$Controle)
ggplot(Data_ois_v3, aes(x = Participant, y = Controle)) +
  geom_point() +
  geom_hline(yintercept = average_controle, linetype = "dashed", color = "red") +
  labs(title = "Scatterplot of Controle with Average Line",
       x = "Participant",
       y = "Controle") +
  theme(axis.text.x = element_blank())


average_30db <- mean(Data_ois_v3$`30 dB`)
ggplot(Data_ois_v3, aes(x = Participant, y = Data_ois_v3$`30 dB`)) +
  geom_point() +
  geom_hline(yintercept = average_30db, linetype = "dashed", color = "red") +
  labs(title = "Scatterplot of 40 dB with Average Line",
       x = "Participant",
       y = "40 dB") +
  theme(axis.text.x = element_blank())


average_60db <- mean(Data_ois_v3$`60 dB`)
ggplot(Data_ois_v3, aes(x = Participant, y = Data_ois_v3$`60 dB`)) +
  geom_point() +
  geom_hline(yintercept = average_60db, linetype = "dashed", color = "red") +
  labs(title = "Scatterplot of 60 dB with Average Line",
       x = "Participant",
       y = "60 dB") +
  theme(axis.text.x = element_blank())


average_80db <- mean(Data_ois_v3$`80dB`)
ggplot(Data_ois_v3, aes(x = Participant, y = `80dB`)) +
  geom_point() +
  geom_hline(yintercept = average_80db, linetype = "dashed", color = "red") +
  labs(title = "Scatterplot of 80 dB with Average Line",
       x = "Participant",
       y = "80 dB") +
  theme(axis.text.x = element_blank())


ggplot(Data_ois_v3_corrected, aes(x = OS)) +
  geom_bar() +
  labs(title = "Distribution of Operating Systems",
       x = "OS",
       y = "Count")

ggplot(Data_ois_v3, aes(x = Geslacht)) +
  geom_bar() +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count")

cor_data <- cor(Data_ois_v3[, c("Controle", "Gamen (uur)", "Leeftijd")])
melted_cor_data <- melt(cor_data)
ggplot(melted_cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Correlation Heatmap",
       x = "",
       y = "") +
  theme_minimal()

cor_data2 <- cor(Data_ois_v3[, c("30 dB", "60 dB", "80dB")])
melted_cor_data2 <- melt(cor_data2)
ggplot(melted_cor_data2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Correlation Heatmap",
       x = "",
       y = "") +
  theme_minimal()

long_data_audio <- reshape2::melt(Data_ois_v3, id.vars=c("Participant", "Geslacht", "OS", "volgorde", "melding", "Gamen (uur)", "Leeftijd"))

aov_result_audio <- ezANOVA(data=long_data_audio,
                            dv=.(value),
                            wid=.(Participant),
                            within=.(dB_level),
                            between=.(NULL))

anova_result <- aov(cbind(`30 dB`, `60 dB`, `80dB`) ~ 1, data = Data_ois_v3)
print(anova_result)
summary(anova_result)

result_30dB <- aov(`30 dB` ~ 1, data = Data_ois_v3)
summary(result_30dB)

anova_result_new <- aov(Controle ~ `30 dB` + `60 dB` + `80dB`, data = Data_ois_v3)
summary(anova_result_new)

summary_result <- summary(anova_result_new2)

anova_table <- data.frame(
  Term = rownames(summary_result[[1]]),
  Df = summary_result[[1]][, "Df"],
  Sum_Sq = summary_result[[1]][, "Sum Sq"],
  Mean_Sq = summary_result[[1]][, "Mean Sq"],
  F_Value = summary_result[[1]][, "F value"],
  Pr = summary_result[[1]][, "Pr(>F)"]
)

print(anova_table)

psych::describe(Data_ois_v3$`40 dB`)
psych::describe(Data_ois_v3$`60 dB`)
psych::describe(Data_ois_v3$`80dB`)
psych::describe(Data_ois_v3$Controle)

t_test_30db_control <- t.test(Data_ois_v3$`40 dB`, Data_ois_v3$Controle, paired = FALSE)
t_test_60db_control <- t.test(Data_ois_v3$`60 dB`, Data_ois_v3$Controle, paired = FALSE)
t_test_80db_control <- t.test(Data_ois_v3$`80dB`, Data_ois_v3$Controle, paired = FALSE)

p_value_30db_control <- t_test_40db_control$p.value
p_value_60db_control <- t_test_60db_control$p.value
p_value_80db_control <- t_test_80db_control$p.value

print(paste("P-value for 40 dB vs Control:", p_value_40db_control))
print(paste("P-value for 60 dB vs Control:", p_value_60db_control))
print(paste("P-value for 80 dB vs Control:", p_value_80db_control))

selected_items <- Data_ois_v3_corrected[, c("Controle", "40 dB", "60 dB", "80dB")]
alpha_result <- alpha(selected_items)
print(alpha_result)

> correlations_gender_control <- ltm::biserial.cor(data$Controle.ms., data$Geslacht)
> correlations_gender_control #corr tussen gender en controle (zonder geluid)
[1] -0.3738199
> correlations_numerical <- cor(data[, c("Controle.ms.", "X40.dB..ms.", "X60.dB..ms.", "X80dB..ms.", "melding", "Gamen..uur.")])
> correlations_numerical #correlations between numerical values
             Controle.ms. X30.dB..ms. X60.dB..ms. X80dB..ms.     melding Gamen..uur.
Controle.ms.    1.0000000  0.78673703  0.85284167  0.8532058 -0.13047899  -0.2285758
X30.dB..ms.     0.7867370  1.00000000  0.77474836  0.8743129 -0.05981656  -0.2192411
X60.dB..ms.     0.8528417  0.77474836  1.00000000  0.8519929 -0.08906037  -0.1216206
X80dB..ms.      0.8532058  0.87431286  0.85199285  1.0000000 -0.17048426  -0.3661810
melding        -0.1304790 -0.05981656 -0.08906037 -0.1704843  1.00000000  -0.1254168
Gamen..uur.    -0.2285758 -0.21924113 -0.12162061 -0.3661810 -0.12541680   1.0000000
> t_gender_control <- t.test(Controle.ms. ~ Geslacht, data = data)
> t_gender_control #T-test voor reactietijdcontrole tussen geslachten

	Welch Two Sample t-test

data:  Controle.ms. by Geslacht
t = -2.3305, df = 20.87, p-value = 0.0299
alternative hypothesis: true difference in means between group Male and group Female is not equal to 0
95 percent confidence interval:
 -70.029631  -3.970369
sample estimates:
  mean in group Male mean in group Female 
            359.5714             396.5714 

> anova_control_db <- anova(lm(Controle.ms. ~ X40.dB..ms. + X60.dB..ms. + X80dB..ms., data = data)) 
> anova_control_db #Dit test of er een significant verschil is in reactietijden tussen verschillende decibelniveaus.
Analysis of Variance Table

Response: Controle.ms.
            Df Sum Sq Mean Sq  F value    Pr(>F)    
X30.dB..ms.  1  56595   56595 111.4463 7.412e-13 ***
X60.dB..ms.  1  13541   13541  26.6656 7.949e-06 ***
X80dB..ms.   1   2003    2003   3.9436    0.0543 .  
Residuals   38  19297     508                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> model_regression <- lm(Controle.ms. ~ X40.dB..ms. + X60.dB..ms. + X80dB..ms. + melding + Gamen..uur., data = data)
> summary(model_regression) #Dit voert een lineaire regressie uit om te onderzoeken welke factoren de reactietijd beïnvloeden.

Call:
lm(formula = Controle.ms. ~ X40.dB..ms. + X60.dB..ms. + X80dB..ms. + 
    melding + Gamen..uur., data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-55.643 -11.599   2.928  13.790  40.033 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)   
(Intercept) 46.678647  34.525976   1.352  0.18481   
X30.dB..ms.  0.137158   0.166295   0.825  0.41492   
X60.dB..ms.  0.444719   0.153125   2.904  0.00625 **
X80dB..ms.   0.309279   0.233677   1.324  0.19400   
melding     -0.003500   0.009709  -0.361  0.72056   
Gamen..uur. -0.246822   0.809337  -0.305  0.76215   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 23.1 on 36 degrees of freedom
Multiple R-squared:  0.7899,	Adjusted R-squared:  0.7608 
F-statistic: 27.08 on 5 and 36 DF,  p-value: 2.886e-11

> chi_square_gender_os <- chisq.test(table(data$Geslacht, data$OS))
> chi_square_gender_os #Dit test of er een significant verband is tussen geslacht en het besturingssysteem.

	Pearson's Chi-squared test with Yates' continuity correction

data:  table(data$Geslacht, data$OS)
X-squared = 1.2045, df = 1, p-value = 0.2724

> correlation_melding_reaction <- cor(data$Controle.ms., data$melding)
> correlation_melding_reaction #correlatie geluid en meldingen pearson
[1] -0.130479
> # MANOVA voor reactietijden en geslacht met meerdere variabelen
> manova_results <- manova(cbind(Controle.ms., X40.dB..ms., X60.dB..ms., X80dB..ms.) ~ Geslacht, data = data)
> summary(manova_results) #Dit voert een multivariate variantieanalyse uit om te onderzoeken of er verschillen zijn in reactietijden tussen geslachten voor alle decibelniveaus.
          Df Pillai approx F num Df den Df Pr(>F)
Geslacht   1 0.1675   1.8612      4     37  0.138
Residuals 40                                     
> # Multivariate regressie voor reactietijden met meerdere voorspellers
> multi_regression <- lm(cbind(Controle.ms., X40.dB..ms., X60.dB..ms., X80dB..ms.) ~ melding + Gamen..uur., data = data)
> summary(multi_regression) #Dit voert een multivariate regressie uit om te onderzoeken welke factoren de reactietijden beïnvloeden voor verschillende decibelniveaus.
Response Controle.ms. :

Call:
lm(formula = Controle.ms. ~ melding + Gamen..uur., data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-70.864 -40.414  -6.425  28.969 132.444 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 394.66024   15.20572  25.955   <2e-16 ***
melding      -0.01880    0.01802  -1.043    0.303    
Gamen..uur.  -2.10240    1.30934  -1.606    0.116    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 46.49 on 39 degrees of freedom
Multiple R-squared:  0.07798,	Adjusted R-squared:  0.0307 
F-statistic: 1.649 on 2 and 39 DF,  p-value: 0.2053


Response X40.dB..ms. :

Call:
lm(formula = X30.dB..ms. ~ melding + Gamen..uur., data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-96.663 -30.790  -4.602  30.244 122.002 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 384.44955   15.40659  24.954   <2e-16 ***
melding      -0.01033    0.01826  -0.566    0.575    
Gamen..uur.  -1.94864    1.32664  -1.469    0.150    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 47.11 on 39 degrees of freedom
Multiple R-squared:  0.05581,	Adjusted R-squared:  0.007392 
F-statistic: 1.153 on 2 and 39 DF,  p-value: 0.3263


Response X60.dB..ms. :

Call:
lm(formula = X60.dB..ms. ~ melding + Gamen..uur., data = data)

Residuals:
     Min       1Q   Median       3Q      Max 
-108.400  -28.436   -5.192   23.147  119.815 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 381.80922   16.54902  23.071   <2e-16 ***
melding      -0.01305    0.01962  -0.665    0.510    
Gamen..uur.  -1.20683    1.42501  -0.847    0.402    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 50.6 on 39 degrees of freedom
Multiple R-squared:  0.02585,	Adjusted R-squared:  -0.02411 
F-statistic: 0.5174 on 2 and 39 DF,  p-value: 0.6001


Response X80dB..ms. :

Call:
lm(formula = X80dB..ms. ~ melding + Gamen..uur., data = data)

Residuals:
     Min       1Q   Median       3Q      Max 
-101.809  -31.806   -6.741   25.134  118.633 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 405.63204   14.64224  27.703   <2e-16 ***
melding      -0.02614    0.01736  -1.506   0.1402    
Gamen..uur.  -3.40021    1.26082  -2.697   0.0103 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 44.77 on 39 degrees of freedom
Multiple R-squared:  0.1817,	Adjusted R-squared:  0.1397 
F-statistic: 4.329 on 2 and 39 DF,  p-value: 0.02005
