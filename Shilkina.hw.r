library(ggplot2)
library(lme4)
library(dplyr)

data <- read.csv('C:/Users/Александра/Desktop/R.hw/duryagin_ReductionRussian.txt', sep = '\t')

# 1.1

ggplot(data, aes(f2, f1))+
  geom_point(aes(col = vowel))+
  scale_y_reverse()+
  scale_x_reverse()

# 1.2

ggplot(data, aes(vowel, f1))+
  geom_boxplot(aes(fill = vowel))+
  coord_flip()

ggplot(data, aes(vowel, f2))+
  geom_boxplot(aes(fill = vowel))+
  coord_flip()

# 1.3

boxplot(data$f1[data$vowel == 'a'])$out

# 1.4

cor.test(data$f1, data$f2)

# 1.5

cor.test(data$f1[data$vowel == 'a'], data$f2[data$vowel == 'a'])
cor.test(data$f1[data$vowel == 'A'], data$f2[data$vowel == 'A'])
cor.test(data$f1[data$vowel == 'y'], data$f2[data$vowel == 'y'])

#1.6

model <- lm(f2 ~ f1, data)

#1.6.1

summary(model)$call
#1.6.2
summary(model)$adj.r.squared
#1.6.3
ggplot(data, aes(f2, f1))+
  geom_point(aes(col = vowel))+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_smooth(method='lm', se = F, col = 'gray')

#1.7

mix_model <- lmer(f1 ~ f2 + (1|vowel), data)

#1.7.1

summary(mix_model)$call

#1.7.2
summary(mix_model)$varcor$vowel[1]

#1.7.4
data$mix_pred <- predict(mix_model)
ggplot(data, aes(x = f2, y = f1))+
  geom_point(aes(col = vowel))+
  scale_y_reverse()+
  scale_x_reverse()+
  geom_line(data = data, aes(x = f2, y = mix_pred, col = vowel), size = 0.7)


# 2. English Lexicon Project data

data2 <- read.csv('C:/Users/Александра/Desktop/R.hw/ELP.csv', sep = ',')

#2.1
cor_matrix <- data2 %>% select_if(is.numeric) %>% cor
max_cor <- max(cor_matrix[lower.tri(cor_matrix)])
rownames(which(cor_matrix == max_cor, arr.ind=TRUE))

#2.2
ggplot(data2, aes(SUBTLWF, Mean_RT))+
  geom_point(aes(col = Length))+
  scale_color_continuous(low = "lightblue", high = "red")+
  facet_wrap(~ POS)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_trans(x = "log10")

#2.3
model2 <- lm(Mean_RT ~ log(SUBTLWF) + POS, data2)

#2.3.1
summary(model2)$call

#2.3.2
summary(model2)$adj.r.squared

#2.3.3
ggplot(data2, aes(log(SUBTLWF), Mean_RT))+
  geom_point(aes(col = Length))+
  scale_color_continuous(low = "lightblue", high = "red")+
  geom_smooth(method='lm', se = F, col = 'gray')

#2.4
mix_model2 <- lmer(Mean_RT ~ log(SUBTLWF) + (1|POS), data2)

#2.4.1
summary(mix_model2)$call

#2.4.2
summary(mix_model2)$varcor$POS[1]

#2.4.3
data2$mix_pred <- predict(mix_model2)
ggplot(data2, aes(log(SUBTLWF), Mean_RT))+
  geom_point(aes(col = POS))+
  facet_wrap(~ POS)+
  geom_line(data = data2, aes(x = log(SUBTLWF), y = mix_pred), col = 'black', size = 0.7)


# 3. Dutch causative constructions

d_caus <-  read.csv('C:/Users/Александра/Desktop/R.hw/dutch_causatives.txt')

### 3.1
# It should be analysed using Fisher`s Exact Test because the number of observations is < 25 for some levels of this variable (clauses).
table(d_caus$CeSynt)
fisher.test(d_caus$Aux, d_caus$CeSynt)$p.value # significant

### 3.2
chisq.test(d_caus$Aux, d_caus$CrSem)$p.value # significant
chisq.test(d_caus$Aux, d_caus$CeSem)$p.value # non-significant
chisq.test(d_caus$Aux, d_caus$CdEvSem)$p.value # significant
chisq.test(d_caus$Aux, d_caus$EPTrans)$p.value # significant
chisq.test(d_caus$Aux, d_caus$Country)$p.value # significant
chisq.test(d_caus$Aux, d_caus$Domain)$p.value # significant

### 3.3
(chi_exp <- chisq.test(d_caus$Aux, d_caus$EPTrans)$expected)

### 3.4.
(chi_exp[1][1]/chi_exp[3][1])/(chi_exp[3][1]/chi_exp[4][1])

### 3.5
# library(cramer)
# cramer.test(d_caus$Aux, d_caus$EPTrans) ??

### 3.7.
library(vcd)
vcd::mosaic(~ Aux + CrSem + Country, data=d_caus, shade=TRUE, legend=TRUE)

### 3.8
# Just because performing this test we don't consider the dependence between independent variables, I mean in the formula.

