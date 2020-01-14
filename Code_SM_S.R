
library(foreign)
library(tidyverse)
library(repmis)
library(lme4)
library(stargazer)
library(jtools)
library(gplots)

# Here is the link, where you can get the data from: 
# http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp
# Download the data in R and SPSS (sav) formats. You will need them both.


#### Merging WVS data with country-level data ####

# Load data
countries <- read.csv(file.choose(), header = TRUE, sep = ",", dec = ".") #csv file "Regimes"

load(file.choose()) # load data from WVS 6th wave "WV6_Data_v_2015_04_18"
data1 <- WV6_Data_v_2015_04_18 # a data frame
table(data1$V2) # countries in the data

# merge country and individual level data
data <- merge(data1, countries, by="V2", all.x=TRUE)

# check if everything went well
names(data)
table(data$polity)
table(data$X)

# data[data == "China"] <- NA # in case we need to delete countries
# data[data == "Jordan"] <- NA

#### Recoding variables of interest ####

# member
table(data$V29)
data$V29[data$V29 < 0] <- NA
table(data$V29)

table(data$V30)
data$V30[data$V30 < 0] <- NA

table(data$V31)
data$V31[data$V31 < 0] <- NA

table(data$V32)
data$V32[data$V32 < 0] <- NA

member<-(data$V29+data$V30+data$V31+data$V32)/4
member<-as.numeric(member)
table(member)


# Key:
# 1 - democracy (+5 to +10), 
# 2 — anocracy (-5 to +5) or transitional regime,
# 3 — autocracy (-10 to -6)


# news
table(data$V223)
data$V223[data$V223 < 1] <- NA
data$news[data$V223 == 1 | data$V223 == 2] <- 1
data$news[data$V223 > 3 | data$V223 == 3] <- 0
table(data$news)
news<-factor(data$news)
table(news)

# check control vars for the model

# radio
table(data$V220)
data$V220[data$V220 < 1] <- NA
data$radio[data$V220 == 1 | data$V220 == 2] <- 1
data$radio[data$V220 > 3 | data$V220 == 3] <- 0
table(data$radio)
radio<-factor(data$radio)
table(radio)
radio<-factor(radio)

# daily newspapers
table(data$V217)
data$V217[data$V217 < 1] <- NA
data$paper[data$V217 == 1 | data$V217 == 2] <- 1
data$paper[data$V217 > 3 | data$V217 == 3] <- 0
table(data$paper)
paper<-factor(data$paper)
table(paper)
newspapers<-factor(paper)

# TV
table(data$V219)
data$V219[data$V219 < 1] <- NA
data$tv[data$V219 == 1 | data$V219 == 2] <- 1
data$tv[data$V219 > 3 | data$V219 == 3] <- 0
table(data$tv)
TV<-factor(data$tv)
table(TV)

# mediaindex<-(TV+radio+newspapers)/3
# table(mediaindex)

# edu
table(data$V248)
hist(data$V248)
data$V248[data$V248 < 1] <- NA
education<-as.numeric(data$V248)
table(education)

# politics
table(data$V84)
data$V84[data$V84 < 1] <- NA
data$poly[data$V84 == 1 | data$V84 == 2] <- 1
data$poly[data$V84 > 3 | data$V84 == 3] <- 0
table(data$poly)
politics<-factor(data$poly)

# friends
table(data$V224)
data$V224[data$V224 < 1] <- NA
data$friend[data$V224 == 1 | data$V224 == 2] <- 1
data$friend[data$V224 > 3 | data$V224 == 3] <- 0
table(data$friend)
friends<-factor(data$friend)

# age
hist(data$V242)
table(data$V242)
data$V242[data$V242 > 65 | data$V242 < 18] <- NA
table(data$V242)
age<-as.numeric(data$V242)
summary(age)

table(age)
data$agecat[age > 60] <- "Elder"
data$agecat[age > 35 & age <= 60] <- "Middle Aged"
data$agecat[age <= 35] <- "Young"
table(data$agecat)
agecat<-factor(data$agecat)

# employment
table(data$V229)
data$V229[data$V229 < 1] <- NA
data$empl[data$V229 == 7] <- 0
data$empl[data$V229 == 8 | data$V229 < 7 ] <- 1
table(data$empl)
employment<-factor(data$empl)


# Tricks with variable "protest"

# crosstab(data$V92, data$V87)
table(data$V87,data$V92)

# protest rec
table(data$V92) # recent protesters
table(data$V87) # all protesters

cbind(data$V87, data$V92)

protest = 
  (data$V87 == -1 & data$V92 == -3) * 3 +
  (data$V87 == -2 & data$V92 == -3) * 3 +
  (data$V87 == -5 & data$V92 == -3) * 3 +
  (data$V87 == -4 & data$V92 == -4) * 3 +
  (data$V87 == 1 & data$V92 == -5) * 3 +
  (data$V87 == -2 & data$V92 == -2) * 3 +
  (data$V87 == -1 & data$V92 == -2) * 3 +
  (data$V87 == -1 & data$V92 == -1) * 3 +
  (data$V87 == -1 & data$V92 == 1) * 3 +
  (data$V87 == -1 & data$V92 == 2) * 3 +
  (data$V87 == -2 & data$V92 == 2) * 3 +
  (data$V87 == -2 & data$V92 == 3) * 3 +
  (data$V87 == 1 & data$V92 == -3) * 0 +
  (data$V87 == 2 & data$V92 == -3) * 0 +
  (data$V87 == 3 & data$V92 == -3) * 0 +
  (data$V87 == 3 & data$V92 == -2) * 0 +
  (data$V87 == 2 & data$V92 == -2) * 0 +
  (data$V87 == 1 & data$V92 == -2) * 0 +
  (data$V87 == 1 & data$V92 == -1) * 0 +
  (data$V87 == 1 & data$V92 == 1) * 0 +
  (data$V87 == 2 & data$V92 == 1) * 0 +
  (data$V87 == 3 & data$V92 == 1) * 0 +
  (data$V87 == 3 & data$V92 == -1) * 0 +
  (data$V87 == 1 & data$V92 == 2) * 1 +
  (data$V87 == 1 & data$V92 == 3) * 1 +
  (data$V87 == 1 & data$V92 == 4) * 1 +
  (data$V87 == 1 & data$V92 == 5) * 1 +
  (data$V87 == 2 & data$V92 == 2) * 1 +
  (data$V87 == 2 & data$V92 == 3) * 1 +
  (data$V87 == 2 & data$V92 == 4) * 1 +
  (data$V87 == 2 & data$V92 == 5) * 1 +
  (data$V87 == 3 & data$V92 == 2) * 1 +
  (data$V87 == 3 & data$V92 == 3) * 1 +
  (data$V87 == 3 & data$V92 == 4) * 1 +
  (data$V87 == 3 & data$V92 == 5) * 1

table(protest)
protest[protest > 1] <- NA
protest <- factor(protest)
table(protest)
summary(protest)

# gender
table(data$V240)
data$V240[data$V240 < 1] <- NA
gender <- data$V240
table(gender)

#country
country <- factor(data$V2)

hist(data$polity)
hist(data$dummy)

Polity <- as.numeric(data$polity)
table(Polity)
Regime <- factor(data$dummy)
table(Regime)
GDP <- as.numeric(data$X)
table(GDP)

# You can also save the data only with variables of interest for regression analysis
# A data frame with all needed variables 
allvars <- data.frame(protest, news, politics, friends, member, age, gender,
                      employment, education, TV, newspapers, radio, country, 
                      Regime, GDP, Polity)
MYDATA<-na.omit(allvars)
write.csv(MYDATA, file = "LINISdata.csv") # for supplementary materials
summary(MYDATA)

# summary statistics
stargazer(MYDATA, type="html",  title="Descriptive statistics", digits=2, out="sumstat.htm")


##### Analysis #########

# Hypothesis 1. Full model, with all controls

m1.age<-glmer(protest ~ news + politics + friends + member + age^2 + factor(gender)
              + employment + education + (1|country), 
              family = binomial(link="logit"))

summary(m1.age)

m1.Age <- glmer(protest ~ news + politics + friends + member + age + factor(gender)
                + employment + education + (1|country), 
                family = binomial(link="logit"))

summary(m1.Age)

# no age, Hypothesis 1
m1 <- glmer(protest ~ news + politics + friends + member
            + factor(gender) + employment + education + (1|country), 
            family = binomial(link="logit"))

summary(m1)


# with Regime, Hypothesis 1
m2 <- glmer(protest ~ news + politics + friends + member
            + factor(gender) + employment + education + Regime + (1|country), 
            family = binomial(link="logit"))
summary(m2)

# with GDP
m3 <- glmer(protest ~ news + politics + friends + member + factor(gender)
            + employment + education + log(GDP) + (1|country), 
            family = binomial(link="logit"))
summary(m3)

# news random effect
# random effect of news controlling for Regime
m4 <- glmer(protest ~ news + politics + friends + member
            + factor(gender) + employment + education + Regime + (news|country), 
            family = binomial(link="logit"))

summary(m4)

plot_summs(m1.age, scale = FALSE)

# Hypothesis 2
# interest in politics and news (interaction term)

table(politics,news)

politics.news = 
  (politics == 0 & news == 0) * 0 +
  (politics == 1 & news == 0) * 1 +
  (politics == 0 & news == 1) * 2 +
  (politics == 1 & news == 1) * 3

table(politics.news)


mH2 <- glmer(protest ~ factor(politics.news) + friends + member + factor(gender)
             + employment + education + Regime + (1|country), 
             family = binomial(link="logit"))

summary(mH2)

plot_summs(mH2, scale = FALSE)



# Hypothesis 3, see supplementary R-codes by regime


# tables for MS Word

stargazer(m1, m2, m3, m4, title="Results", 
          align=TRUE, out="H1reg.htm", type="html")

stargazer(mH2, title="Results", 
          align=TRUE, out="H2reg.htm", type="html")

stargazer(mH3, title="Results", 
          align=TRUE, out="H3reg.htm", type="html")

#### Simulations ####

# for H1

remove.packages("lme4")
install.packages("MASS")
library(MASS)

gamma.hat <- fixef(m1) # note that here is fixef instead of coef since the model has two levels
V.hat <- vcov(m1)
S <- mvrnorm(1000,gamma.hat,V.hat)
head(S)
dim(S)

scenario1 <- cbind(1,1,0,0,mean(member,na.rm=TRUE),0,0, mean(education,na.rm=TRUE)) # news
scenario2 <- cbind(1,0,0,0,mean(member,na.rm=TRUE),0,0, mean(education,na.rm=TRUE)) # no news


X.c <- as.matrix(rbind(scenario1,scenario2))
mu.c <- S %*% t(X.c)
theta_c <- 1/(1+exp(-mu.c)) # link-function
head(theta_c)

quants <- apply(theta_c,2, quantile, c(0.025,0.5,0.975))
mean <- apply(theta_c,2, mean)

#par(mfrow=c(1,1))
fd <- theta_c[,1] - theta_c[,2] # first difference
hist(fd)
boxplot(fd)
mean(fd)
quantile(fd)


d1 = data.frame(protest=c("Do not read", "Read"), mean=c(0.04,0.06), 
                lower=c(0.03,0.045), upper=c(0.051,0.075))
p1 <- ggplot() + 
  geom_errorbarh(data = d1, mapping=aes(x=upper, y=protest, xmax=lower, xmin=upper), 
                 height=0.2, size=1, color="black") + 
  geom_point(data=d1, mapping=aes(y=protest, x=mean), 
             size=4, shape=21, fill="black") + 
  theme_bw() + ylab("Online news") + xlab("Pr(Protest participation),% (FD=0.018[0.01,0.02])")


# for H2

gamma.hat2 <- fixef(mH2) # note that here is fixef instead of coef since the model has two levels
V.hat2 <- vcov(mH2)
S2 <- mvrnorm(1000,gamma.hat2,V.hat2)
head(S2)
dim(S2)

scenario21 <- cbind(1,0,0,0,0,mean(member,na.rm=TRUE),0,0, mean(education,na.rm=TRUE),1,1) # 
scenario22 <- cbind(1,1,0,0,0,mean(member,na.rm=TRUE),0,0, mean(education,na.rm=TRUE),1,1) # 
scenario23 <- cbind(1,0,1,0,0,mean(member,na.rm=TRUE),0,0, mean(education,na.rm=TRUE),1,1) # 
scenario24 <- cbind(1,0,0,1,0,mean(member,na.rm=TRUE),0,0, mean(education,na.rm=TRUE),1,1) # 

X.c2 <- as.matrix(rbind(scenario21,scenario22,scenario23,scenario24))
mu.c2 <- S2 %*% t(X.c2)
theta_c2 <- 1/(1+exp(-mu.c2)) # link-function
head(theta_c2)

quants2 <- apply(theta_c2,2, quantile, c(0.025,0.5,0.975))
mean2 <- apply(theta_c2,2, mean)

fd2 <- theta_c2[,2]-theta_c2[,1] # first difference
mean(fd2)
quantile(fd2)

fd3 <- theta_c2[,2]-theta_c2[,3]
mean(fd3)
quantile(fd3)

fd4 <- theta_c2[,4]-theta_c2[,3]
mean(fd4)
quantile(fd4)

d2=data.frame(protest=c("No politics, no news","Politics, no news",
                        "No politics, news","Politics, news"), mean=c(0.04,0.07,0.05,0.1), 
              lower=c(0.03,0.05,0.03,0.078), upper=c(0.055,0.093,0.073,0.13))
ggplot() + 
  geom_errorbarh(data=d2, mapping=aes(x=upper, y=protest, xmax=lower, xmin=upper), 
                 height=0.2, size=1, color="black") + 
  geom_point(data=d2, mapping=aes(y=protest, x=mean), 
             size=4, shape=21, fill="black") + 
  theme_bw() + ylab("Interest in politics and online news consumption") +
  xlab("Pr(Protest participation),% CIs for all FD > 0")


# for H3

gamma.hat3 <- fixef(mH3)
V.hat3 <- vcov(mH3)
S3 <- mvrnorm(1000,gamma.hat3,V.hat3)
head(S3)
dim(S3)

seq.age <- seq(min(age,na.rm=TRUE), max(age, na.rm=TRUE), length.out=1000)
scenario31 <- cbind(1,0,mean(age,na.rm=TRUE),0,0,mean(member,na.rm=TRUE),1,0, mean(education,na.rm=TRUE),1,1,seq.age)
head(scenario31)

X.c3 <- as.matrix(rbind(scenario31))
head(X.c3)
mu.c3 <- S3 %*% t(X.c3)
head(mu.c3)
theta_c3 <- 1/(1+exp(-mu.c3)) # link-function
head(theta_c3)

quants3 <- apply(theta_c3, 2, quantile, c(0.025,0.5,0.975))
mean3 <- apply(theta_c3, 2, mean)
head(mean3)

fd3 <- theta_c3[72,]-theta_c3[73,]
mean(fd3)
quantile(fd3)


#### Robustness checks ####
# other media in the model

m5 <- glmer(protest ~ news + newspapers + radio + TV + politics +
              friends + member + employment + education + sex + 
              Regime + (1|country), 
            family = binomial(link="logit"))

summary(m5)

stargazer(m5, title="Results", 
          align=TRUE, out="M5_media.htm", type="html")

# By regimes (see key in online appendix)

#anocracies
data[data == 12]<-NA #data[data == "Algeria"]<-NA
data[data == 51]<-NA #data[data == "Armenia"]<-NA
data[data == 218]<-NA #data[data == "Ecuador"]<-NA
data[data == 368]<-NA #data[data == "Iraq"]<-NA
data[data == 400]<-NA #data[data == "Jordan"]<-NA
data[data == 422]<-NA #data[data == "Lebanon"]<-NA
data[data == 504]<-NA #data[data == "Morocco"]<-NA
data[data == 566]<-NA #data[data == "Nigeria"]<-NA
data[data == 643]<-NA #data[data == "Russia"]<-NA
data[data == 702]<-NA #data[data == "Singapore"]<-NA
data[data == 716]<-NA #data[data == "Zimbabwe"]<-NA
data[data == 788]<-NA #data[data == "Tunisia"]<-NA
data[data == 818]<-NA #data[data == "Egypt"]<-NA
data[data == 887]<-NA #data[data == "Yemen"]<-NA

#autocracies
data[data == 31]<-NA #data[data == "Azerbaijan"]<-NA
data[data == 48]<-NA #data[data == "Bahrain"]<-NA
data[data == 156]<-NA #data[data == "China"]<-NA
data[data == 398]<-NA #data[data == "Kazakhstan"]<-NA
data[data == 414]<-NA # data[data == "Kuwait"]<-NA
data[data == 434]<-NA #data[data == "Libya"]<-NA     
data[data == 634]<-NA #data[data == "Qatar"]<-NA
data[data == 646]<-NA #data[data == "Rwanda"]<-NA
data[data == 860]<-NA #data[data == "Uzbekistan"]<-NA

#democracies
#data[data == 32]<-NA #data[data == "Argentina"]<-NA
#data[data == 36]<-NA #data[data == "Australia"]<-NA
#data[data == 76]<-NA #data[data == "Brazil"]<-NA
#data[data == 152]<-NA #data[data == "Chile"]<-NA
#data[data == 170]<-NA #data[data == "Colombia"]<-NA
#data[data == 196]<-NA #data[data == "Cyprus"]<-NA
#data[data == 233]<-NA #data[data == "Estonia"]<-NA
#data[data == 268]<-NA #data[data == "Georgia"]<-NA
#data[data == 276]<-NA #data[data == "Germany"]<-NA
#data[data == 288]<-NA #data[data == "Ghana"]<-NA
#data[data == 356]<-NA #data[data == "India"]<-NA
#data[data == 392]<-NA #data[data == "Japan"]<-NA
#data[data == 410]<-NA #data[data == "South Korea"]<-NA
#data[data == 417]<-NA #data[data == "Kyrgyzstan"]<-NA     
#data[data == 458]<-NA #data[data == "Malaysia"]<-NA
#data[data == 484]<-NA #data[data == "Mexico"]<-NA
#data[data == 528]<-NA #data[data == "Netherlands"]<-NA
#data[data == 554]<-NA #data[data == "New Zealand"]<-NA
#data[data == 586]<-NA #data[data == "Pakistan"]<-NA     
#data[data == 604]<-NA #data[data == "Peru"]<-NA
#data[data == 608]<-NA #data[data == "Philippines"]<-NA
#data[data == 616]<-NA #data[data == "Poland"]<-NA
#data[data == 642]<-NA #data[data == "Romania"]<-NA
#data[data == 705]<-NA #data[data == "Slovenia"]<-NA
#data[data == 710]<-NA #data[data == "South Africa"]<-NA
#data[data == 764]<-NA #data[data == "Thailand"]<-NA
#data[data == 724]<-NA #data[data == "Spain"]<-NA
#data[data == 840]<-NA #data[data == "United States"]<-NA
#data[data == 858]<-NA #data[data == "Uruguay"]<-NA
#data[data == 752]<-NA #data[data == "Sweden"]<-NA
#data[data == 792]<-NA #data[data == "Turkey"]<-NA     
#data[data == 804]<-NA #data[data == "Ukraine"]<-NA

#not in the sample
data[data == 112]<-NA #data[data == "Belarus"]<-NA
data[data == 275]<-NA #data[data == "Palestine"]<-NA
data[data == 344]<-NA #data[data == "Hong Kong"]<-NA
data[data == 158]<-NA #data[data == "Taiwan"]<-NA
data[data == 780]<-NA #data[data == "Trinidad and Tobago"]<-NA



#### Barcharts for news and protests #### 

library(gridExtra)
library(grid)
library(lattice)
library(Hmisc)


df <- spss.get(file.choose(), use.value.labels=TRUE) # load the data from WVS in spss format

# country
table(df$V2)

# news 
table(df$V223)
df$news[df$V223 == "Daily" | df$V223 == "Weekly" ] <- "ARead"
df$news[df$V223 == "Monthly" | df$V223 == "Less than monthly" | df$V223 == "Never"] <- "Otherwise"
table(df$news, df$V2)

# remove NAs
df <- df[!is.na(df$news), ]

df2 <- df %>% 
  group_by(V2, news) %>% 
  tally() %>% 
  complete(news, fill = list(n = 114)) %>% 
  mutate(percentage = n / sum(n) * 100)

df2 <- df2[order(df2$news, -df2$percentage),]
df4 <- arrange(df2, news, -desc(percentage))
df4$V2 <- factor(df2$V2, levels = unique(df2$V2)) 

news_plot <- ggplot(df4, aes(x = V2, y = percentage,
             fill = factor(news, labels = c("Less than weekly", "Daily or weekly")))) + 
  geom_bar(stat = "identity", position = "stack", width = .8) + 
  guides(fill = guide_legend(title = "Online News Consumption")) + 
  coord_flip() + scale_fill_grey(start = .7, end = .3) + ylab("Percent (%)") + xlab("") +
  scale_x_discrete(limits = rev(levels(df4$V2)))


# protest
table(df$V87, df$V92)
cbind(df$V87, df$V92)
table(df$V87)
table(df$V92)
df$V87 <- as.numeric(df$V87)
df$V92 <- as.numeric(df$V92)

df$protest = 
  (df$V87 == 1 & df$V92 == 1) * 0 +
  (df$V87 == 2 & df$V92 == 1) * 0 +
  (df$V87 == 3 & df$V92 == 1) * 0 +
  (df$V87 == 1 & df$V92 == 2) * 1 +
  (df$V87 == 1 & df$V92 == 3) * 1 +
  (df$V87 == 1 & df$V92 == 4) * 1 +
  (df$V87 == 1 & df$V92 == 5) * 1 +
  (df$V87 == 2 & df$V92 == 2) * 1 +
  (df$V87 == 2 & df$V92 == 3) * 1 +
  (df$V87 == 2 & df$V92 == 4) * 1 +
  (df$V87 == 2 & df$V92 == 5) * 1 +
  (df$V87 == 3 & df$V92 == 2) * 1 +
  (df$V87 == 3 & df$V92 == 3) * 1 +
  (df$V87 == 3 & df$V92 == 4) * 1 +
  (df$V87 == 3 & df$V92 == 5) * 1


table(df$V87)
df$prot[df$V87 == 1 ] <- "AHave done"
df$prot[df$V87 >= 2 ] <- "Otherwise"
table(df$prot, df$V2)

# remove NAs
df <- df[!is.na(df$prot), ]

dfp <-  df %>% 
  group_by(V2, prot) %>% 
  tally() %>% 
  complete(prot, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)


dfp <-  dfp[order(dfp$prot, dfp$percentage),]
dfp <- arrange(dfp, prot, desc(percentage))
dfp$V2 <-  factor(dfp$V2, levels = unique(dfp$V2)) 

# protest_plot
protest_plot <-  ggplot(dfp, aes(x = V2, y = percentage,
                                 fill = factor(prot, labels = c("Have done", "Otherwise")))) + 
  geom_bar(stat = "identity", position = "stack", width = .8) + 
  guides(fill = guide_legend(title = "Protest Participation")) + 
  coord_flip() + scale_fill_grey(start = .3, end = .7) + ylab("Percent (%)") + xlab("") +
  scale_x_discrete(limits = rev(levels(dfp$V2)))

# put plots next to each other
gridExtra::grid.arrange(protest_plot, news_plot, ncol=2)


#### Plot with lm function ####

protest_done <- dfp[!(dfp$prot=="Otherwise"),]
news_read <- df2[!(df2$news=="Otherwise"),]
merged_df <- merge(protest_done, news_read, by = "V2", all.x=TRUE)

require("ggrepel")

ggplot(merged_df, aes(percentage.y, percentage.x, label=merged_df$V2)) + 
  geom_point() + 
  geom_text_repel(size=2, hjust=0.5, vjust=0.5) +
  stat_smooth(method = "lm", formula = y~x, color="black") +
  labs(x = "Population reading news online daily or weekly (%)", 
       y = "Population who have participated in protests (%)") + 
  theme_classic()




