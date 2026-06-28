#### Loading packages
library(dplyr)
library(ggplot2)
library(sandwich)
library(tableone)
library(tmle)
library(earth)
library(Matching)
library(survey)

#### Retrieving datasets
IND <- read.csv("IND_dataset.csv")
JAX <- read.csv("JAX_dataset.csv")
KC <- read.csv("KC_dataset.csv")
IND1 <-IND
JAX1 <- JAX
KC1 <- KC


#### Chevauchement, Bootstrap, IPTW, Matching and TMLESL function
bootstrap.anything <- function(funct, DAT1, K = 500, n = nrow(DAT1)) {
  first_estimate <- funct(DAT1)
  p <- length(first_estimate)
  vect.est <- matrix(NA, nrow = K, ncol = p)
  colnames(vect.est) <- names(first_estimate)
  for (k in 1:K) {
    resamp <- sample(1:n, n, replace = TRUE)
    DATk <- DAT1[resamp, ]
    estimate <- funct(DATk)
    vect.est[k, ] <- estimate
    print(k)
  }
  CIs <- apply(vect.est, 2, quantile, probs = c(0.025, 0.975))
  VARs <- apply(vect.est, 2, var)
  CIlow_named <- setNames(CIs[1, ], paste0(names(first_estimate), "_CI_low"))
  CIhigh_named <- setNames(CIs[2, ], paste0(names(first_estimate), "_CI_high"))
  VAR_named <- setNames(VARs, paste0(names(first_estimate), "_var"))
  return(list(
    VAR = VAR_named,
    CIlow = CIlow_named,
    CIhigh = CIhigh_named,
    estList = vect.est
  ))
}


chevauchement<-function(treat,pscore,hist.width=0.01){
  treated<-as.data.frame(cbind(pscore=pscore[treat==1]))
  untreated<-as.data.frame(cbind(pscore=pscore[treat==0]))
  treated$type<-"tx=1"
  untreated$type<-"tx=0"
  df<-rbind(treated,untreated)
  ggplot(df,aes(pscore,fill=type))+geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', binwidth=hist.width)
}

IPTW_IND<-function(DAT1){
  prop_score <- glm(exposure ~ field_location + surface_new + roof_new + def_team_rating + off_team_rating
                    + I(spread_line_new^2) + I(temp_new^3) + wind_new, family = binomial(), data=DAT1)
  pi <-predict.glm(prop_score,type="response") 
  w1<-1/pi
  w0<-1/(1-pi)
  w<-exposure*w1+(1-exposure)*w0
  wmod<-glm(outcome~exposure,weights=w, data=DAT1)
  est_IPTW <- wmod$coefficients[2]
  return(c(IPTW = est_IPTW))
}

IPTW_JAX<-function(DAT1){
  prop_score <- glm(exposure ~ field_location + surface_new + roof_new + def_team_rating + off_team_rating
      + spread_line_new + I(temp_new^2) + I(wind_new^4), family = binomial(), data=DAT1)
  pi <-predict.glm(prop_score,type="response") 
  w1<-1/pi
  w0<-1/(1-pi)
  w<-exposure*w1+(1-exposure)*w0
  wmod<-glm(outcome~exposure,weights=w, data=DAT1)
  est_IPTW <- wmod$coefficients[2]
  return(c(IPTW = est_IPTW))
}


IPTW_KC<-function(DAT1){
  prop_score <- glm(exposure ~ field_location + surface_new + roof_new + def_team_rating + off_team_rating
                    + spread_line_new + temp_new + I(wind_new^2), family = binomial(), data=DAT1)
  pi <-predict.glm(prop_score,type="response") 
  w1<-1/pi
  w0<-1/(1-pi)
  w<-exposure*w1+(1-exposure)*w0
  wmod<-glm(outcome~exposure,weights=w, data=DAT1)
  est_IPTW <- wmod$coefficients[2]
  return(c(IPTW = est_IPTW))
}


tmle_funSL<-function(DAT1){
  Y <- DAT1$outcome
  A <- DAT1$exposure
  W <- data.frame(
    spread_line_new = DAT1$spread_line_new,
    temp_new = DAT1$temp_new,
    wind_new = DAT1$wind_new,
    def_team_rating = DAT1$def_team_rating,
    off_team_rating = DAT1$off_team_rating,
    field_location = DAT1$field_location,
    surface_new = DAT1$surface_new,
    roof_new = DAT1$roof_new)
  W$obsWeights=rep(1,length(Y))
  estimate<-tmle(Y=Y,A=A,W=W,Q.SL.library = c("SL.glm", "SL.mean", "SL.earth"),
                 g.SL.library = c("SL.glm", "SL.mean", "SL.earth"), family="binomial")
  estimateATC <- estimate$estimates$ATC$psi
  estimateATT <- estimate$estimates$ATT$psi
  estimateATE <- estimate$estimates$ATE$psi
  return(c(ATC = estimateATC, ATT = estimateATT, ATE = estimateATE))
}


#### IND analysis

### Descriptive stats
continuous_vars <- c("temp_new", "spread_line_new", "wind_new")  
categorical_vars <- c("outcome", "field_location", "surface_new", "roof_new", "def_team_rating", "off_team_rating")  
all_vars <- c(continuous_vars, categorical_vars)
table1IND <- CreateTableOne(vars = all_vars, strata = "exposure", data = IND, factorVars = categorical_vars)
print(table1IND, showAllLevels = TRUE)


### Risk Difference via IPTW, TMLE and Matching
# standardization
IND$spread_line_new = (IND$spread_line_new-mean(IND$spread_line_new))/sd(IND$spread_line_new)
IND$temp_new = (IND$temp_new-mean(IND$temp_new))/sd(IND$temp_new)
IND$wind_new = (IND$wind_new-mean(IND$wind_new))/sd(IND$wind_new)


### TMLE
attach(IND)

# neff <= 500 and have continuous covariates
n_IND = dim(IND)[1]
p_IND = sum(IND$outcome)/n_IND
neff_IND  = min(n_IND, 5*(n_IND*min(p_IND, 1-p_IND)))

# model
Y=IND$outcome
A=IND$exposure
L1=IND$spread_line_new
L2=IND$temp_new
L3=IND$wind_new
L4=IND$def_team_rating
L5=IND$off_team_rating
L6=IND$field_location
L7=IND$surface_new
L8=IND$roof_new
W=as.data.frame(cbind(L1,L2,L3,L4,L5,L6,L7,L8))
W$obsWeights=rep(1,length(Y))

set.seed(123)
TMLE_IND<-tmle(Y=Y,A=A,W=W,Q.SL.library = c("SL.glm", "SL.mean", "SL.earth"),
               g.SL.library = c("SL.glm", "SL.mean", "SL.earth"), family="binomial")
TMLE_IND$Qinit$coef
TMLE_IND$g$coef



### IPTW
# linearity, quadratic term, log transform and skewness
hist(spread_line_new)
hist(temp_new)
hist(wind_new)
ggplot(IND, aes(x = spread_line_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Spread",
       y = "Probability of Exposure") +
  theme_minimal()
ggplot(IND, aes(x = temp_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Temperature",
       y = "Probability of Exposure") +
  theme_minimal()
ggplot(IND, aes(x = wind_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Wind",
       y = "Probability of Exposure") +
  theme_minimal()
model1 <- lm(exposure ~ spread_line_new)
model2 <- lm(exposure ~ temp_new)
model3 <- lm(exposure ~ wind_new)
summary(model1) 
summary(model2)
summary(model3) # looks best
model_quadratic1 <- lm(exposure ~ poly(spread_line_new, 2))
model_quadratic2 <- lm(exposure ~ poly(temp_new, 2))
model_quadratic3 <- lm(exposure ~ poly(wind_new, 2))
summary(model_quadratic1) # quad looks best
summary(model_quadratic2)
summary(model_quadratic3)
model_cubic1 <- lm(exposure ~ poly(spread_line_new, 3))
model_cubic2 <- lm(exposure ~ poly(temp_new, 3))
model_cubic3 <- lm(exposure ~ poly(wind_new, 3))
summary(model_cubic1)
summary(model_cubic2) # cub looks best
summary(model_cubic3) 
model_quartic1 <- lm(exposure ~ poly(spread_line_new, 4))
model_quartic2 <- lm(exposure ~ poly(temp_new, 4))
model_quartic3 <- lm(exposure ~ poly(wind_new, 4))
summary(model_quartic1)
summary(model_quartic2)
summary(model_quartic3)


# propensity score model
prop_scoreIND <- glm(exposure ~ field_location + surface_new + roof_new + def_team_rating + off_team_rating
                     + I(spread_line_new^2) + I(temp_new^3) + wind_new, 
                     family = binomial())
piIND <-predict.glm(prop_scoreIND,type="response") 
summary(piIND)
summary(1-piIND)


# weights for IPTW
w1<-1/piIND 
w0<-1/(1-piIND)
summary(w1[exposure==1])
summary(w0[exposure==0])
w<-exposure*w1+(1-exposure)*w0
wmod<-glm(outcome~exposure,weights=w)
summary(wmod)

# Truncate with Gruber (did nothing)
b.Gruber=sqrt(n_IND)*log(n_IND/5) 
w.tronc<-w

# Truncate the weights to keep only those between the 5th and 95th percentiles (did not really change conclusions)
p5 <- quantile(w, 0.05)
p95 <- quantile(w, 0.95)
w.tronc <- w
w.tronc[w < p5] <- p5  
w.tronc[w > p95] <- p95 
wmod<-glm(outcome~exposure,weights=w.tronc)
summary(wmod)


# Matching
chevauchement(treat=exposure,pscore=piIND,hist.width=0.01)
m<-Match(Y=NULL, Tr=exposure, X=piIND, 
         estimand = "ATT", M = 1,
         caliper = 0.09, replace=TRUE)
summary(m)
set.seed(123)
MatchBalance(exposure~field_location + surface_new + roof_new + def_team_rating + off_team_rating
             + spread_line_new + temp_new + wind_new,
             match.out=m,
             ks = TRUE, nboots=500)
m<-Match(Y=outcome, Tr=exposure, X=piIND, 
         estimand = "ATT", M = 1,
         caliper = 0.09, replace=TRUE)
summary(m)


# Table 1 post IPTW
design_iptw <- svydesign(ids = ~1, weights = ~w, data = IND1)
continuous_vars <- c("temp_new", "spread_line_new", "wind_new")  
categorical_vars <- c("outcome", "field_location", "surface_new", "roof_new", "def_team_rating", "off_team_rating")  
all_vars <- c(continuous_vars, categorical_vars)
table1_weighted <- svyCreateTableOne(vars = all_vars, strata = "exposure", 
                                     data = design_iptw, factorVars = categorical_vars)
print(table1_weighted, showAllLevels = TRUE, smd = TRUE)

# CI IND dd
set.seed(123)
CI_IPTW_IND <- bootstrap.anything(IPTW_IND, IND)
CI_TMLESL_IND <- bootstrap.anything(tmle_funSL, IND)







#### JAX analysis

### Descriptive stats
continuous_vars <- c("temp_new", "spread_line_new", "wind_new")  
categorical_vars <- c("outcome", "field_location", "surface_new", "roof_new", "def_team_rating", "off_team_rating")  
all_vars <- c(continuous_vars, categorical_vars)
table1JAX <- CreateTableOne(vars = all_vars, strata = "exposure", data = JAX, factorVars = categorical_vars)
print(table1JAX, showAllLevels = TRUE)


### Risk Difference via IPTW, TMLE and Matching
# standardization
JAX$spread_line_new = (JAX$spread_line_new-mean(JAX$spread_line_new))/sd(JAX$spread_line_new)
JAX$temp_new = (JAX$temp_new-mean(JAX$temp_new))/sd(JAX$temp_new)
JAX$wind_new = (JAX$wind_new-mean(JAX$wind_new))/sd(JAX$wind_new)


### TMLE
attach(JAX)

# neff <= 500 and have continuous covariates
n_JAX = dim(JAX)[1]
p_JAX = sum(JAX$outcome)/n_JAX
neff_JAX  = min(n_JAX, 5*(n_JAX*min(p_JAX, 1-p_JAX)))

# model
Y=JAX$outcome
A=JAX$exposure
L1=JAX$spread_line_new
L2=JAX$temp_new
L3=JAX$wind_new
L4=JAX$def_team_rating
L5=JAX$off_team_rating
L6=JAX$field_location
L7=JAX$surface_new
L8=JAX$roof_new
W=as.data.frame(cbind(L1,L2,L3,L4,L5,L6,L7,L8))
W$obsWeights=rep(1,length(Y))
set.seed(123)
TMLE_JAX<-tmle(Y=Y,A=A,W=W,Q.SL.library = c("SL.glm", "SL.mean", "SL.earth"),
               g.SL.library = c("SL.glm", "SL.mean", "SL.earth"), family="binomial")
TMLE_JAX$Qinit$coef
TMLE_JAX$g$coef



### IPTW
# linearity, quadratic term, log transform and skewness
hist(spread_line_new)
hist(temp_new)
hist(wind_new)
ggplot(JAX, aes(x = spread_line_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Spread",
       y = "Probability of Exposure") +
  theme_minimal()
ggplot(JAX, aes(x = temp_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Temperature",
       y = "Probability of Exposure") +
  theme_minimal()
ggplot(JAX, aes(x = wind_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Wind",
       y = "Probability of Exposure") +
  theme_minimal()
model1 <- lm(exposure ~ spread_line_new)
model2 <- lm(exposure ~ temp_new)
model3 <- lm(exposure ~ wind_new)
summary(model1) # looks best
summary(model2)
summary(model3) 
model_quadratic1 <- lm(exposure ~ poly(spread_line_new, 2))
model_quadratic2 <- lm(exposure ~ poly(temp_new, 2))
model_quadratic3 <- lm(exposure ~ poly(wind_new, 2))
summary(model_quadratic1) 
summary(model_quadratic2) # quad looks best
summary(model_quadratic3)
model_cubic1 <- lm(exposure ~ poly(spread_line_new, 3))
model_cubic2 <- lm(exposure ~ poly(temp_new, 3))
model_cubic3 <- lm(exposure ~ poly(wind_new, 3))
summary(model_cubic1)
summary(model_cubic2) 
summary(model_cubic3) 
model_quartic1 <- lm(exposure ~ poly(spread_line_new, 4))
model_quartic2 <- lm(exposure ~ poly(temp_new, 4))
model_quartic3 <- lm(exposure ~ poly(wind_new, 4))
summary(model_quartic1)
summary(model_quartic2)
summary(model_quartic3) # quart looks best


# propensity score model
prop_scoreJAX <- glm(exposure ~ field_location + surface_new + roof_new + def_team_rating + off_team_rating
                     + spread_line_new + I(temp_new^2) + I(wind_new^4), 
                     family = binomial())
piJAX <-predict.glm(prop_scoreJAX,type="response") 
summary(piJAX)
summary(1-piJAX)



# weights for IPTW
w1<-1/piJAX
w0<-1/(1-piJAX)
summary(w1[exposure==1])
summary(w0[exposure==0])
w<-exposure*w1+(1-exposure)*w0
wmod<-glm(outcome~exposure,weights=w)
summary(wmod)

# Truncate with Gruber (did nothing)
b.Gruber=sqrt(n_JAX)*log(n_JAX/5) 

# Truncate the weights to keep only those between the 5th and 95th percentiles (did not really change conclusions)
p5 <- quantile(w, 0.05)
p95 <- quantile(w, 0.95)
w.tronc <- w
w.tronc[w < p5] <- p5  
w.tronc[w > p95] <- p95 
wmod<-glm(outcome~exposure,weights=w.tronc)
summary(wmod)


# Matching
chevauchement(treat=exposure,pscore=piJAX,hist.width=0.01)
m<-Match(Y=NULL, Tr=exposure, X=piJAX, 
         estimand = "ATT", M = 1,
         caliper = 0.1, replace=TRUE)
summary(m)
set.seed(123)
MatchBalance(exposure~field_location + surface_new + roof_new + def_team_rating + off_team_rating
             + spread_line_new + temp_new + wind_new,
             match.out=m,
             ks = TRUE, nboots=500)
m<-Match(Y=outcome, Tr=exposure, X=piJAX, 
         estimand = "ATT", M = 1,
         caliper = 0.1, replace=TRUE)
summary(m)

# Table 1 post IPTW
design_iptw <- svydesign(ids = ~1, weights = ~w, data = JAX1)
continuous_vars <- c("temp_new", "spread_line_new", "wind_new")  
categorical_vars <- c("outcome", "field_location", "surface_new", "roof_new", "def_team_rating", "off_team_rating")  
all_vars <- c(continuous_vars, categorical_vars)
table1_weighted <- svyCreateTableOne(vars = all_vars, strata = "exposure", 
                                     data = design_iptw, factorVars = categorical_vars)
print(table1_weighted, showAllLevels = TRUE, smd = TRUE)


# CI JAX
CI_IPTW_JAX <- bootstrap.anything(IPTW_JAX, JAX)
CI_TMLESL_JAX <- bootstrap.anything(tmle_funSL, JAX)







#### KC analysis

### Descriptive stats
continuous_vars <- c("temp_new", "spread_line_new", "wind_new")  
categorical_vars <- c("outcome", "field_location", "surface_new", "roof_new", "def_team_rating", "off_team_rating")  
all_vars <- c(continuous_vars, categorical_vars)
table1KC <- CreateTableOne(vars = all_vars, strata = "exposure", data = KC, factorVars = categorical_vars)
print(table1KC, showAllLevels = TRUE)


### Risk Difference via IPTW, TMLE and Matching
# standardization
KC$spread_line_new = (KC$spread_line_new-mean(KC$spread_line_new))/sd(KC$spread_line_new)
KC$temp_new = (KC$temp_new-mean(KC$temp_new))/sd(KC$temp_new)
KC$wind_new = (KC$wind_new-mean(KC$wind_new))/sd(KC$wind_new)


### TMLE
attach(KC)

# neff <= 500 and have continuous covariates
n_KC = dim(KC)[1]
p_KC = sum(KC$outcome)/n_KC
neff_KC  = min(n_KC, 5*(n_KC*min(p_KC, 1-p_KC)))

# model
Y=KC$outcome
A=KC$exposure
L1=KC$spread_line_new
L2=KC$temp_new
L3=KC$wind_new
L4=KC$def_team_rating
L5=KC$off_team_rating
L6=KC$field_location
L7=KC$surface_new
L8=KC$roof_new
W=as.data.frame(cbind(L1,L2,L3,L4,L5,L6,L7,L8))
W$obsWeights=rep(1,length(Y))
set.seed(123)
TMLE_KC<-tmle(Y=Y,A=A,W=W,Q.SL.library = c("SL.glm", "SL.mean", "SL.earth"),
               g.SL.library = c("SL.glm", "SL.mean", "SL.earth"), family="binomial")
TMLE_KC$Qinit$coef
TMLE_KC$g$coef



### IPTW
# linearity, quadratic term, log transform and skewness
hist(spread_line_new)
hist(temp_new)
hist(wind_new)
ggplot(KC, aes(x = spread_line_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Spread",
       y = "Probability of Exposure") +
  theme_minimal()
ggplot(KC, aes(x = temp_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Temperature",
       y = "Probability of Exposure") +
  theme_minimal()
ggplot(KC, aes(x = wind_new, y = exposure)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.05), color = 1) +
  geom_smooth(method = "loess", color = 2, se = TRUE) +
  labs(title = "Exposure vs. Temperature",
       x = "Wind",
       y = "Probability of Exposure") +
  theme_minimal()
model1 <- lm(exposure ~ spread_line_new)
model2 <- lm(exposure ~ temp_new)
model3 <- lm(exposure ~ wind_new)
summary(model1) # looks best
summary(model2) # looks best
summary(model3)  
model_quadratic1 <- lm(exposure ~ poly(spread_line_new, 2))
model_quadratic2 <- lm(exposure ~ poly(temp_new, 2))
model_quadratic3 <- lm(exposure ~ poly(wind_new, 2))
summary(model_quadratic1) 
summary(model_quadratic2) 
summary(model_quadratic3) # quad looks best
model_cubic1 <- lm(exposure ~ poly(spread_line_new, 3))
model_cubic2 <- lm(exposure ~ poly(temp_new, 3))
model_cubic3 <- lm(exposure ~ poly(wind_new, 3))
summary(model_cubic1)
summary(model_cubic2) 
summary(model_cubic3) 
model_quartic1 <- lm(exposure ~ poly(spread_line_new, 4))
model_quartic2 <- lm(exposure ~ poly(temp_new, 4))
model_quartic3 <- lm(exposure ~ poly(wind_new, 4))
summary(model_quartic1)
summary(model_quartic2)
summary(model_quartic3) 


# propensity score model
prop_scoreKC <- glm(exposure ~ field_location + surface_new + roof_new + def_team_rating + off_team_rating
                     + spread_line_new + temp_new + I(wind_new^2), 
                     family = binomial())
piKC <-predict.glm(prop_scoreKC,type="response") 
summary(piKC)
summary(1-piKC)


# weights for IPTW
w1<-1/piKC
w0<-1/(1-piKC)
summary(w1[exposure==1])
summary(w0[exposure==0])
w<-exposure*w1+(1-exposure)*w0
wmod<-glm(outcome~exposure,weights=w)
summary(wmod)

# Truncate with Gruber (did nothing)
b.Gruber=sqrt(n_KC)*log(n_KC/5) 

# Truncate the weights to keep only those between the 5th and 95th percentiles (did not really change conclusions)
p5 <- quantile(w, 0.05)
p95 <- quantile(w, 0.95)
w.tronc <- w
w.tronc[w < p5] <- p5  
w.tronc[w > p95] <- p95 
wmod<-glm(outcome~exposure,weights=w.tronc)
summary(wmod)


# Matching
chevauchement(treat=exposure,pscore=piKC,hist.width=0.01)
m<-Match(Y=NULL, Tr=exposure, X=piKC, 
         estimand = "ATT", M = 1,
         caliper = 0.08, replace=TRUE)
summary(m)
set.seed(123)
MatchBalance(exposure~field_location + surface_new + roof_new + def_team_rating + off_team_rating
             + spread_line_new + temp_new + wind_new,
             match.out=m,
             ks = TRUE, nboots=500)
m<-Match(Y=outcome, Tr=exposure, X=piKC, 
         estimand = "ATT", M = 1,
         caliper = 0.08, replace=TRUE)
summary(m)


# Table 1 post IPTW
design_iptw <- svydesign(ids = ~1, weights = ~w, data = KC1)
continuous_vars <- c("temp_new", "spread_line_new", "wind_new")  
categorical_vars <- c("outcome", "field_location", "surface_new", "roof_new", "def_team_rating", "off_team_rating")  
all_vars <- c(continuous_vars, categorical_vars)
table1_weighted <- svyCreateTableOne(vars = all_vars, strata = "exposure", 
                                     data = design_iptw, factorVars = categorical_vars)
print(table1_weighted, showAllLevels = TRUE, smd = TRUE)


# CI KC
CI_IPTW_KC <- bootstrap.anything(IPTW_KC, KC)
CI_TMLESL_KC <- bootstrap.anything(tmle_funSL, KC)





