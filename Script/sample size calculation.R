library(WebPower)
library(powerSurvEpi)

# Sample size calculation for logistic regression (univariate)
WebPower::wp.logistic(p0=0.165, p1=0.201, alpha=0.05, power=0.80, alternative="two.sided", family="Bernoulli")

WebPower::wp.logistic(p0=0.132, p1=0.5, alpha=0.05, power=0.80, alternative="two.sided", family="Bernoulli")

WebPower::wp.logistic(p0=0.28, p1=0.39, alpha=0.05, power=0.80, alternative="two.sided", family="Bernoulli")


# Sample size calculation for Cox PH regression
powerSurvEpi::powerEpi.default()
  # n = total number of subjects
  # theta = posulated hazard ratio
  # p = proportion of subjects taking the value one for the covariate of interest
  # psi = proportion of subjects died of disease of interest
  # rho2 = square of the correlation between the covariate of interest and the other covariate
  # alpha = type 1 error rate

#example
powerEpi.default(n = 2000,
                 theta = 1.25,
                 p = 0.02,
                 psi = 0.3,
                 rho2 = 0.3^2,
                 alpha = 0.05)

# Sample size calculation for survival analysis

hr=1.5       # hazard ratio 
hr0=1         # hypothesised hazard ratio under null
pE=0.3        # overall probability of event occurring within study period
pA=0.08       # proportions of sample size allotted to the two groups
alpha=0.05    # type 1 error
beta=0.20     # type 2 error (1-beta) = power
(n=((qnorm(1-alpha/2)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE))
ceiling(n)
(Power=pnorm((log(hr)-log(hr0))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha/2)))

# Sample size calculation for prevalence studies

sample_calc_prev = function(p, z, d){
  # p = expected prevalence e.g. 0.3
  # z = confidence level e.g. 1.96
  # d = precision of effect size e.g. 0.05
  n = p*(1-p)*(z/d)^2
  return(n)
}

sample_calc_prev(0.3, 1.96, 0.05)
sample_calc_prev(0.08, 1.96, 0.05)
sample_calc_prev(0.1, 1.96, 0.05)
sample_calc_prev(0.3, 1.96, 0.05)
sample_calc_prev(0.6, 1.96, 0.05)
sample_calc_prev(0.9, 1.96, 0.05)

