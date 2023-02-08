library(WebPower)

# logistic regression (univariate)
WebPower::wp.logistic(p0=0.165, p1=0.201, alpha=0.05, power=0.80, alternative="two.sided", family="Bernoulli")

WebPower::wp.logistic(p0=0.132, p1=0.5, alpha=0.05, power=0.80, alternative="two.sided", family="Bernoulli")

WebPower::wp.logistic(p0=0.28, p1=0.39, alpha=0.05, power=0.80, alternative="two.sided", family="Bernoulli")

# prevalence study

sample_calc_prev = function(p, z, d){
  # p = expected prevalence e.g. 0.3
  # z = confidence level e.g. 1.96
  # d = precision of effect size e.g. 0.05
  n = p*(1-p)*(z/d)^2
  return(n)
}

sample_calc_prev(0.02, 1.96, 0.05)
sample_calc_prev(0.08, 1.96, 0.05)

sample_calc_prev(0.1, 1.96, 0.05)
sample_calc_prev(0.3, 1.96, 0.05)
sample_calc_prev(0.6, 1.96, 0.05)
sample_calc_prev(0.9, 1.96, 0.05)

