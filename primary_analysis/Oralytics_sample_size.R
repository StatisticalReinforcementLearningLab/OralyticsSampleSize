# Sample Size Calculation Oralytics
# install.packages("‘MRTSampleSize")

source("CalculateSampleSize.R")
source("internal.R")
source("PlotProximalEffect.R")

# First 35 days: starts at effect of 0.4, ave of 0.2, zero effect at day 35
all_days <- 1:70
beta_vals <- all_days * 0.4/35 * (-1) + 0.4
beta_vals[beta_vals < 0] <- 0 # zero out last 35 days

plot(all_days, beta_vals, xlab="Day in Study", ylab="Proximal Effect Size", 
     main="Proximal Effect Size Trend for Sample Size Calculations", xlim=c(1,70))

# effect sizes for all decision times
beta_vals_all <- rev(sort(c(beta_vals, beta_vals)))


# Sample size calculation
clip <- 0.2
availability <- rep(1,140)
power <- 0.93
params_main <- 5
params_treat <- 1
SampleSize(days=70, occ_per_day=2, beta_t=beta_vals_all, tau=availability, 
           delta=clip, alpha0=0.05, beta0=power, p=params_treat, q=params_main)

# ave effect first 35 days: 0.1, 0.2, 0.3, 0.4
# clipping: 0.25, 0.2, 0.15, 0.1

#clipvals <- c(0.1, 0.15, 0.2, 0.25)
clipvals <- c(0.2)
ave_effect_half_vals <- c(0.1, 0.15, 0.2, 0.25, 0.3)
for (ave_effect_half in ave_effect_half_vals) {
  for (clip in clipvals) {
    all_days <- 1:70
    beta_vals <- all_days * ave_effect_half*2/35 * (-1) + ave_effect_half*2
    beta_vals[beta_vals < 0] <- 0 # zero out last 35 days
    beta_vals_all <- rev(sort(c(beta_vals, beta_vals)))
    
    size <- SampleSize(days=70, occ_per_day=2, beta_t=beta_vals_all, tau=availability, 
               delta=clip, alpha0=0.05, beta0=power, p=params_treat, q=params_main)
    print( paste("ave_effect_half:", ave_effect_half, "; min_clip:", clip, "; sample_size:", size) )
  }
}

ave_effect_half <- 0.1
all_days <- 1:70
beta_vals <- all_days * ave_effect_half*2/35 * (-1) + ave_effect_half*2
beta_vals[beta_vals < 0] <- 0 # zero out last 35 days

# 70*0.85 = 59.5 (15% attrition)

# 40 users ave effect first 35 days is 0.2; clipping 0.2
# 40 users ave effect first 35 days is 0.2; clipping 0.2







'''
replicate(10, 0.1)

#plotProximalEffect(days=70, occ_per_day=2, beta_shape="linear", 
#                   beta_mean=0.1, beta_initial=0.1)

plotProximalEffect(days=70, occ_per_day=2, beta_shape="quadratic", 
                   beta_mean=0.1, beta_initial=0.1, 
                   beta_quadratic_max=18)

calculateSampleSize(days=70, occ_per_day=2, prob=0.2, 
                    beta_shape="linear", beta_mean=0.1, beta_initial=0.1,
                    tau_mean=1, tau_shape="linear", tau_initial=1, dimB=5,
                    sigLev=0.05, power=0.8)
'''



                  