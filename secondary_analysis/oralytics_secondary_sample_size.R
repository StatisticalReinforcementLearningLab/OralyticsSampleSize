
library(MRTSampleSizeBinary)
# Using the package: https://cran.r-project.org/web/packages/MRTSampleSizeBinary/index.html


BPstart <- 0.15 # BP stands for Base Probabilities under no prompt
BPend <- 0.1
RRstart <- 1.4
RRend <- 1.25


# Plotting baseline probability of app engagement ============================
# proximal engagement under no prompt: starting at 0.15 and ending at 0.1 (log-linear)
all_days <- 1:70
base_exp <- log((BPstart / BPend)) / log(70)
base_vals <- exp( log(BPstart) - base_exp* log(all_days) )
plot(all_days, base_vals, xlab="Day in Study", ylab="Probability of Proximal App Engagement", 
     main="Probability of Proximal App Engagement when \n Prompt is not Delivered", xlim=c(1,70),
     ylim=c(BPend,BPstart))


# Plotting RR treatment effect ============================
# RR for TE starting at 1.25 and ending with 1.15 (log-linear)
all_days <- 1:70
TE_exp <- log((RRstart / RRend)) / log(70)
TE_vals <- exp( log(RRstart) - TE_exp* log(all_days) )
plot(all_days, TE_vals, xlab="Day in Study", ylab="Relative Risk", 
     main="Relative Risk Treatment Effect of Delivering Prompt on \n Proximal App Engagement", 
     xlim=c(1,70), ylim=c(RRend-0.01, RRstart+0.01))



# Sample Size Calculation ============================
decision_times <- 70*2
avail_pattern <- rep(1, decision_times)
lower_probs <- rep(1, decision_times)*0.2
upper_probs <- rep(1, decision_times)*0.8

null_exp_full <- log((BPstart / BPend)) / log(140)
null_feats <- matrix(data = c(rep(1, decision_times), 
                              log(1:decision_times)), 
                     ncol = 2, byrow = FALSE)
null_coef <- c(log(BPstart), -null_exp_full)
plot(exp(null_feats %*% null_coef)) # sanity check


TE_exp_full <- log((RRstart / RRend)) / log(140)
TE_feats <- matrix(data = c(rep(1, decision_times), 
                            log(1:decision_times)), 
                   ncol = 2, byrow = FALSE)
TE_coef <- c(log(RRstart), -TE_exp_full)
plot(exp(TE_feats %*% TE_coef)) # sanity check

sample_size <- mrt_binary_ss(avail_pattern=avail_pattern, 
                             f_t=TE_feats, g_t=null_feats, 
                             beta=TE_coef, alpha=null_coef,
                             p_t=upper_probs,    # randomization probability
                             gamma=0.05,         # type 1 error
                             b=.2,               # power of 0.8
                             exact=FALSE)        # exact means rounds to nearest whole number
print(sample_size)




