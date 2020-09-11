# Introduction to Bayesian Inference in Practice - Transmitting Science
# Elildo Carvalho Jr, 2020-09-10

# MCMC algorithm (Metropolis-Hastings)
# Body sizes of unburned vs. burned
# With hyper-priors!!!!

# ---- Read data ----
#burned = c(7.3529412, 2.9411765, -1.4347202,  0.0000000, 14.4642857, 0.0000000, 0.9540117, -0.8477633, 16.4285714,  0.0000000,  7.6785714)
#unburned = c(-6.097561, -31.645570, -7.847296,  11.823966,  -3.658537)



# initialize the output file (provide path to your target directory)
setwd("C:/Users/ICMBio/R/fire-amazon-vertebrates/results")
logfile <- "r_mcmc_samples_unburned_vs_burned_hyper_prior.txt"

# define the column names
cat(c("iteration","posterior","likelihood","prior", "rateSig_hp", "mu_burned", "mu_unburned", "sig_burned", "sig_unburned", "difference", "\n"), file=logfile, sep="\t")

# ---- MCMC settings ----

n_iter <- 150000
sampling_freq <- 100 # write to file every n iterations
print_freq <- 5000 # print to screen every n iterations

# window sizes for updating function
window_size_update_mu <- 10
window_size_update_sig <- 5


# --- functions ----

# pdf uniform
log_pdf_uniform <- function(x,minMu,maxMu) {
  if(x > minMu & x < maxMu) {
    return(log(1)-log(maxMu-minMu))
  } else {
    return(-Inf)
  }
}

# pdf gamma
log_pdf_gamma <- function(x,shapeSig,rateSig){
  loglik <- ((shapeSig*log(rateSig)) - log(gamma(shapeSig))) + ((shapeSig-1)*log(x)) + ((-rateSig*x)*log(exp(1)))
  return(loglik)
}

# likelihood
log_pdf_normal <- function(x,mu,std){
  p <- -log(sqrt(2*pi*std^2)) - ((x-mu)^2)/(2*std^2)
  return(p)
}

# Proposal function (sliding window)
update_uniform <- function(parameter, windowsize){
  lower <- parameter-(windowsize/2)
  upper <- parameter+(windowsize/2)
  new_parameter <- runif(1, lower, upper)
  #new_parameter <- abs(new_parameter) # we can use abs because body mass is always positive
  #log_hastings_ratio <- 0
  hastings_ratio <- 1
  return(c(new_parameter, hastings_ratio))
}

# proposal function (multiplier)
multiplier_proposal <- function(i,d=1.5){ # d must be > 1!!
  u <- runif(1)
  l <- 2*log(d)
  m <- exp(l*(u-.5))
  new_x <- i * m
  hastings_ratio <- log(m)
  return( c(new_x, hastings_ratio) )
}


# ---- Prior parameters ----
# boundaries of the uniform prior on mu
minMu <- -10
maxMu <- 10

# shape and rate parameters of gamma prior on sigma
shapeSig <- 1 # i.e., this pins the gamma distribution to an exponential form 
#rateSig <- rexp(1,1) # rateSig will be a parameter to be estimated


# ----------------------------------
# ---- Current parameter values ----
# ----------------------------------

# rateSig
current_rateSig <- rexp(1,1)

# mu and sigma burned
current_mu_burned <- rnorm(1, mean(burned))
current_sigma_burned <- rgamma(1, shapeSig, current_rateSig)

# mu and sigma unburned
current_mu_unburned <- rnorm(1, mean(unburned))
current_sigma_unburned <- rgamma(1, shapeSig, current_rateSig)

# current likelihood and priors
current_lik <- sum(log_pdf_normal(burned,current_mu_burned,current_sigma_burned)) + sum(log_pdf_normal(unburned,current_mu_unburned,current_sigma_unburned))
current_prior_burned <- log_pdf_uniform(current_mu_burned, minMu, maxMu) + log_pdf_gamma(current_sigma_burned, shapeSig, current_rateSig)
current_prior_unburned <- log_pdf_uniform(current_mu_unburned, minMu, maxMu) + log_pdf_gamma(current_sigma_unburned, shapeSig, current_rateSig)
current_prior_hp <- log_pdf_gamma(current_sigma_unburned, shapeSig, current_rateSig) + log_pdf_gamma(current_sigma_burned, shapeSig, current_rateSig)
current_prior <- current_prior_burned + current_prior_unburned + current_prior_hp
current_posterior <- current_lik + current_prior


# ----- start loop -----
for (iteration in 1:n_iter){
  
  # --------------------------------------
  # ---- Propose new parameter values ----
  # --------------------------------------
  
  # new rateSig (hyperparameter)
  proposal_rateSig <- multiplier_proposal(current_rateSig, 1.2)
  new_rateSig <- abs(proposal_rateSig[1]) # only positive values
  hastings_ratio_rateSig <- proposal_rateSig[2]
  
  # new mu_burned
  proposal_mu_burned <- update_uniform(current_mu_burned, window_size_update_mu)
  new_mu_burned <- proposal_mu_burned[1]
  hastings_ratio_mu_burned <- proposal_mu_burned[2]
  
  # new mu_unburned
  proposal_mu_unburned <- update_uniform(current_mu_unburned, window_size_update_mu)
  new_mu_unburned <- proposal_mu_unburned[1]
  hastings_ratio_mu_unburned <- proposal_mu_unburned[2]
  
  # new sigma_burned
  proposal_sigma_burned <- multiplier_proposal(current_sigma_burned, 1.2)
  new_sigma_burned <- abs(proposal_sigma_burned[1]) # sigma cannot be negative so we use reflection at the boundary (0)
  hastings_ratio_sigma_burned <- proposal_sigma_burned[2]
  
  # new sigma_unburned
  proposal_sigma_unburned <- multiplier_proposal(current_sigma_unburned, 3)
  new_sigma_unburned <- abs(proposal_sigma_unburned[1]) # sigma cannot be negative so we use reflection at the boundary (0)
  hastings_ratio_sigma_unburned <- proposal_sigma_unburned[2]
  
  # get overall hastings ratio of new parameter proposals
  hastings_ratio <- hastings_ratio_rateSig + hastings_ratio_mu_burned + hastings_ratio_mu_unburned + hastings_ratio_sigma_burned + hastings_ratio_sigma_unburned
  
  # Calculate posterior for the new parameters
  # current likelihood and priors
  new_lik <- sum(log_pdf_normal(burned,new_mu_burned,new_sigma_burned)) + sum(log_pdf_normal(unburned,new_mu_unburned,new_sigma_unburned))
  new_prior_burned <- log_pdf_uniform(new_mu_burned, minMu, maxMu) + log_pdf_gamma(new_sigma_burned, shapeSig, new_rateSig)
  new_prior_unburned <- log_pdf_uniform(new_mu_unburned, minMu, maxMu) + log_pdf_gamma(new_sigma_unburned, shapeSig, new_rateSig)
  new_prior_hp <- log_pdf_gamma(new_sigma_unburned, shapeSig, new_rateSig) + log_pdf_gamma(new_sigma_burned, shapeSig, new_rateSig)
  new_prior <- new_prior_burned + new_prior_unburned + new_prior_hp
  new_posterior <- new_lik + new_prior
  
  
  # --------------------------------------
  # ---- Calculate acceptance ratio ----
  # --------------------------------------
  
  #posterior_ratio = exp(new_posterior)/exp(current_posterior) # exp because posteriors are in log scale
  posterior_ratio = new_posterior-current_posterior # the ratio becomes a subtraction in log space
  acceptance_probability = posterior_ratio + log(hastings_ratio) # log of hastings since we are using the log of the posterior rate
  
  
  # -------------------------------------------
  # ---- Accept or reject parameter values ----
  # -------------------------------------------
  
  # draw random number between zero and one
  u <- runif(1)
  u <- log(u) # since we are using logs
  
  # acceptance condition
  if (u <= acceptance_probability){
    current_rateSig <- new_rateSig
    current_mu_burned <- new_mu_burned
    current_mu_unburned <- new_mu_unburned
    current_sigma_burned <- new_sigma_burned
    current_sigma_unburned <- new_sigma_unburned
    current_lik <- new_lik
    current_prior <- new_prior
    current_posterior <- new_posterior
    difference <- current_mu_burned-current_mu_unburned
  }
  
  # print to screen if iteration is a multiple of print_freq
  if (iteration %% print_freq == 0){
    print(c(iteration,current_lik, current_prior, current_rateSig, current_mu_burned, current_mu_unburned, current_sigma_burned, current_sigma_unburned, difference))
  }
  
  # save to file if iteration is a multiple of sampling_freq
  if (iteration %% sampling_freq == 0){
    cat(c(iteration, current_posterior, current_lik, current_prior, current_rateSig, current_mu_burned, current_mu_unburned, current_sigma_burned, current_sigma_unburned, difference, "\n"), sep="\t",file=logfile,append=T)
  }
}  # end loop