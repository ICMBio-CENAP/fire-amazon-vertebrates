# Introduction to Bayesian Inference in Practice - Transmitting Science
# Elildo Carvalho Jr, 2020-09-10

# Photo capture rates for burned and unburned sites
# With hyper-priors!!!!

# ---- Read data ----
dataBurned = c(7.3529412, 2.9411765, -1.4347202,  0.0000000, 14.4642857, 0.0000000, 0.9540117, -0.8477633, 16.4285714,  0.0000000,  7.6785714)
dataUnburned = c(-6.097561, -31.645570, -7.847296,  11.823966,  -3.658537)


#It will be easiest to look at your results if you log the difference of mu_panda and mu_bear
#into your log file together with all the other logged parameters 
#(you can simply add another column called mu_p-mu_b or whatever name you prefer)


# initialize the output file (provide path to your target directory)
setwd("C:/Users/ICMBio/R/fire-amazon-vertebrates/results")
logfile <- "r_mcmc_samples_unburn_vs_dataBurned_hyper_prior.txt"

# define the column names
cat(c("iteration","posterior","likelihood","prior","mu_burn", "mu_unburn", "sig_burn", "sig_unburn", "difference", "\n"), file=logfile, sep="\t")

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
  new_parameter <- abs(new_parameter) # we can use abs because body mass is always positive
  #log_hastings_ratio <- 0
  hastings_ratio <- 1
  return(c(new_parameter, hastings_ratio))
}


# ---- Prior parameters ----
# boundaries of the uniform prior on mu
minMu <- -40
maxMu <- 40

# shape and rate parameters of gamma prior on sigma
#shapeSig <- 2
#rateSig <- 0.01
#hyper-parameter is exponential (= gamma with rate 1) with shape exp(1)
shapeSig <- 1 
rateSig <- rexp(1,1)


# ----------------------------------
# ---- Current parameter values ----
# ----------------------------------

# mu and sigma burn
current_mu_burn <- rnorm(1, mean(dataBurned))
current_sigma_burn <- rgamma(1, shapeSig, rateSig)

# mu and sigma unburn
current_mu_unburn <- rnorm(1, mean(dataUnburned))
current_sigma_unburn <- rgamma(1, shapeSig, rateSig)

# current likelihood and priors
current_lik <- sum(log_pdf_normal(dataBurned,current_mu_burn,current_sigma_burn)) + sum(log_pdf_normal(dataUnburned,current_mu_unburn,current_sigma_unburn))
current_prior_burn <- log_pdf_uniform(current_mu_burn, minMu, maxMu) + log_pdf_gamma(current_sigma_burn, shapeSig, rateSig)
current_prior_unburn <- log_pdf_uniform(current_mu_unburn, minMu, maxMu) + log_pdf_gamma(current_sigma_unburn, shapeSig, rateSig)
current_prior <- current_prior_burn + current_prior_unburn
current_posterior <- current_lik + current_prior

# ----- start loop -----
for (iteration in 1:n_iter){
  
  
  # --------------------------------------
  # ---- Propose new parameter values ----
  # --------------------------------------
  
  # new mu_burn
  proposal_mu_burn <- update_uniform(current_mu_burn, window_size_update_mu)
  new_mu_burn <- proposal_mu_burn[1]
  hastings_ratio_mu_burn <- proposal_mu_burn[2]
  
  # new mu_unburn
  proposal_mu_unburn <- update_uniform(current_mu_unburn, window_size_update_mu)
  new_mu_unburn <- proposal_mu_unburn[1]
  hastings_ratio_mu_unburn <- proposal_mu_unburn[2]
  
  # new sig_burn
  proposal_sigma_burn <- update_uniform(current_sigma_burn, 3)
  new_sigma_burn <- abs(proposal_sigma_burn[1]) # sigma cannot be negative so we use reflection at the boundary (0)
  hastings_ratio_sigma_burn <- proposal_sigma_burn[2]
  
  # new sig_unburn
  proposal_sigma_unburn <- update_uniform(current_sigma_unburn, 3)
  new_sigma_unburn <- abs(proposal_sigma_unburn[1]) # sigma cannot be negative so we use reflection at the boundary (0)
  hastings_ratio_sigma_unburn <- proposal_sigma_unburn[2]
  
  # get overall hastings ratio of new parameter proposals
  hastings_ratio <- 1
  
  # Calculate posterior for the new parameters
  new_lik <- sum(log_pdf_normal(dataBurned,new_mu_burn,new_sigma_burn)) + sum(log_pdf_normal(dataUnburned,new_mu_unburn,new_sigma_unburn))
  new_prior_burn <- log_pdf_uniform(new_mu_burn, minMu, maxMu) + log_pdf_gamma(new_sigma_burn, shapeSig, rateSig)
  new_prior_unburn <- log_pdf_uniform(new_mu_unburn, minMu, maxMu) + log_pdf_gamma(new_sigma_unburn, shapeSig, rateSig)
  new_prior <- new_prior_burn + new_prior_unburn
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
    current_mu_burn <- new_mu_burn
    current_mu_unburn <- new_mu_unburn
    current_sigma_burn <- new_sigma_burn
    current_sigma_unburn <- new_sigma_unburn
    current_lik <- new_lik
    current_prior <- new_prior
    current_posterior <- new_posterior
    difference <- current_mu_burn-current_mu_unburn
  }
  
  # print to screen if iteration is a multiple of print_freq
  if (iteration %% print_freq == 0){
    print(c(iteration,current_lik, current_prior, current_mu_burn, current_mu_unburn, current_sigma_burn, current_sigma_unburn, difference))
  }
  
  # save to file if iteration is a multiple of sampling_freq
  if (iteration %% sampling_freq == 0){
    cat(c(iteration, current_posterior, current_lik, current_prior, current_mu_burn, current_mu_unburn, current_sigma_burn, current_sigma_unburn, difference, "\n"), sep="\t",file=logfile,append=T)
  }
}  # end loop

# use tracer to check posteriors and convergence
# https://github.com/beast-dev/tracer/releases/tag/v1.7.1

