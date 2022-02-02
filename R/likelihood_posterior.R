#Posterior Distributions and Likelihood
#Sigma2 posterior -- inverse gamma distribution
#--Requires: y vector, tnode_vec = vector of terminal nodes from apply_split func,
#--- node_stats from summarize nodes func, & hyper parameters a, v, lambda, ubar
post_sig2 = function(y, tnode_vec, node_summary, a, v, lambda, ubar){
  n = length(y)
  b = nrow(node_summary)
  
  #Cast terminal node in tnode_vec as a character in a dataframe
  tnode_vec = data.frame(as.character(tnode_vec))
  colnames(tnode_vec) = 'terminal_node_id'
  
  #Cast the terminal node id in node_summary as a character
  node_summary$terminal_node_id = as.character(node_summary$terminal_node_id)
  
  #Join node_summary with tnode_vec
  node_stats = tnode_vec %>% left_join(node_summary, by = 'terminal_node_id')
  
  #Merge with Y -- response data
  node_stats = data.frame(node_stats, y)
  
  #Get the differences (Yij - mu_i)^2 and (mu_i - ubar)^2 named diff1 and diff2, respectively
  diff1 = (node_stats$y - node_stats$node_mean)^2
  diff2 = (node_summary$node_mean - ubar)^2
  
  #Sum over the observations in the above vectors
  sum_diff1 = sum(diff1)
  sum_diff2 = sum(diff2)
  
  #Get shape and rate parameters
  shape = 0.5*(b + n + v)
  rate = 0.5*(sum_diff1 + a*sum_diff2 + v*lambda)
  
  #Draw a new sig2
  new_sig2 = rinvgamma(1, shape = shape, rate = rate)
  return(new_sig2)
}

post_sig2(y, tnode_vec, node_summary, a = 3, v = 3, lambda = 1, ubar = 1)

#Mu_j posterior
#Requires: summarize node function output, hyperparameters a & ubar,
#--- Most recent draw of sig2, and the index j for the jth terminal node
post_mu_j = function(node_summary, a, ubar, sig2, j){
  node_mean = node_summary$node_mean[j]
  node_n = node_summary$node_size[j]
  
  #Get posterior mean and variance
  mu_mean = (node_mean*node_n + ubar*a)/(node_n + a)
  mu_var = sig2/(node_n + a)
  
  #Draw a new mu_j
  new_mu = rnorm(1, mean = mu_mean, sd = sqrt(mu_var))
  return(new_mu)
}

node_sum_st = calc_si_ti(node_summary, a = 1, mu_bar = 1)

post_mu_j(node_summary, a=3, ubar = 1, sig2 = 2, j = 1)
post_mu_j(node_sum_st, a=3, ubar = 1, sig2 = 2, j = 1)

#Likelihood after integrating out parameters Y|X,T (See Section 4.1)
#Requires: Output of calc_si_ti and hyper parameters of v, lambda, and a
lhood_y_xt = function(node_summary, v, lambda, a){
  b = nrow(node_summary)
  n = sum(node_summary$node_size)
  
  #Get si + ti and sum over i
  sum_st = sum(node_summary$si + node_summary$ti)
  
  #Get product of sqrt(ni + a)
  prod_na = prod(sqrt(node_summary$node_size + a))
  #Get numerator and denominator
  num = a^(b/2)
  denom = prod_na*(sum_st = v*lambda)^((n + v)/2)
  
  #Return the density
  den = num/denom
  return(den)
}

lhood_y_xt(node_sum_st, 1,1,1)
