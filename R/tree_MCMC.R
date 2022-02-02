#Set MCMC parameters
n_chains = 1 #Number of separate chains to run, each restarting at the root node
N = 10 #Number of MCMC sets per chain
burn = 100 #Number of Burn in steps
a = 1
mu_bar = 1
v = 3
lambda = 1
mns = 1
n_pred = 4

#Initialize the data frames required to store MCMC results
#--(Write fit_values as a n_chains dimensional list and rbind at each step?)
sig2_matrix = matrix(NA, nrow = N, ncol = n_chains)
fit_values = array(NA, dim = c(N, length(y), n_chains))
#pred_values = array(NA, dim = c(N, length(y_test), n_chains)) #Use for test data


get_tree_proposal <- function(tree, x, y, min_nobs){
  move_possible <- FALSE
  max_iter <- 1e3
  iter <- 0
  while(iter < max_iter & !move_possible){
    #Draw proposed step and get df with the operation, the move prob, and the reverse prob
    move_df = mh_proposal(tree)
    
    #Get proposed tree and apply the splits
    tree_proposal = modify_tree(operation = move_df$operation, tree = tree_list$tree, x, y, min_nobs = min_nobs)
    # Check if move is valid
    move_possible <- tree_proposal$conditional
  }
  if(iter >= max_iter){
    print('Could not final a valid move in MH proposal step')
  }
  else{
    return(tree_proposal$tree)
  }
}

temp_list <- list()
move_list <- list()

#Set initial values for the parameters
sig2_matrix[1,] = rep(1, n_chains)
k = 1
for(i in 2:N){
  #Initialize mu_update
  mu_update = 0
  
  #Draw proposed step and get df with the operation, the move prob, and the reverse prob
  move_df = mh_proposal(tree_list$tree)
  move_list[[i]] <- move_df$operation
  
  
  #Get proposed tree and apply the splits
  tree_proposal = modify_tree(operation = move_df$operation, tree = tree_list$tree, x, y, min_nobs = mns)
  tree_proposal <- tree_proposal$tree
  temp_list[[i]] <- tree_proposal
  # Moved the above two function calls into this function
  # I've left the two above just to double check that everything is correct
  # The function is above just to check, we'll move it after we're sure it's working properly
  # May not even need this? Was able to run without it
  #tree_proposal <- get_tree_proposal(tree_list$tree, x, y, min_nobs = mns)
  x_proposal = apply_splits(tree_proposal, x)
  
  #Get summary statistics for the terminal nodes
  ns_proposal = summarize_nodes(tree_proposal, x_proposal, y)
  ns_proposal = calc_si_ti(ns_proposal, a = a, mu_bar = mu_bar)
  
  #Combine the tree proposal with the summary stats into a list
  tree_prop_list = list(tree = tree_proposal, node_summary = ns_proposal, data = x_proposal)
  
  #Get the tree prior ratio
  tree_ratio = prior_tree(tree_list$tree, tree_proposal, x, min_node_size = mns,
                          n_pred, update = move_df$operation)
  
  #Get likelihoods
  c_lhood = lhood_y_xt(tree_list$node_summary, v, lambda, a)
  n_lhood = lhood_y_xt(tree_prop_list$node_summary, v, lambda, a)
  
  #Update the proposal distribution for the grow/prune cases (accounts for reversible jump)
  update_proposal = mh_update_proposal(current_tree = tree_list$tree, new_tree = tree_prop_list$tree,
                                       move_pr = move_df$move_prob, rev_pr = move_df$rev_prob,
                                       operation = move_df$operation)
  
  #Perform the Metropolis Hastings Step
  tree_list = mh_step(n_tree = tree_prop_list, c_tree = tree_list, n_lhood = n_lhood, c_lhood = c_lhood,
                      n_proposal = update_proposal$rev_prob, c_proposal = update_proposal$move_prob,
                      prior_ratio = tree_ratio)
  
  #Sample Mu_i for each of the terminal node parameters (requires loop or apply function)
  b = nrow(tree_list$node_summary)
  for(j in 1:b){
    mu_update[j] = post_mu_j(tree_list$node_summary, a, ubar = mu_bar, sig2 = sig2_matrix[i-1,k])
  }
  
  #Assign the terminal node parameters to the corresponding observations
  mu_update = data.frame(terminal_node = tree_list$node_summary$terminal_node_id,
                         mu = mu_update)
  mu_update$terminal_node = as.numeric(levels(mu_update$terminal_node))[mu_update$terminal_node]
  new_fits = tree_list$data %>% left_join(mu_update, by = 'terminal_node') %>%
    select(mu)
  fit_values[i,,k] = as.matrix(new_fits, nrow = 1)
  
  #Sample sigma2
  sig2_matrix[i,k] = post_sig2(y, tree_list$data$terminal_node, tree_list$node_summary,
                               a, v, lambda, ubar = mu_bar)
}
