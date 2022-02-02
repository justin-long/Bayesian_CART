#Set model parameters
alpha = 0.95
beta = 1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Moved getting number of predictor values into its own function
get_avail_predictors <- function(data, min_node_size, n_pred){
  #Initialize the available predictor vector and loop through each predictor to obtain
  #the number of available cutpoints considering the min node size
  av_pred_vec = 0
  for(i in 1:n_pred){
    
    if(class(unlist(data[i])) == "character"){
      #Get table of available values for the predictor
      value_df <- data.frame(table(copy_data$Sepal.Width))
      
      # Calcualte upper and lower cutoff points
      lower_bd <- min_node_size
      upper_bd <- sum(value_df$Freq) - min_node_size
      
      # Get subset of available cutpoints
      av_cutpoints <- subset(value_df,
                             value_df$Freq >= lower_bd & value_df$Freq <= upper_bd)
      av_pred_vec[i] <- nrow(av_cutpoints)
      
    } else if (is.factor(unlist(data[i]))){
      # Same as above
      #Get table of available values for the predictor
      value_df <- data.frame(table(copy_data$Sepal.Width))
      
      # Set upper and lower cutoff points
      lower_bd <- min_node_size
      upper_bd <- sum(value_df$Freq) - min_node_size
      
      # Get subset of available cutpoints
      av_cutpoints <- subset(value_df,
                             value_df$Freq >= lower_bd & value_df$Freq <= upper_bd)
      av_pred_vec[i] <- nrow(av_cutpoints)
      
    } else{
      #Get table of available values for the predictor
      value_df = data.frame(table(data[,i]))
      
      #Get the number of observation leq to the cutpoint
      value_df['below'] = cumsum(value_df$Freq)
      
      #Get the number of obs above the cutpoint
      value_df['above'] = nrow(data) - value_df$below
      
      #Get the subset of available cutpoints
      av_cutpoints = subset(value_df,
                            value_df$below >= min_node_size & value_df$above >= min_node_size)
      av_pred_vec[i] = nrow(av_cutpoints)
    }
  }
  
  #Assign the predictor names to the available predictor vector
  names(av_pred_vec) = colnames(data)[1:n_pred]
  
  # Return the vector
  return(av_pred_vec)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function for computing the tree prior ratio of growing/pruning the tree
tree_prior_grow_prune = function(current_tree, new_tree, data, min_node_size, n_pred){
  if(nrow(current_tree) < nrow(new_tree)){
    #This is a Grow Step
    tree_lg = new_tree #set larger tree
    tree_sm = current_tree #set smaller tree
  }else{
    #This is a Prune Step
    tree_sm = new_tree #set smaller tree
    tree_lg = current_tree #set larger tree
  }
  #Get the indexes of the larger tree that are not included in the smaller tree
  child_nodes = which(!(tree_lg$node_id %in% tree_sm$node_id))
  parent_node = unique(tree_lg[child_nodes, 'parent_node'])
  
  # #Get the terminal node assignments and apply to the data
  # temp_data = apply_splits(tree_lg, data)
  # temp_data = temp_data[which(temp_data$terminal_node %in% child_nodes),]
  #
  # # !! Moved it into a function !!
  # # Get available split points per predictor
  # av_pred_vec <- get_avail_predictors(temp_data, min_node_size, n_pred)
  #
  # #Assign the predictor names to the available predictor vector
  # names(av_pred_vec) = colnames(data)[1:n_pred]
  #
  # #Compute prule(Xk, Ck | Tree, Node)
  # #Get the probability of selecting any of the available predictors (discrete uniform prob)
  # pr_pred = 1/(sum(av_pred_vec > 0))
  #
  # #Get the probability of selecting the value used in the splitting by the parent
  # split_pred = tree_lg[which(tree_lg$node_id == parent_node), 'split_var']
  # pr_val = 1/av_pred_vec[split_pred]
  #
  # #Get p_rule
  # p_rule = pr_pred*pr_val
  
  #Get the depth of the parent and the probability it is internal and terminal
  p_depth = tree_sm[which(tree_sm$node_id == parent_node), 'depth']
  p_internal = alpha*(1 + p_depth)^(-beta)
  p_term = 1 - p_internal
  
  #Get the depth of the children and the prob each is terminal
  c_depth = p_depth + 1
  c_term = (1 - alpha*(1 + c_depth)^(-beta))^2 #Both are terminal
  
  #Get the ratio of tree priors (tree_lg/tree_sm)
  grow_prior = (p_internal*c_term)/p_term
  #grow_prior = (p_internal*p_rule*c_term)/p_term
  
  #If the update is a grow then return grow_prior. If update is prune, return 1/grow_prior
  tree_prior = ifelse(nrow(new_tree) > nrow(current_tree),grow_prior, 1/grow_prior)
  return(tree_prior)
}

#Testing the code
#Grow
tree_prior_grow_prune(current_tree = tree1, new_tree = tree2, data = data,
                      min_node_size = 2, n_pred = 4)
#Prune
tree_prior_grow_prune(current_tree = tree2, new_tree = tree1, data = data,
                      min_node_size = 2, n_pred = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tree_prior_swap = function(tree, data, min_node_size, n_pred){
  #Get the internal tree i.e. the internal nodes from the current tree
  int_tree = subset(tree, tree$terminal == 1)
  
  #Initialize the terminal node vector and p_rule vector
  term_node = rep(1, nrow(data))
  p_rule = 0
  for(i in 1:nrow(int_tree)){
    #Get the indexes of the data located in the current internal node
    h = which(term_node == int_tree$node_id[i])
    
    #Get the splitting criteria at the current internal node
    split_var = int_tree$split_var[i]
    split_pt = int_tree$split_point[i]
    
    #Get the child node id's
    left_child = int_tree$left_child[i]
    right_child = int_tree$right_child[i]
    
    #Update the terminal node vector to account for the updated location of the obs
    term_node[h] = as.numeric(ifelse(data[h, split_var] <= split_pt, left_child,
                                     right_child))
    
    #Get the available predictors and split points at this internal node
    temp_data = data[h,]
    
    # Get available split points per predictor
    av_pred_vec <- get_avail_predictors(temp_data, min_node_size, n_pred)
    
    #Compute prule(Xk, Ck | Tree, Node)
    #Get the probability of selecting any of the available predictors (discrete uniform prob)
    pr_pred = 1/(sum(av_pred_vec > 0))
    
    #Get the probability of selecting the value used in the splitting by the parent
    pr_val = 1/av_pred_vec[split_var]
    
    #Get p_rule
    p_rule[i] = pr_pred*pr_val
  }
  
  #Compute the product of p_rule -- this is the probability of all the splitting rules
  #across all internal nodes in the tree.
  tree_prior = prod(p_rule)
  return(tree_prior)
}

tree_prior_swap(tree = tree3, data = data, min_node_size = 2, n_pred = 4)
tree_prior_swap(tree = tree3_swap, data = data, min_node_size = 2, n_pred = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Compute the ratio of the tree priors P(T')/P(T)
#-- T' = proposed tree throw grow, prune, swap, or change
#-- T = current tree
prior_tree = function(current_tree, new_tree, data, min_node_size, n_pred, update){
  if(update == 'grow' | update == 'prune'){
    prior_ratio = tree_prior_grow_prune(current_tree = current_tree,
                                        new_tree = new_tree, data = data,
                                        min_node_size = min_node_size, n_pred = n_pred)
    
  } else if(update == 'swap'){
    prior_t = tree_prior_swap(tree = current_tree, data = data,
                              min_node_size = min_node_size, n_pred = n_pred)
    prior_tprime = tree_prior_swap(tree = new_tree, data = data,
                                   min_node_size = min_node_size, n_pred = n_pred)
    prior_tprime = as.numeric(ifelse(prior_tprime == Inf, 0, prior_tprime))
    prior_ratio = prior_tprime/prior_t
  } else{
    prior_ratio = 1
  }
  return(prior_ratio)
}
