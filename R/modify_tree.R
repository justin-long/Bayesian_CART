# Get proposed tree from specified operation
get_modification <- function(operation, tree, x, y){
  if(operation == 'grow'){
    proposed_tree <- grow(tree, x, y)
  }else if(operation == 'prune'){
    proposed_tree <- prune(tree)
  }else if(operation == 'change'){
    proposed_tree <- change(tree, x, y)
  }else if(operation == 'swap'){
    proposed_tree <- swap(tree)
  }
  return(proposed_tree)
}

# Combining all operations into one function
# Can set minimum number of observations
# Can also choose whether or not to enfore a min num of obs at each terminal node
modify_tree <- function(operation, tree, x, y,
                        min_nobs = 5, check_min_nobs = TRUE){
  # Note that check_min_nobs is a boolean
  # If set to false, will not check if operation is valud
  
  # Combine x, y
  data <- cbind(x,y)
  
  if(!check_min_nobs){
    # Get new tree
    proposed_tree <- get_modification(operation, tree, x, y)
    
    # Print warning
    print('CAUTION: not checking terminal node size')
    
    # Return tree
    return(proposed_tree)
  } else{
    # Set conditional to F to enter loop
    min_nobs_condition <-  FALSE
    
    # Need to set max iter so don't enter an infinite loop
    max_iter <- 1e3
    iter <- 0
    # Loop until split is found, or max attemps
    while(!min_nobs_condition & iter < max_iter & check_min_nobs){
      # Propose a new tree
      proposed_tree <- get_modification(operation, tree, x, y)
      
      # Check condition
      min_nobs_condition <- check_nobs_terminal_nodes(proposed_tree,
                                                      data, min_nobs)
      # Iterate
      iter <- iter+1
    }
    
    if(iter >= max_iter){
      print(paste(operation, ' unsuccessful'))
      
      # Set conditional to false since operation failed
      operation_condition <- FALSE
      
      # Return a list with current tree and conditional
      return_list <- list(tree = tree, conditional = operation_condition)
      return(return_list)
    }else{
      print(paste(operation ,' attempts: ', iter))
      
      # Set conditional to true since operation was successful
      operation_condition <- TRUE
      
      # Return a list with proposed tree and conditional
      return_list <- list(tree = proposed_tree, conditional = operation_condition)
      return(return_list)
    }
  }
}