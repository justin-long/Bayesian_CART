## Helper Functions

# Function to add children for grow
add_children <- function(num_nodes, parent_node, parent_depth){
  # Set child depth
  child_depth <- parent_depth + 1
  
  # Create left and right children
  left  <- c(num_nodes + 1, parent_node, 0, 0, -1, 0, 0, child_depth)
  right <- c(num_nodes + 2, parent_node, 0, 0, -1, 0, 0, child_depth)
  return(rbind(left, right))
}

# Need to write own sample function
# If there's only one number to sample from, sample function will sample
# from 1:that value
# This avoids that
new_sampler <- function(x, ...){
  x[sample(length(x), ...)]
}

# Check number of observations at terminal nodes
check_nobs_terminal_nodes <- function(df, data, min_nobs){
  # Apply the splits
  applied_tree <- apply_splits(df, data)
  
  # Get list of all terminal nodes
  terminal_node_ids <- df[df['terminal'] == -1, 'node_id']
  
  # Force in all terminal nodes
  check_table <- table(factor(applied_tree$terminal_node,
                              levels = terminal_node_ids))
  # Convert to vector
  nobs_per_node <- as.vector(check_table)
  
  # Evaluate conditional
  cond <- all(nobs_per_node >= min_nobs)
  return(cond)
}


# Return number of observations per terminal node given table structure
get_nobs_terminal_nodes <- function(df, data){
  # Apply the splits
  applied_tree <- apply_splits(df, data)
  
  # Get list of all terminal nodes
  terminal_node_ids <- df[df['terminal'] == -1, 'node_id']
  
  check_table <- table(factor(applied_tree$terminal_node,
                              levels = terminal_node_ids))
  nobs_per_node <- as.vector(check_table)
  
  return(check_table)
}


# Get rows that have splitting criteria in them, i.e. internal nodes
get_splitting_rows <- function(df){
  rows <- df[df$split_var != 0, ]
  return(rows)
}


# Apply tree to data
# Return dataframe with column indicating terminal node
apply_splits <- function(tree, data){
  df <- tree
  #Check if df is a root node
  if(nrow(df) == 1){
    data['terminal_node'] <- rep(1, nrow(data))
    return(data)
  }
  
  # Add column for terminal node into data
  data['terminal_node'] <- rep(1, nrow(data))
  
  # Get splitting rows
  splitting_rows <- get_splitting_rows(df)
  
  for (i in 1:nrow(splitting_rows)) {
    
    # Get characteristics of split
    split_node_id <- splitting_rows[i,'node_id']
    split_var   <- splitting_rows[i,'split_var']
    split_point <- splitting_rows[i,'split_point']
    left_child  <- splitting_rows[i, 'left_child']
    right_child <- splitting_rows[i, 'right_child']
    
    # Need to select data that can be split
    rows_to_update <- data['terminal_node'] == split_node_id
    
    # Get new terminal nodes based off split
    new_terminal_nodes <- as.numeric(ifelse(data[split_var] <= split_point,
                                            left_child, right_child))
    # Add to data
    data[rows_to_update, 'terminal_node'] <- new_terminal_nodes[rows_to_update]
  }
  return(data)
}