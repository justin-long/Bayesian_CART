## Main tree modifier functions

# Grow function
# Will randomly split a terminal node and add two children
grow <- function(df, x, y){
  # Need to select which terminal node to split
  terminal_rows <- df[df['terminal'] == -1, ]
  terminal_ids <- unlist(terminal_rows['node_id'])
  
  # Select node based off id
  split_node <- as.integer(sample(terminal_ids, 1))
  
  # Generate some splitting rule based on predictors
  # Right now, randomly select a column in x
  rand_col_idx <- sample(1:ncol(x), 1)
  split_col_name <- names(x)[rand_col_idx]
  
  # Column to split on
  split_col <- x[, rand_col_idx]
  
  # Split criteria
  rand_split <- sample(split_col, 1)
  
  # Modify details of parent node
  max_node <- max(df['node_id'])
  num_nodes <- nrow(df)
  df[df['node_id'] == split_node, 'left_child']  <- max_node + 1
  df[df['node_id'] == split_node, 'right_child'] <- max_node + 2
  df[df['node_id'] == split_node, 'terminal']    <- 1
  df[df['node_id'] == split_node, 'split_var']   <- split_col_name
  df[df['node_id'] == split_node, 'split_point'] <- rand_split
  
  # Get depth
  depth <- df[df['node_id'] == split_node, 'depth']
  
  # Add children rows
  child_rows_idx <- c(num_nodes+1, num_nodes+2)
  df[child_rows_idx, ] <- add_children(max_node, split_node, depth)
  
  return(df)
}



# Prune function
# Will collapse 2 terminal nodes into parent node
prune <- function(df){
  # Select parent of 2 terminal nodes
  # first get terminal rows
  terminal_rows <- df[df['terminal'] == -1, ]
  terminal_ids <- unlist(terminal_rows['node_id'])
  
  # Loop until selected child node has a parent with two terminal nodes
  left_terminal_cond  <- 1
  right_terminal_cond <- 1
  
  while(!(left_terminal_cond == -1 & right_terminal_cond == -1)){
    # Select node based off id
    terminal_node <- as.integer(sample(terminal_ids, 1))
    
    # Now, get parent from selected node
    parent_node <- df[df['node_id'] == terminal_node, 'parent_node']
    
    # Children, i.e. rows  to remove
    left_child_id  <- df[df['node_id'] == parent_node, 'left_child']
    right_child_id <- df[df['node_id'] == parent_node, 'right_child']
    
    # Need to make sure that both left and right children are terminal
    left_terminal_cond  <- df[df['node_id'] == left_child_id, 'terminal']
    right_terminal_cond <- df[df['node_id'] == right_child_id, 'terminal']
  }
  
  # Drop rows
  df <- df[!(df['node_id'] == left_child_id | df['node_id'] == right_child_id) , ]
  
  # Modify details of parent node
  num_nodes <- nrow(df)
  df[df['node_id'] == parent_node, 'left_child']  <- 0
  df[df['node_id'] == parent_node, 'right_child'] <- 0
  df[df['node_id'] == parent_node, 'terminal']    <- -1
  df[df['node_id'] == parent_node, 'split_var']   <- 0
  df[df['node_id'] == parent_node, 'split_point'] <- 0
  
  return(df)
}

# Change function
# Will select an internal node, and change its splitting rule
change <- function(df, x, y){
  # Select internal node, i.e. terminal = 1
  internal_rows <- df[df['terminal'] == 1, ]
  internal_ids <- unlist(internal_rows['node_id'])
  
  # Randomly select internal node
  change_node <- as.integer(new_sampler(internal_ids, 1))
  
  # Generate some splitting rule based on predictors
  # Right now, randomly select a column in x
  rand_col_idx <- sample(1:ncol(x), 1)
  split_col_name <- names(x)[rand_col_idx]
  
  # Column to split on
  split_col <- x[, rand_col_idx]
  
  # Split criteria
  rand_split <- sample(split_col, 1)
  
  # Modify details of change node
  num_nodes <- nrow(df)
  df[df['node_id'] == change_node, 'split_var']   <- split_col_name
  df[df['node_id'] == change_node, 'split_point'] <- rand_split
  
  return(df)
}



# Swap Function
# Will select an internal node, and swap its splitting rule with its parent
swap <- function(df){
  # Select all left child nodes (or right, doesn't matter)
  swap_rows <- df[df$parent_node != 0 & df$terminal == 1, ]
  swap_ids <- unlist(swap_rows['node_id'])
  
  # Randomly select internal node
  swap_child_id <- as.integer(new_sampler(swap_ids, 1))
  
  # Get parent node id
  swap_parent_id <- df[df$node_id == swap_child_id, 'parent_node']
  
  # Need to compare splitting rules
  # If both children have same rule, need to swap both
  left_child_id  <- df[df$node_id == swap_parent_id, 'left_child']
  right_child_id <- df[df$node_id == swap_parent_id, 'right_child']
  
  left_child_split_rule <-  df[df$node_id ==left_child_id,
                               c('split_var', 'split_point')]
  right_child_split_rule <- df[df$node_id ==right_child_id,
                               c('split_var', 'split_point')]
  
  # Check if match or not
  # True if match
  children_same <- all(left_child_split_rule ==  right_child_split_rule)
  
  # If the same
  if(children_same){
    # Get rows
    parent_row <- df[df$node_id == swap_parent_id, ]
    left_child_row <- df[df$node_id == left_child_id, ]
    right_child_row <- df[df$node_id == right_child_id, ]
    
    # Swap the splitting rules
    # swap left child
    df[df$node_id == left_child_id, 'split_var'] <- parent_row$split_var
    df[df$node_id == left_child_id, 'split_point'] <- parent_row$split_point
    
    # swap right child
    df[df$node_id == right_child_id, 'split_var'] <- parent_row$split_var
    df[df$node_id == right_child_id, 'split_point'] <- parent_row$split_point
    
    # swap parent with left child, but should be the same
    df[df$node_id == swap_parent_id, 'split_var'] <- left_child_row$split_var
    df[df$node_id == swap_parent_id, 'split_point'] <- left_child_row$split_point
    
    print('children had same rules')
    print(paste('swapped nodes ', swap_child_id, ' and ', swap_parent_id))
    
    return(df)
    
  } else{
    # Get rows
    child_row  <- df[df$node_id == swap_child_id, ]
    parent_row <- df[df$node_id == swap_parent_id, ]
    
    # Swap the splitting rules
    # make sure to add error check, hold on
    df[df$node_id == swap_child_id, 'split_var'] <- parent_row$split_var
    df[df$node_id == swap_child_id, 'split_point'] <- parent_row$split_point
    
    df[df$node_id == swap_parent_id, 'split_var'] <- child_row$split_var
    df[df$node_id == swap_parent_id, 'split_point'] <- child_row$split_point
    
    print('children had different rules')
    print(paste('swapped nodes ', swap_child_id, ' and ', swap_parent_id))
    
    return(df)
  }
}
