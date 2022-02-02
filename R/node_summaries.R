## Node Summary Functions
# Change y column to be numeric for testing
y <- rep(c(1,2), nrow(x) / 2)

# Get terminal node size
summarize_nodes <- function(tree, x, y){
  # Combine data
  data <- cbind(x,y)
  
  # Apply tree structure
  data <- apply_splits(tree, data)
  
  # Apply summary functions
  node_mean <- as.data.frame(tapply(y, data['terminal_node'], mean))
  node_var <- as.data.frame(tapply(y, data['terminal_node'], var))
  node_size <- as.data.frame(table(data['terminal_node']))
  
  # Combine into one data frame
  node_summary <- cbind(node_size, node_mean, node_var)
  colnames(node_summary) <- c('terminal_node_id', 'node_size',
                              'node_mean', 'node_var')
  
  # Return
  return(node_summary)
}

# Function to calculate si and ti for each node i
calc_si_ti <- function(node_summary, a, mu_bar){
  # calculate si
  node_summary['si'] <- (node_summary['node_size'] - 1) * node_summary['node_var']
  
  # Calculate ti
  first_part <- (node_summary['node_size'] * a) / (node_summary['node_size'] + a)
  second_part <- (node_summary['node_mean'] - mu_bar)^2
  node_summary['ti'] <- first_part * second_part
  
  return(node_summary)
}