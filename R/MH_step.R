#Metropolis Hastings Steps
#Perform the Metropolis Hastings step to select a tree (accept/reject proposal)
#Required:
#-- n_tree = proposed tree, list of the tree df and node summary with si and ti
#-- c_tree = current tree, list of the tree df and node summary with si and ti
#-- n_lhood = likelihood of the data given the proposed tree (output of lhood_y_xt)
#-- c_lhood = likelihood of the data given the current tree (output of lhood_y_xt)
#-- n_proposal = proposal of moving from T' to T, denoted Q(T',T) in the paper
#-- c_proposal = proposal of moving from T to T', denoted Q(T,T') in the paper
#-- prior_ratio = ratio of tree priors P(T')/P(T)

#Output = List of the accepted tree and the summary statistics
mh_step = function(n_tree, c_tree, n_lhood, c_lhood, n_proposal, c_proposal, prior_ratio){
  #Get the acceptance ratio
  r = (n_lhood*n_proposal)/(c_proposal*c_lhood)*prior_ratio
  alpha = min(1, r)
  
  #Perform acceptance/rejection step
  u = runif(1,0,1)
  if(u <= alpha){
    #Accept the proposed tree
    tree = n_tree
  }else{
    #Reject the propsed tree
    tree = c_tree
  }
  return(tree)
}

#Test of function with just the trees and not the list
mh_step(tree2, tree1, 0.00001, 0.0000005, 0.4, 0.1, 2)

#Create Proposal Distribution
#Requires: Tree dataframe
mh_proposal = function(tree){
  if(nrow(tree) == 1){
    #If root node, the only possible operation is grow
    move = data.frame(operation = 'grow', move_prob = 1, rev_prob = 0.25)
  } else if(nrow(tree) == 3){
    #Define possible moves
    move_list = c('grow', 'prune', 'change')
    
    #Get the probability of selecting a move, along with the probability of the reverse move
    move_prob = c(0.25, 0.25, 0.4)
    rev_prob = c(0.25, 0.25, 0.4)
    
    #Randomly sample a move
    m = sample(1:3, size = 1, prob = move_prob)
    
    #Get the operation name, the move probability, and the reverse probability
    move = data.frame(operation = move_list[m], move_prob = move_prob[m],
                      rev_prob = rev_prob[m])
    
  }
  else{
    
    #Define possible moves
    move_list = c('grow', 'prune', 'change', 'swap')
    
    #Get the probability of selecting a move, along with the probability of the reverse move
    move_prob = c(0.25, 0.25, 0.4, 0.1)
    rev_prob = c(0.25, 0.25, 0.4, 0.1)
    
    #Randomly sample a move
    m = sample(1:4, size = 1, prob = move_prob)
    
    #Get the operation name, the move probability, and the reverse probability
    move = data.frame(operation = move_list[m], move_prob = move_prob[m],
                      rev_prob = rev_prob[m])
  }
  return(move)
}


#Update the proposal distribution generated from mh_proposal in grow/prue
#Account for the reversible jump conditions
#Requires: new and current trees, the move and reverse move probabilites
mh_update_proposal = function(current_tree, new_tree, move_pr, rev_pr, operation){
  if(operation == 'grow'){
    #Move: prob(Birth at node the given node given T) = 1/#Terminal Nodes
    birth_prob = 1/sum(current_tree$terminal == -1)
    
    #Reverse: prob(Death at child nodes given T')
    #Get the number available parents with 2 terminal nodes that can be collapsed
    av_parents = new_tree %>% filter(terminal == -1) %>%
      count(parent_node) %>% filter(n == 2)
    
    death_prob = 1/nrow(av_parents)
    
    #Adjust the move and reverse probabilities
    move_pr = move_pr*birth_prob
    rev_pr = rev_pr*death_prob
    return(list(move_prob = move_pr, rev_prob = rev_pr))
    
  }else if(operation == 'prune'){
    #Move: prob(Birth at node the given node given T) = 1/#Terminal Nodes
    birth_prob = 1/sum(new_tree$terminal == -1)
    
    #Reverse: prob(Death at child nodes given T')
    #Get the number available parents with 2 terminal nodes that can be collapsed
    av_parents = current_tree %>% filter(terminal == -1) %>%
      count(parent_node) %>% filter(n == 2)
    
    death_prob = 1/nrow(av_parents)
    
    #Adjust the move and reverse probabilities
    move_pr = move_pr*death_prob
    rev_pr = rev_pr*birth_prob
    
    return(list(move_prob = move_pr, rev_prob = rev_pr))
    
  }else{
    return(list(move_prob = move_pr, rev_prob = rev_pr))
  }
}