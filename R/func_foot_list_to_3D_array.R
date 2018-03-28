#' foot_list_to_3D_array
#'
#'
#' @export

foot_list_to_3D_array = function(foot_list){
  matrix_dim_est = F # flag once the matrix slice dimentions have been established
  
  for(i in 1:length(foot_list)){
    if(length(foot_list[[i]]) > 1 & matrix_dim_est == F){ #whena  footprint ouptut is valid and matrix size has not been determined
      dimen = dim(foot_list[[i]]$PHI) # store size of matrix
      if(!exists("valid_footprints")) # if this is the first valid_output
        valid_footprints = 1 # start counting
      else
        valid_footprints = valid_footprints + 1 # if not first valid, keep counting
      matrix_dim_est = T # flag that matrix has been determined
    }
    if(length(foot_list[[i]]) > 1 & matrix_dim_est == T){
      if(!exists("valid_footprints"))
        valid_footprints = 1
      else
        valid_footprints = valid_footprints + 1
    }
  }
  valid_footprints = valid_footprints-1
  foot_array = array(NA,dim = c(valid_footprints,dimen)) # create empty array of determined dimentions
  k = 1
  for(i in 1:length(foot_list)){
    if(length(foot_list[[i]]$PHI) > 1){
      foot_array[k,,] = foot_list[[i]]$PHI
      if(!exists("array_names"))
        array_names = names(foot_list)[i]
      else
        array_names = c(array_names,names(foot_list)[i])
      k = k + 1
    }
  }
  dimnames(foot_array)[[1]] = array_names
  #return
  foot_array
}