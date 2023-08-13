# just so you know you can change based on index?

# now we are going to a function for use of access
# to computer nearest nei
#' Utility Tools to Easy Conversion of row col to index
#' @name idx_conversion
#' @param ij vector of idex, row, col number
#' @param idx singular index 
#' @param dim vector of dimension
#' @param delta_mtx
#' @param colwise 
#' @details 
#' if your input exceed matrix dimension it will automatically return zero
ij_to_idx = function(ij, dim) {
  i = ij[1]
  j = ij[2]
  if(!i |> between(1,dim[1])) return(0)
  if(!j |> between(1,dim[2])) return(0)
  (j - 1) * dim[1] + i
}
#' @rdname idx_conversion
idx_to_ij = function(idx, dim) {
  if(idx > prod(dim)) return(0)
  i = (idx - 1) %% dim[1] + 1
  j = (idx - 1) %/% dim[1] + 1
  return(c(i, j))
}

#' Making Delta Matrix of Something
#' @name nearyby
#' @param delta_mtx see detail
#' @param colwise for making delta matrix in column wise way
#' @param ij vector of idex, row, col number
#' @param idx singular index 
#' @param dim vector of dimension
#' @details
#' Delta matrix is a matrix define what a position should search for. 
#' Relative to seed position. So Left will be c(0,-1), Up will be c(1,0)
#' Going Up or Going Right is always positive, going the opposit direction 
#' will always be negative. 
make_delta_mtx = function(..., colwise = F) {
  ax_name=list(
    row=c('U','', 'D'),
    col=c('L','','R')
  )
  if(colwise) {
    mtx = cbind(
      ...
    )
  } else {
    mtx = rbind(...)
  }
  colnames(mtx) = c("row","col")
  rownames(mtx) = mtx |>
    apply(
      1,
      \(x) paste0(ax_name$row[x[1]+2], ax_name$col[x[2]+2])
    )
  return(mtx)
}
make_delta_cross = function() {
  make_delta_mtx(
    x = c(1, 0,-1,0),
    y = c(0,-1,0,1),
    colwise=T
  )
}
make_delta_surnd = function() {
  make_delta_mtx(
    c(1,1),
    c(-1,1),
    c(1,-1),
    c(-1,-1),
    c(1,0),
    c(-1,0),
    c(0,1),
    c(0,-1)
  )
}
#' @rdname nearyby
near_by_1 = function(idx, dim, delta_mtx) {
  if(missing(delta_mtx) || is.null(delta_mtx)) {
    delta_mtx=make_delta_mtx(
      x = c(1, 0,-1,0),
      y = c(0,-1,0,1),
      colwise=T
    )
  }
  ij = idx_to_ij(idx, dim)
  delta_mtx |> 
    apply(1, \(x) x + ij) |> 
    t() |> 
    apply(1, ij_to_idx, dim)
}
near_by = function(idxs, dim, delta_mtx) {
  t(sapply(idxs, near_by_1, dim, delta_mtx))
}
solve_mtx = function(canvas,
                     delta_mtx=NULL, 
                     update_FUN=NULL,
                     ...,
                     plot=F,
                     plot.lag=0.5
                     ) {
  mtx=canvas
  # make this vairable accessible
  dim = dim(mtx)
  unsolved = seq(length(mtx)) # keep index of to interate through
  step_value = 1
  while(any(mtx == 0)) {
    # save computation by find index of which one.
    grass = unsolved[which(mtx[unsolved] != 0)]
    soil = grass |>
      # Here need the speed can go exponentially slow ....
      near_by(dim, delta_mtx) |> 
      c() |> 
      unique() |>
      purrr::discard(~.x == 0)
    seed_to_plant=soil[which(mtx[soil] == 0 )]
    
    # This generation of grass is died
    unsolved = unsolved |> purrr::discard(~.x %in% grass)
    # all you need now is use patten matrix on this code
    if(is.function(update_FUN)) {
      mtx[seed_to_plant] <- update_FUN(...)
    } else {
      mtx[seed_to_plant] <- step_value
    }
    if(plot) {
      image(mtx)
      title(paste(step_value,"iteration"))
      Sys.sleep(plot.lag)
    }
    step_value = step_value + 1
    if(step_value > max(dim(mtx))) break
  }
  return(mtx)
}
#The trouble of this function is if I ever need a customisable way of update
# Variable it is going to be difficult
reso = 3
rule_matrix = matrix(sample(LETTERS[1:3], reso * reso, replace=T), reso,reso)
rule_matrix

identity = unique(c(rule_matrix)) |> sort()
i_rule_matrix = match(rule_matrix, identity)
r_dim = dim(rule_matrix)
dim(i_rule_matrix) = r_dim


rule_df = seq(length(i_rule_matrix)) |> 
  near_by(dim(rule_df))
i_rule_matrix |> 
  length() |> 
  seq() |> 
  near_by(dim(i_rule_matrix)) |> 
  apply(1, \(x) ifelse(x==0, 0, i_rule_matrix[x])) |> 
  t() |> 
  data.frame() |> 
  mutate(v = c(i_rule_matrix)) |> 
  tidyr::pivot_longer(
    col = c(everything(),v)
  )
  


  

mtx_empty = matrix(rep(0, 100), 10,10)
mtx = mtx_empty

mtx[sample(seq(length(mtx)), 4)] = sample(seq(length(identity)), 4, replace = T)
cell_seed = which(mtx!=0)
cell_new = cell_seed |> 
  near_by(dim(mtx)) |> 
  c() |> unique()
surounding = cell_new |> # surroundings
  near_by(dim(mtx)) |> 
  apply(1, \(x) {
    ifelse(x==0,x,mtx[x])
  }) |> 
  t()
surounding
rule_list
