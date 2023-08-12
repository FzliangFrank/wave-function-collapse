require(purrr)
require(dplyr)

# just so you know you can change based on index?
mtx_empty = matrix(rep(0, 100), 10,10)
mtx = mtx_empty
mtx[sample(length(mtr),1)] = 1

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
#' @rdname nearyby
near_by = function(idx, dim, delta_mtx) {
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
solve_mtx = function(canvas,
                     delta_mtx=NULL, 
                     update_FUN=NULL, 
                     plot=F,
                     plot.lag=0.5,
                     ...) {
  mtx=canvas
  # set first seed
  first_seed = sample(seq(length(mtx)),1) # random intialise
  ini_value = 1
  step_value = ini_value
  mtx[first_seed] = step_value
  
  # make this vairable accessible
  dim = dim(mtx)
  
  while(any(mtx == 0)) {
    grass = which(mtx != 0)
    soil = grass |>
      # Here need the speed can go exponentially slow ....
      purrr::map(
        near_by, dim, delta_mtx
      ) |>
      unlist() |>
      unique() |>
      purrr::discard(~.x == 0)
    seed_to_plant=soil[which(mtx[soil] ==0 )]
    
    # all you need now is use patten matrix on this code
    if(is.function(update_FUN)) {
      mtx[seed_to_plant] <- update_FUN(...)
    } else {
      mtx[seed_to_plant] <- sqrt(step_value)
    }
    if(plot) {
      image(mtx)
      title(paste(step_value,"iteration"))
      Sys.sleep(plot.lag)
    }
    step_value = step_value + 1
    # if(step_value > 50) break
  }
  return(mtx)
}

# near_by(33, dim(mtx))

# Examples and Test of your Code have worked
# Demo what `near_by` deos
if(interactive()) {
  resolution=20
  mtx_empty = matrix(rep(0, resolution^2), resolution,resolution)
  mtx = mtx_empty
  rdm_idx = sample(seq(length(mtx)),1)
  mtx[rdm_idx] = 1
  image(mtx)
  Sys.sleep(0.5)
  mtx[near_by(rdm_idx,
              dim(mtx)
              )] = 2
  image(mtx)
}
# Default Update Schema
if(interactive()) {
  # initializeing
  resolution=20
  mtx_empty = matrix(rep(0, resolution^2), resolution,resolution)
  mtx = mtx_empty
  solve_mtx(mtx, plot=T, plot.lag=0.1)
}
# Use Custom Delta Matrix
if(interactive()) {
  custom_delta = make_delta_mtx(
    c(0,1),
    c(0,2),
    c(1,2),
    c(2,2),
    c(2,1),
    c(2,0),
    c(2,-1),
    c(2,-2),
    c(1,-2),
    c(0,-2),
    c(-1,-2),
    c(-2,-2),
    c(-1,-2),
    c(0,-2),
    c(1,-2),
    c(2,-2)
  )
  resolution=50
  mtx_empty = matrix(rep(0, resolution^2), resolution,resolution)
  mtx = mtx_empty
  solve_mtx(mtx, custom_delta,plot=T,plot.lag=0.1)
}
