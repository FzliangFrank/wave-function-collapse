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