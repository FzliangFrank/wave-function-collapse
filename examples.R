require(purrr)
require(dplyr)
mtx_empty = matrix(rep(0, 100), 10,10)
mtx = mtx_empty
mtx[sample(length(mtr),1)] = 1
source("tools.R")
# Set Random Seed and get near by square positions
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
# Interacte Through a series of number
if(interactive()) {
  # initializing
  resolution=50
  mtx_empty = matrix(rep(0, resolution^2), resolution, resolution)
  mtx = mtx_empty
  # set first seed
  first_seed = sample(seq(length(mtx)),1) # random intialise
  ini_value = 1
  step_value = ini_value
  mtx[first_seed] = step_value
  solve_mtx(mtx, plot=T, plot.lag=0.1)
}
# Check - Board
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
  first_seed = sample(seq(length(mtx)),1) # random intialise
  mtx[first_seed] = step_value
  solve_mtx(mtx, custom_delta,plot=T,plot.lag=0.1)
}
if(interactive()) {
  custom_delta = make_delta_mtx(
    c(1,1),
    c(-1,1),
    c(1,-1),
    c(-1,-1)
  )
  resolution=50
  mtx_empty = matrix(rep(0, resolution^2), resolution,resolution)
  mtx = mtx_empty
  # set first seed
  
  first_seed = sample(seq(length(mtx)),1) # random intialise
  ini_value = 1
  step_value = ini_value
  mtx[first_seed] = step_value
  
  solve_mtx(mtx, custom_delta,plot=T,plot.lag=0.1)
}
if(interactive()) {
  custom_delta = make_delta_mtx(
    c(1,1),
    c(-1,1),
    c(1,-1),
    c(-1,-1),
    c(1,0),
    c(-1,0),
    c(0,1),
    c(0,-1)
  )
  resolution=50
  mtx_empty = matrix(rep(0, resolution^2), resolution,resolution)
  mtx = mtx_empty
  mtx[sample(seq(length(mtx)), 1)]=1
  solve_mtx(mtx, custom_delta,plot=T,plot.lag=0.1)
}

