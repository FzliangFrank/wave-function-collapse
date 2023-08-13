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