#==========================================================
# A function to create a truncated periodic dataset
#==========================================================

trunc_function <- function(
    t_period = 86400,      # [s] periodicity of the function
    t_offset  = 0,         # [s] offset (t-Offset)
    t_trunc  = t_period/2, # [s] seconds during which function is > trunc
    v_trunc   = 0,         # [-] value below which function is truncated
    v_average = 1,         # [-] average of the function over one period
    v_maximum = NA) {      # [-] max of the function over one period
  
  if (! is.na(v_maximum)){
    if (v_trunc > v_maximum) stop ("v_trunc cannot be > v_maximum")
  }
  
  # function to estimate parameters
  xx <- seq(0, 2*pi, length.out = 100000)
  cx <- sin(xx - pi/2)
  
  # x: 
  fx <- function(x) 
    return (sum( (cx + x) > 0 ) / length(cx) - t_trunc/t_period)
  
  rt <- uniroot(fx, c(-1.1, 1.1))$root
  
  if (! is.na(v_maximum)) 
    M  <- v_maximum / max (pmax(cx + rt, v_trunc)) else 
      M  <- v_average / mean(pmax(cx + rt, v_trunc))
  
  # Function to be returned
  RF <- function(t)   # time in seconds
    pmax(v_trunc, 
         M * (sin(2*pi*(t - t_offset)/t_period - pi/2) + rt))
  
  return(RF)
}

