#==========================================================
# A function to create a truncated periodic dataset
#==========================================================

trunc_function <- function(period = 86400,      # [s] periodicity of the function
                          duration = period/2, # [s] seconds during which function is positive
                          offset = 0,          # [s] offset (t-Offset)
                          trunc = 0,           #     value below which function is truncated
                          average = 1,         # [-] average of the function over one period
                          maximum = NA) {      # [-] max of the function over one period

  if (! is.na(maximum)){
    if (trunc>maximum) stop ("trunc cannot be > maximum")
  }

  xx <- seq(0, 2*pi, length.out = 100000)
  cx <- sin(xx -pi/2)
  fx <- function(x) return (sum( (cx+x)>0 )/length(cx) -duration/period)
  rt <- uniroot(fx, c(-1.1,1.1))$root

  if (! is.na(maximum)) M  <- maximum/max(pmax(cx +rt,trunc)) else M  <- average/mean(pmax(cx +rt,trunc))

  # Function to be returned
  RF <- function(t)   # time in seconds
    pmax(trunc, M*(sin(2*pi*(t-offset)/period -pi/2) +rt))
  return(RF)
}

