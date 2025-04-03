
# Goto, S., Yamano, M., Morita, S. et al. Physical and thermal properties of mud-dominant sediment from the Joetsu Basin in the eastern margin of the Japan Sea. Mar Geophys Res 38, 393–407 (2017). https://doi.org/10.1007/s11001-017-9302-y


#===============================================================================
#===============================================================================
# Mineral properties
#===============================================================================
#===============================================================================

Mdesc <- function(names){
 Md <- data.frame(
      names = c("density", "tc", "td", "cp"),
      description = c("density", "thermal conductivity",
                     "thermal diffusivity", "heat capacity"),
      units = c("kg/m3", "W/m/s", "m2/s", "J/kg/dg"))
  Md[Md$names %in% names,]
}


mineral_properties <- function(
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
               "Muscovite", "Illite", "Smectite (montmorillonite)", 
               "Chlorite", "Pyrite")){
  
  mineral_prop <- data.frame(
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
              "Muscovite", "Illite", "Smectite (montmorillonite)", 
              "Chlorite", "Pyrite", "Seawater"),
    density = c(2648, 2570, 2620, 2760, 2710, 2831, 2660, 2608, 2800, 5011, 1025),
    tc = c(7.690,  2.320,  2.140,  1.680,  3.590,  
         2.320,  1.850,  1.880,  5.150, 19.210, 0.596),
    td = c(3.92e-06, 1.28e-06, 1.05e-06, 8.17e-07, 1.62e-06, 
         1.03e-06, 8.61e-07, 9.07e-07, 2.25e-06, 7.40e-06, 1.46e-07),
    cp = c(741,  707,  776,  745,  820,  796,  808,  795,  818,  518, 3993))

  mineral <- match.arg(mineral, several.ok = TRUE)
  MP      <- mineral_prop[mineral_prop$mineral %in% mineral, ]
  attributes(MP)$description <- Mdesc(c("density", "tc", "td", "cp"))
  attributes(MP)$parameters <- mineral
  MP
}

#===============================================================================

mineral_density <- function(
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite")){
  

  MP <- mineral_properties(mineral = mineral)[, c("mineral", "density")]
  attributes(MP)$description <- Mdesc("density")

  return(MP)
}

#===============================================================================

mineral_tc <- function(
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite")){
  
  
  MP <- mineral_properties(mineral = mineral)[, c("mineral", "tc")]
  attributes(MP)$description <- Mdesc("tc")
  
  return(MP)
}

#===============================================================================

mineral_td <- function(
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite")){
  
  
  MP <- mineral_properties(mineral = mineral)[, c("mineral", "td")]
  attributes(MP)$description <- Mdesc("td")
  
  return(MP)
}

#===============================================================================

mineral_cp <- function(
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite")){
  
  
  MP <- mineral_properties(mineral = mineral)[, c("mineral", "cp")]
  attributes(MP)$description <- Mdesc("cp")
  
  return(MP)
}

