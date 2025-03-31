# Tablas regionales -------------------------------------------------------

PROYECCIONES_POBmulti <- PROYECCIONES_POBmulti %>% mutate(NombreComuna = case_when(NombreComuna == "TILTIL" ~ "TIL TIL", TRUE ~ NombreComuna))



tab_poblacion <- function(region)
  {PROYECCIONES_POBmulti %>%
  filter(RegionAB == region) %>% 
  select(NombreComuna, `Población censada`, `Suma de Poblacion 2025`, `Número de personas en situación de pobreza multidimensional (**)`, Código) %>%
  group_by(NombreComuna) %>%
  summarise(
    "Población 2024" = sum(`Población censada`), 
    "Población 2025" = sum(`Suma de Poblacion 2025`), 
    "Crec/Decrec relativo de población 2024 al 2025" = format(
      round((first(`Suma de Poblacion 2025`) - first(`Población censada`)) / first(`Población censada`) * 100, 2), 
      big.mark = ".", 
      decimal.mark = ",", 
      nsmall = 2
    ),
    "Pobreza Multidimensional" = scales::comma(
      round(first(`Número de personas en situación de pobreza multidimensional (**)`)), 
      big.mark = "."
    ),
    Código = Código
    
  )
}




tab_monto <- function(region){
  BASE %>%
  filter(RegionAB == region) %>% 
  select(NombreComuna, `Monto 2025`) %>%
  group_by(NombreComuna) %>%
  summarise("Monto2025" = sum(`Monto 2025`)) |> 
    mutate(NombreComuna = case_when(NombreComuna == "COIHAIQUE" ~ "COYHAIQUE", TRUE ~ NombreComuna) )
}






tab_poblacion_Metropolitana <- PROYECCIONES_POBmulti %>%
  filter(RegionAB=="METROPOLITANA") %>%  
  select(NombreComuna, `Población censada`, `Suma de Poblacion 2025`, `Número de personas en situación de pobreza multidimensional (**)`, Código) %>%
  group_by(NombreComuna) %>%
  summarise(
    "Población 2024" = first(`Población censada`),  
    "Población 2025" = first(`Suma de Poblacion 2025`),  
    "Crec/Decrec relativo de población 2024 al 2025" = format(
      round((first(`Suma de Poblacion 2025`) - first(`Población censada`)) / first(`Población censada`) * 100, 2), 
      big.mark = ".", 
      decimal.mark = ",", 
      nsmall = 1
    ),
    "Pobreza Multidimensional" = scales::comma(
      round(first(`Número de personas en situación de pobreza multidimensional (**)`)), 
      big.mark = "."
    ),
    Código = Código)


tab_monto_RM <- BASE %>%
  filter(RegionAB == "METROPOLITANA") %>% 
  select(NombreComuna, `Monto 2025`) %>%
  group_by(NombreComuna) %>%
  summarise("Monto2025" = sum(`Monto 2025`))


tab_resumen_RM <- left_join(tab_poblacion_Metropolitana,tab_monto_RM, by = "NombreComuna")


tab_resumen_RM <- tab_resumen_RM %>%
  mutate(
    Monto2025 = case_when(
      NombreComuna == "Pirque" ~ 0, 
      TRUE ~ Monto2025        
    )
  )





tab_poblacion_Biobio <- PROYECCIONES_POBmulti %>%
  filter(RegionAB == "BIOBÍO") %>%  
  select(NombreComuna, `Población censada`, `Suma de Poblacion 2025`, `Número de personas en situación de pobreza multidimensional (**)`, Código) %>%
  group_by(NombreComuna) %>%
  summarise(
    "Población 2024" = first(`Población censada`),  
    "Población 2025" = first(`Suma de Poblacion 2025`),  
    "Crec/Decrec relativo de población 2024 al 2025" = format(
      round((first(`Suma de Poblacion 2025`) - first(`Población censada`)) / first(`Población censada`) * 100, 1), 
      big.mark = ".", 
      decimal.mark = ",", 
      nsmall = 1
    ),
    
    "Pobreza Multidimensional" = scales::comma(
      round(first(`Número de personas en situación de pobreza multidimensional (**)`)), 
      big.mark = "."
    ),
    Código = Código
  )



tab_monto_BIOBIO <- BASE %>%
  filter(RegionAB == "BIOBÍO") %>% 
  select(NombreComuna, `Monto 2025`) %>%
  group_by(NombreComuna) %>%
  summarise("Monto2025" = sum(`Monto 2025`)) %>% 
  mutate(NombreComuna = case_when(NombreComuna == "ALTO BIO BIO" ~ "ALTO BIOBIO", TRUE ~ NombreComuna) )



tab_resumen_BIOBIO <- left_join(tab_poblacion_Biobio,tab_monto_BIOBIO, by = "NombreComuna")





