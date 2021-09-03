# Helpers ------------------------------------------------------------
## Tablaa para exportar
tabla <- function(data) {
  table_return <- data %>% group_by(prov,name_cau_67) %>%  summarise(fallecidos = n(),
                                                                     mean_YLL = round(mean(yll, na.rm=T),2),
                                                                     mean_dis_YLL = round(mean(ylldis,na.rm=T),2))
  return(table_return)
}

# Función para juntar Data Geografica y datos calculados
merge_geo <- function(data, filtro, ...) {
  #req(input)
  ## Importar geojson
  
  ecu_prov <- geojsonio::geojson_read(paste0(dir_bdd, "ecu_prov.geojson"), what = "sp")
  
  ## Data de trabajo
  datos <-  data %>%  mutate( # name_cau_67= str_trim(name_cau_67, side = "both"),
    prov=toupper(prov))     %>%
    filter(name_cau_67 %in% filtro) %>%
    group_by(prov)                 %>%
    summarise(yll    = round(mean(yll,    na.rm=T),1),
              ylldis = round(mean(ylldis, na.rm=T),1)) 
  
  # Merge
  sp::merge(ecu_prov, datos, by.x="dpa_despro", by.y="prov")
  # return(newobj)
}
