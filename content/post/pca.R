require(dplyr)
require(data.table)
require(ggplot2)
require(FactoMineR)
require(factoextra)
dt_data <- haven::read_spss('./content/post/Microdades anonimitzades fusio presencial.sav') %>%
  dplyr::filter(BOP_NUM %in% c(46))

var = c("RELACIONS_CAT_ESP", 
        "CONFI_TRIBUNALS", "CONFI_PARTITS" ,"CONFI_AJUNTAMENT", 
        "CONFI_GOV_ESP", "CONFI_SINDICATS", "CONFI_GOV_CAT", 
        "CONFI_CONGRÉS", "CONFI_PARLAMENT", "CONFI_UE", 
        "CONFI_MONARQUIA", "CONFI_EXÈRCIT", "CONFI_POLICIA",                           
        "CONFI_MOSSOS", "CONFI_ESGLÉSIA", "CONFI_BANCA",                            
        "CONFI_MITJANS", "CONFI_TRIBUNAL_CONSTITUCIONAL" ) 

var1 = c("Independencia", 
         "Tribunales", "Partidos políticos" ,"Ayuntamientos", 
         "Gobierno de España", "Sindicatos", "Govern de la Generalitat", 
         "Congreso de los diputados", "Parlament Català", "UE Parliament", 
         "Monarquía", "Ejército", "Policía Nacional",                           
         "Mossos", "Iglésia", "Banca" ,                       
         "Medios de Comunicación", "Tribunal Constitucional" ) 
dt_data2 <- dt_data %>% 
  dplyr::select(var) %>% 
  dplyr::filter_if(is.numeric, function(x){
    !x %in% c(98, 99)
  }) %>% 
  setnames(var1)

mod.pca <- PCA(dt_data2[,-1], scale.unit = F, graph = F)
fviz_pca_biplot(mod.pca, axes = c(1,2), geom = 'point', pointsize = .9,
                repel = T, 
                habillage = haven::as_factor(dt_data2$Independencia)) + 
  theme_minimal()

var_s <- c('p28', paste0('p20', c('a', 'b', 'c', 'd', 'e', 'f', 'g', 
                         'h', 'i')), paste0('P20', c('J','K', 'L', 'M', 'N',
                         'P', 'Q', 'R')))

var1 = c("Independencia", 
         "Tribunales", "Partidos políticos" ,"Ayuntamientos", 
         "Gobierno de España", "Sindicatos", "Govern de la Generalitat", 
         "Congreso de los diputados", "Parlament Català", "UE Parliament", 
         "Monarquía", "Ejército", "Policía Nacional",                           
         "Mossos", "Iglésia", "Banca" ,                       
         "Medios de Comunicación", "Tribunal Constitucional" ) 

dt_data1 <- haven::read_spss('./content/post/Microdades anonimitzades fusio presencial.sav') %>%
  dplyr::select(c(var, 'ANY', 'MES', 'BOP_NUM')) %>% 
  as.data.table() %>% 
  dplyr::filter_if(is.numeric, function(x){
    !x %in% c(98, 99)
  }) %>% 
  setnames(new = var1, old = var) %>% 
  # mutate(Independencia = haven::as_factor(Independencia)) %>% 
  dplyr::filter(!is.na(Tribunales)) %>% 
  mutate(Independencia = ifelse(Independencia == 4, 'Si', 'No'))
  

dt_data2 <- haven::read_spss('./content/post/Microdades anonimitzades fusio cine telf.sav') 
dt_data3 <- dt_data2 %>% 
  dplyr::select(c(var_s, 'any', 'mes', 'BOP_num')) %>% 
  as.data.table() %>% 
  dplyr::filter_if(is.numeric, function(x){
    !x %in% c(98, 99)
  }) %>% 
  setnames(new = var1, old = var_s) %>%
  setnames(new = c('ANY', 'MES', 'BOP_NUM'),
           old = c('any', 'mes', 'BOP_num')) %>% 
  # mutate(Independencia = haven::as_factor(Independencia)) %>% 
  mutate(Independencia = ifelse(Independencia == 4, 'Si', 'No')) %>% 
  dplyr::filter(!is.na(Tribunales)) %>% 
  rbind(dt_data1)


dt_pca <- dt_data3 %>% 
  dplyr::filter(ANY %in% c(2009, 2012, 2015, 2017, 2019)) %>% 
  as_tibble() %>% 
  group_by(BOP_NUM, ANY) %>% 
  nest() %>% 
  mutate(
    pca.mod = map(data, function(x){
      PCA(x[, var1[-1]] %>%
            select_if(function(x){
              sum(is.na(x)) != length(x)
            }),
          scale.unit = F, graph = F, ncp = 3)  
    })
  ) %>% 
  ungroup() %>% 
  mutate(    
    scree_plot = pmap(list(pca.mod, ANY), function(x, y){
      fviz_eig(X = x, ncp = 3) + labs(x  = '', y = '', title = y) + 
        ylim(c(0, 60))
    }),
    plot_pca = pmap(list(pca.mod, data, ANY), function(x, datos, ANY){
      fviz_pca_biplot(x, axes = c(1,2), geom = 'point', 
                      pointsize = .4,
                      # pointsize = .8,
                      repel = T, 
                      labelsize = 2,
                      habillage = as.factor(datos$Independencia),
                      title = ANY
                      # subtitle = scales::percent(sum(datos$Independencia=='Si')/nrow(datos))
                      ) +
        theme(legend.position = 'none', title = element_text(size = 10)) +
        labs(
          x = '', y = '',
          color = 'Independencia',
             shape = 'Independencia') 
    })
  )
dt_pca$plot_pca[[2]] <- dt_pca$plot_pca[[2]] + scale_y_reverse()

plot_grid(plotlist = dt_pca$plot_pca)
plot_grid(plotlist = dt_pca$scree_plot, nrow = 1)
