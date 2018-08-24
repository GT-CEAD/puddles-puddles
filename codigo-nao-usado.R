# a seguir, uma rotina para gerar uma tabela com as posições das primeiras e
# últimas UGs de cada órgão, para poder colorir no gráfico a região que engloba # # todas as UGs de determinado órgão

lista_UGs <- dados_sumarizados %>%
  group_by(UG, Orgao) %>%
  summarise(newUG = first(UG)) %>%
  arrange(desc(UG))

orgs <- levels(as.factor(dados_sumarizados$Orgao))

Primeiros <- NULL
Ultimos <- NULL
Indice_Primeiros <- NULL
Indice_Ultimos <- NULL

for (i in 1:length(orgs)) {
  subconjunto_UGs_Orgao <- subset(lista_UGs, lista_UGs$Orgao == orgs[i])
  
  primeira_UG <- subconjunto_UGs_Orgao$UG[1]
  ultima_UG <- rev(subconjunto_UGs_Orgao$UG)[1]
  
  indice_primeira_UG <- which(lista_UGs$UG == primeira_UG)
  indice_ultima_UG <- which(lista_UGs$UG == ultima_UG)
  
  Primeiros <- c(Primeiros, as.character(primeira_UG))
  Ultimos <- c(Ultimos, as.character(ultima_UG))
  
  Indice_Primeiros <- c(Indice_Primeiros, indice_primeira_UG)
  Indice_Ultimos <- c(Indice_Ultimos, indice_ultima_UG)
}

orgs_posicoes <- data.frame("Orgao" = orgs, Primeiros, Ultimos, Indice_Primeiros, Indice_Ultimos)

#####################

sumario <- dados_sumarizados %>%
  group_by(UG) %>%
  summarise(minimo = first(minimo),
            maximo =  first(maximo),
            mediana = first(mediana),
            media =   first(media))
