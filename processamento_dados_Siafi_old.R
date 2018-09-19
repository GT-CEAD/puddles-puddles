# vamos tratar os dados. comentarios nas notas de cada linha

dados <- dados_Siafi %>%
  mutate(Date = if_else(Mes == 0, ymd(paste0(Ano,"-01-01")), dmy(Data))) %>% # (0)
  group_by(Date, codOrgao) %>%
  summarise(Movimento = sum(as.numeric(as.character(Movimento)))) %>% # (1)
  ungroup() %>% # (2)
  select(Date, codOrgao, Movimento) %>% # (3)
  spread(key = codOrgao, value = Movimento, fill = 0) %>% # (4)
  arrange(Date) %>% # (5)
  complete(Date = seq(min(Date), max(Date), by = "days")) %>% # (6)
  replace(is.na(.), 0) %>% # (7)
  mutate_at(-1, funs(cumsum(.))) %>% # (8)
  gather(-1, key = "codOrgao", value = "Saldo") # (9)

# (0) criar primeiro a coluna no formato de data, tratando os meses de abertura. importante: tem que usar o dplyr::if_else senão o ifelse muda o tipo do dado! :/

# poderia ter usado também:

# mutate(Date = if_else(Mes == 0, as.Date(paste0(Ano,"-01-01")), 
#                       as.Date(paste(Ano, Mes, Dia, sep = "-"))))

# (1) esse group_by com summarise é para somar eventuais registros repetidos. Ou seja, preciso ficar com um movimento para cada dia, para cada orgao. como atribuí que o saldo de abertura seria um movimento no dia 01/01, e tinha um caso em que havia de fato movimento no dia 01/01 em um orgao, apareciam dois registros no dia 01/01 nesse orgao. o summarise elimina esse problema.

# (2) por um motivo que ainda não entendo, sem esse ungroup ele não calcula o cumsum corretamente.

# (3) estou pegando apenas o essencial para que minha planilha fique com apenas uma linha por data, quando eu fizer o spread. depois trago os nomes dos órgãos de novo via join, se for necessário.

# (4) com o spread, um orgao que eventualmente não possuía movimento em determinado dia vai aparecer com "NA". Preenchi com 0 pq depois vou usar o cumsum para calcular os saldos.

# (5) classificando por data para o cumsum ter o significado que quero: saldos acumulados diários.

# (6) o complete vai completar as datas faltantes considerando a sequencia de dias entre o menor e o maior dia presente nos dados. Os valores para esses dias faltantes serão completados com "NA", então...

# (7) o replace substitui esses NAs por 0.

# (8) agora uso o cumsum para calcular os totais acumulados em cada coluna (que neste ponto se referem a cada órgão.) usei o -1 na referência do mutate_at para calcular essa soma acumulada para TODAS as colunas, EXCETO a primeira (pq ela é a data).

# (9) agora, de maneira semelhante, uso o -1 para dizer que quero empilhar todas as colunas, exceto a primeira, que é a coluna de data.
