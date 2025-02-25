setwd("D:/SADECK_DR/Table_teste")
file_list <- list.files(pattern = "\\.csv$")

# Carregar o pacote
library(readr)

# Ler os arquivos em uma lista
tables <- lapply(file_list, read_csv)

library(data.table)

# Ler os arquivos em uma lista
tables <- lapply(file_list, fread)

# Exemplo: Acessar a primeira tabela
head(tables[[1]])

# Exemplo: Nomear as tabelas com o nome dos arquivos
names(tables) <- file_list

colnames(tables[[1]])

# Instalar o pacote dplyr (se necessário)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

# Carregar o pacote
library(dplyr)

# Exibir nomes das colunas da primeira tabela
colnames(tables[[1]])

tables <- lapply(tables, function(df) {
  df %>% select(`area_m²`, id)
})


# Verificar uma amostra maior dos valores da coluna 'area_m²'
head(tables[[1]]$'area_m²', 50)

#---------------

tables <- lapply(tables, function(df) {
  # Calcular a área em km² e adicionar como nova coluna
  df$area_km2 <- df$'area_m²' / 1e6  # Dividir por 1.000.000 para converter m² para km²
  df
})

# Verificar uma amostra maior dos valores da coluna 'area_m²'
head(tables[[1]]$'area_km2', 50)

# Verificar o tipo da nova coluna
class(tables[[1]]$area_km2)

# Adicionar uma coluna 'ano' em cada tabela para identificar o ano
years <- 1988:2023

tables <- mapply(function(df, year) {
  df$ano <- year  # Adiciona o ano à tabela
  df
}, tables, years, SIMPLIFY = FALSE)

# Combinar todas as tabelas em uma única tabela
combined_data <- bind_rows(tables)

install.packages("tidyr")
library(tidyr)


# Transformar os dados para o formato desejado
wide_data <- combined_data %>%
  select(id, ano, area_km2) %>%
  pivot_wider(names_from = ano, values_from = area_km2)

head(wide_data)
# Salvar a tabela em um arquivo CSV
write.csv(wide_data, "tabela_areas_por_ano.csv", row.names = FALSE)

#-------------
area_total <- read.csv("D:/SADECK_DR/Area_nf_hidro_hexa/areatotal_hexa.csv", stringsAsFactors = FALSE)
head(area_total)

area_total <- area_total %>%
  select(id, A_C_km2)

#-------------
area_hidro <- read.csv("D:/SADECK_DR/Area_nf_hidro_hexa/area_hexa_hidro_ok.csv", stringsAsFactors = FALSE)
head(area_hidro)

area_hidro <- area_hidro %>%
  select(id, area_km2_hidr)
#-------------
area_nf <- read.csv("D:/SADECK_DR/Area_nf_hidro_hexa/area_hexa_nf_ok.csv", stringsAsFactors = FALSE)
head(area_nf)

area_nf <- area_nf %>%
  select(id, area_km2_nf)
#-------------
# Unir as tabelas na ordem desejada
tabela_final <- wide_data %>%
  left_join(area_total, by = "id") %>%
  left_join(area_hidro, by = "id") %>%
  left_join(area_nf, by = "id")

head(tabela_final)
write.csv(tabela_final, "tabela_final.csv", row.names = FALSE)
#-------------

# Adicionar uma nova coluna com a soma
tabela_final <- tabela_final %>%
  mutate(area_km2_soma = area_km2_hidr + area_km2_nf)

# Verificar as primeiras linhas da tabela final
head(tabela_final)

#-------------
# Adicionar uma nova coluna com a diferença entre A_C_km2 e area_km2_soma
tabela_final <- tabela_final %>%
  mutate(area_restante_km2 = A_C_km2 - area_km2_soma)

head(tabela_final)
colnames(tabela_final)
#-------------
#-------------
#-------------

# Função para calcular a soma acumulada dos anos anteriores
calcular_velocidade <- function(data, ano_atual) {
  # Calcular a soma dos anos anteriores até o ano_atual
  anos_anteriores <- as.character(1988:ano_atual)  # Lista de anos até o ano atual
  soma_anteriores <- rowSums(data[, anos_anteriores, drop = FALSE], na.rm = TRUE)
  
  # Calcular a "velocidade" de perda de área para o ano atual
  velocidade <- data[[as.character(ano_atual)]] / (data$area_restante_km2 - soma_anteriores)
  
  return(velocidade)
}

# Aplicando a função para calcular a velocidade de perda para cada ano de 1988 a 2023
for (ano in 1988:2023) {
  coluna_nome <- paste0(ano, "_som")
  tabela_final <- tabela_final %>%
    mutate(!!coluna_nome := calcular_velocidade(tabela_final, ano))
}

# Verificando as primeiras linhas da tabela
head(tabela_final)

# Gerar uma lista de colunas de 1988_som até 2023_som
anos_som_cols <- paste0(1988:2023, "_som")

# Selecionando apenas as colunas id e as colunas de anos_som
tabela_som <- tabela_final %>%
  select(id, all_of(anos_som_cols))

# Verificando as primeiras linhas da tabela de resultados
head(tabela_som)

write.csv(tabela_som, "tabela_som.csv", row.names = FALSE)
colnames(tabela_som)
##--------------
##--------------
##--------------
install.packages("som")
library(som)

dados <- read.csv("D:/SADECK_DR/Table_teste/tabela_som.csv") # Substitua pelo caminho do seu arquivo
id_col <- dados$id # Salve a coluna 'id'
dados_som <- dados[,-1] # Exclua o 'id' para treinar o SOM

dados_som <- normalize(dados_som, byrow = FALSE)


#id_col <- tabela_som$id # Salve a coluna 'id'
#dados_som <- tabela_som[,-1] # Remova a coluna 'id'
#dados_som <- normalize(dados_som, byrow = FALSE) # Normalização por coluna

resultado_som <- som(dados_som, xdim = 11, ydim = 11, topol = "rect", neigh = "gaussian")

plot(resultado_som, ylim = c(-1, 1))

coordenadas_som <- resultado_som$visual[, c("x", "y")] # Extraia as coordenadas do SOM
resultado_final <- cbind(id_col, coordenadas_som) # Combine com a coluna 'id'

write.csv(resultado_final, "resultados_som11x11.csv", row.names = FALSE)

bmus <- resultado_som$visual[, c("x", "y")] # Coordenadas das BMUs
dados_som_com_bmus <- cbind(id_col, dados_som, bmus) # Combina com os IDs e dados originais

library(kohonen)
resultado_som <- supersom(data = list(dados_som), grid = somgrid(xdim = 11, ydim = 11, topo = "rect"))
bmu_indices <- predict(resultado_som, newdata = list(dados_som))$unit.classif
dados_com_classes <- cbind(id_col, classe = bmu_indices)

write.csv(dados_com_classes, "dados_com_classes.csv", row.names = FALSE)



#som_grid <- somgrid(xdim = 15, ydim = 15, topo = "hexa")

# Treinar o SOM com supersom
#resultado_som <- supersom(data = list(dados_som), grid = som_grid, 
#                          rlen = 100, # Número de épocas de treinamento
#                          alpha = c(0.05, 0.01), # Taxa de aprendizado
#                          radius = c(5, 1)) # Raio inicial e final
#
#dim(dados_som)  # Verifique as dimensões de dados_som
#
#bmu_indices <- map(resultado_som, newdata = list(dados_som))$unit.classif
#
#
## Criar uma tabela com as coordenadas da grade SOM
#coords <- as.data.frame(expand.grid(x = 0:(som_grid$xdim - 1), y = 0:(som_grid$ydim - 1)))
#
## Associar as coordenadas às observações
#bmu_coords <- coords[bmu_indices, ]
#
#dim(bmu_coords)  # Isso deve ser igual a dados_som
#
#dados_com_coords <- cbind(id_col, dados_som, bmu_coords)
#dim(dados_com_coords)
#
#write.csv(dados_com_coords, "resultados_som15x15.csv", row.names = FALSE)



set.seed(123) # Para reprodutibilidade
k <- 5
folds <- sample(rep(1:k, length.out = nrow(dados_som)))

library(purrr)

erros <- map_dbl(1:k, function(i) {
  train_data <- dados_som[folds != i, ]
  test_data <- dados_som[folds == i, ]
  
  # Treine o SOM nos dados de treino
  som_model <- som(train_data, xdim = 11, ydim = 11, topol = "rect", neigh = "gaussian")
  
  # Avalie o erro de quantização nos dados de teste
  test_bmu <- som_model$code[apply(test_data, 1, function(obs) {
    which.min(colSums((som_model$code - obs)^2))
  }), ]
  
  mean(sqrt(rowSums((test_data - test_bmu)^2)))
})

# Média do erro de validação cruzada
mean(erros)
##--------------
#Dimensão automática

install.packages("kohonen")
library(kohonen)

som_grid <- somgrid(xdim = 11, ydim = 11, topo = "hexa")
som_model <- supersom(data = dados_som, grid = som_grid)

resultados <- list()

for (dim in seq(5, 11, by = 2)) { # Testa diferentes tamanhos
  # Define a grade do SOM
  som_grid <- somgrid(xdim = dim, ydim = dim, topo = "hexa")
  
  # Treina o modelo SOM
  som_model <- supersom(data = dados_som, grid = som_grid)
  
  # Encontra as BMUs para cada linha dos dados
  bmu_indices <- map(som_model, newdata = dados_som)$unit.classif
  
  # Recupera os pesos das BMUs
  pesos_bmus <- som_model$codes[[1]][bmu_indices, ]
  
  # Calcula o erro de quantização
  distancias <- sqrt(rowSums((dados_som - pesos_bmus)^2))
  q_error <- mean(distancias)
  
  # Salva o erro no resultado
  resultados[[paste(dim, dim, sep = "x")]] <- q_error
}

# Identifica o tamanho com menor erro
melhores_dimensoes <- names(resultados)[which.min(unlist(resultados))]
print(melhores_dimensoes)


##--------------

library(ggplot2)

# Cria uma tabela com as coordenadas da grade SOM
grid_som <- expand.grid(x = 0:(resultado_som$xdim - 1), 
                        y = 0:(resultado_som$ydim - 1))

# Conta quantos pontos estão em cada célula
cluster_counts <- dados_som_com_bmus %>%
  group_by(x, y) %>%
  summarize(count = n())

# Junta ao grid
grid_som <- left_join(grid_som, cluster_counts, by = c("x", "y"))
grid_som$count[is.na(grid_som$count)] <- 0 # Preenche células sem dados

ggplot(grid_som, aes(x = x, y = y, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Contagem") +
  theme_minimal() +
  labs(title = "Mapa de Contagem de BMUs no SOM", x = "x", y = "y")

ggplot(dados_som_com_bmus, aes(x = x, y = y)) +
  geom_jitter(color = "darkblue", alpha = 0.6, size = 2) +
  theme_minimal() +
  labs(title = "Distribuição das BMUs no SOM", x = "x", y = "y")

# Extrair o índice da BMU no vetor de pesos
indices_bmu <- dados_som_com_bmus$x + dados_som_com_bmus$y * resultado_som$xdim + 1

# Recuperar os códigos de referência (vetores de pesos) das BMUs
pesos_bmu <- resultado_som$code[indices_bmu, ]

# Calcular a distância entre cada observação e o vetor de pesos da sua BMU
distancias <- sqrt(rowSums((dados_som - pesos_bmu)^2))

# Calcular o erro médio de quantização
quantization_error <- mean(distancias)

# Adicionar o erro ao dataframe
dados_som_com_bmus$quant_error <- distancias


# Visualiza o erro por observação
ggplot(dados_som_com_bmus, aes(x = id_col, y = quant_error)) +
  geom_col(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Erro de Quantização por Observação", x = "ID", y = "Erro de Quantização")

erro_por_celula <- dados_som_com_bmus %>%
  group_by(x, y) %>%
  summarize(mean_error = mean(quant_error))

grid_som <- left_join(grid_som, erro_por_celula, by = c("x", "y"))

ggplot(grid_som, aes(x = x, y = y, fill = mean_error)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Erro Médio") +
  theme_minimal() +
  labs(title = "Mapa de Erro Médio de Quantização no SOM", x = "x", y = "y")

