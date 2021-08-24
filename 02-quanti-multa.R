# --------------------------------------------------------------------
# Análises para Parecer Tanure
# Autores: Bruno Daleffi, Caio Lente, Julio Trecenti e Nicole Luduvice
# Descrição: Análise descritiva da multa
# --------------------------------------------------------------------

# Pacotes necessários para reproduzir o código
# install.packages("tidyverse")

# Leitura da base de dados
dados <- readxl::read_excel("dados_cvm.xlsx")

# Formatação dos valores
format_vl_multa <- scales::dollar_format(
  prefix = "R$ ",
  big.mark = ".",
  decimal.mark = ","
)

#
descritiva_multa <- dados |>
  dplyr::summarise(
    `Mínimo` = quantile(multa, probs = 0),
    `1º Quartil` = quantile(multa, probs = 0.25),
    `Mediana` = quantile(multa, probs = 0.5),
    `Média` = mean(multa),
    `3º Quartil` = quantile(multa, probs = 0.75),
    `Máximo` = quantile(multa, probs = 1)
  ) |>
  dplyr::mutate(dplyr::across(.fns = format_vl_multa)) |>
  tidyr::pivot_longer(dplyr::everything())

descritiva_multa
