# --------------------------------------------------------------------
# Análises para Parecer Tanure
# Autores: Bruno Daleffi, Caio Lente, Julio Trecenti e Nicole Luduvice
# Descrição: Waffle-plot da análise quantitativa
# --------------------------------------------------------------------

# Pacotes necessários para reproduzir o código
# install.packages("tidyverse")
# install.packages("patchwork")

dados <- readxl::read_excel("dados_cvm.xlsx")

# Formatação dos valores
format_vl_multa <- scales::dollar_format(
  prefix = "R$ ",
  big.mark = ".",
  decimal.mark = ","
)

# Ajuste nos nomes e valores
descritiva <- dados |>
  dplyr::rename(
    `Houve agravante` = houve_agravante,
    `Houve TC prévio` = teve_tc,
    Reincidente = reincidente
  ) |>
  dplyr::mutate(
    `Tipo de pessoa` = dplyr::case_when(
      pfpj == "PJ" ~ "Jurídica",
      TRUE ~ "Física"
    )
  )

#' Função que monta o gráfico lollipop
#'
#' @param explicativa nome da coluna a ser analisada
#' @param caption indicador se deve ou não ter o texto de explicação
#' @param descritiva base de dados analisada
montar_grafico <- function(explicativa, caption, descritiva) {
  cap <- paste(
    "Dados: Associação Brasileira de Jurimetria",
    "Gráfico: Terranova Consultoria",
    sep = "\n"
  )

  descritiva |>
    dplyr::group_by(.data[[explicativa]]) |>
    dplyr::summarise(mediana = median(multa)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[explicativa]], y = mediana) +
    ggplot2::geom_segment(ggplot2::aes(
      xend = .data[[explicativa]], y = 0, yend = mediana
    )) +
    ggplot2::geom_point(colour = "#007A74", size = 5) +
    ggplot2::scale_y_continuous(
      labels = format_vl_multa,
      breaks = scales::pretty_breaks(3),
      limits = c(0, 200000)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = explicativa,
      y = "Valor mediano da multa",
      x = "",
      caption = ifelse(caption, cap, "")
    ) +
    ggplot2::coord_flip()
}

explicativas <- c(
  "Houve agravante",
  "Houve TC prévio",
  "Tipo de pessoa",
  "Reincidente"
)

p_descritiva <- explicativas |>
  purrr::map(~ montar_grafico(.x, .x == "Reincidente", descritiva)) |>
  patchwork::wrap_plots(ncol = 2)

ggplot2::ggsave(
  "p_descritiva.png",
  p_descritiva,
  width = 8,
  height = 5,
  dpi = 400,
  bg = "white"
)
