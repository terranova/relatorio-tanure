# --------------------------------------------------------------------
# Análises para Parecer Tanure
# Autores: Bruno Daleffi, Caio Lente, Julio Trecenti e Nicole Luduvice
# Descrição: Waffle-plot da análise quantitativa
# --------------------------------------------------------------------

# Pacotes necessários para reproduzir o código
# install.packages("tidyverse")
# install.packages("waffle", repos = "https://cinc.rud.is")

dados <- readxl::read_excel("dados_cvm.xlsx")

vl_caso <- 14.4e6

p_waffle_quanti <- dados |>
  tibble::add_row(multa = vl_caso) |>
  dplyr::mutate(maior = dplyr::case_when(
    multa > vl_caso ~ "Maior",
    multa == vl_caso ~ "Caso em discussão",
    TRUE ~ "Menor"
  )) |>
  dplyr::mutate(maior = factor(maior, levels = c(
    "Menor", "Caso em discussão", "Maior"
  ))) |>
  dplyr::count(maior) |>
  ggplot2::ggplot() +
  ggplot2::aes(fill = maior, values = n) +
  waffle::geom_waffle(
    n_rows = 39,
    size = 0.33,
    colour = "white",
    flip = FALSE,
    radius = grid::unit(.2, "npc")
  ) +
  ggplot2::scale_alpha_manual(values = c(.1, 1, 1)) +
  ggplot2::scale_fill_manual(
    values = c("#bbbbbb", "#E17605", "#007A74"),
    labels = c("Menor", "R$ 14.400.000,00", "Maior")
  ) +
  ggplot2::coord_equal() +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "bottom") +
  waffle::theme_enhance_waffle() +
  ggplot2::labs(fill = "")

ggplot2::ggsave(
  "p_waffle_quanti.png",
  p_waffle_quanti,
  width = 6,
  height = 4,
  dpi = 400,
  bg = "white"
)
