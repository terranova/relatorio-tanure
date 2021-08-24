# --------------------------------------------------------------------
# Análises para Parecer Tanure
# Autores: Bruno Daleffi, Caio Lente, Julio Trecenti e Nicole Luduvice
# Descrição: Waffle-plot da análise qualitativa
# --------------------------------------------------------------------

# Pacotes necessários
# install.packages("tidyverse")
# install.packages("waffle", repos = "https://cinc.rud.is")

da_quali <- tibble::tribble(
  ~tipo, ~n,
  "Suspensão", 2,
  "Inabilitação", 4,
  "Apenas multa", 44
)

p_quali <- da_quali |>
  dplyr::arrange(tipo) |>
  ggplot2::ggplot() +
  ggplot2::aes(fill = tipo, values = n) +
  waffle::geom_waffle(
    n_rows = 4,
    size = 0.33,
    colour = "white",
    flip = FALSE,
    alpha = .9,
    radius = grid::unit(.2, "npc")
  ) +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_manual(
    values = c("#524243", "#007A74", "#E17605")
  ) +
  ggplot2::theme_void() +
  ggplot2::labs(fill = "") +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  "p_quali.png",
  p_quali,
  width = 6,
  height = 2,
  dpi = 400,
  bg = "white"
)
