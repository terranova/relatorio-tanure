# --------------------------------------------------------------------
# Análises para Parecer Tanure
# Autores: Bruno Daleffi, Caio Lente, Julio Trecenti e Nicole Luduvice
# Descrição: Waffle-plot da análise quantitativa
# --------------------------------------------------------------------

# Pacotes necessários para reproduzir o código
# install.packages("tidyverse")
# install.packages("MASS")
# install.packages("recipes")

# caso em análise ---------------------------------------------------------

caso <- tibble::tibble(
  reincidente_sim = 1,
  houve_agravante_sim = 1,
  teve_prazo_sim = 1,
  pfpj_pj = 0,
  teve_tc_sim = 1,
  qtd_partes = log10(3) # em escala log
)

vl_caso <- 14.4e6

# funções relacionadas à transformação box-cox ----------------------------

inversa_boxcox <- function(bc, lambda) {
  if (lambda == 0) {
    exp(bc)
  } else {
    (bc * lambda + 1)^(1 / lambda)
  }
}

transf_boxcox <- function(multa, lambda) {
  if (lambda == 0) {
    log(multa)
  } else {
    (multa^lambda - 1) / lambda
  }
}

# Leitura da base de dados
dados <- readxl::read_excel("dados_cvm.xlsx") |>
  dplyr::select(-n_processo, -link) |>
  dplyr::mutate(qtd_partes = log10(qtd_partes)) # escala log para partes

# tratamento dos dados
recp_multa <- dados |>
  recipes::recipe(multa ~ .) |>
  recipes::step_dummy(recipes::all_nominal_predictors()) |>
  recipes::prep() |>
  recipes::bake(new_data = NULL) |>
  janitor::clean_names()

# transformação boxcox
bc <- MASS::boxcox(
  object = multa ~ .,
  data = recp_multa,
  plotit = FALSE
)
lambda <- bc$x[which.max(bc$y)]

recp_multa <- recp_multa |>
  dplyr::mutate(multa = purrr::map_dbl(multa, transf_boxcox, lambda = lambda))

# Ajuste do modelo ------------------------------------------------------------

fit_multa <- glm(
  formula = multa ~ .,
  family = gaussian(),
  data = recp_multa
)

multa_pred <- predict(fit_multa, caso, type = "response") |>
  inversa_boxcox(lambda)

# predicao com erro padrão
pred_multa_se <- predict(fit_multa, newdata = caso, se.fit = TRUE)

# limite do intervalo de confiança
multa_max_l <- pred_multa_se$fit + (2 * pred_multa_se$se.fit)
multa_max <- inversa_boxcox(multa_max_l, lambda)
multa_max_lab <- scales::dollar(
  multa_max,
  big.mark = ".",
  decimal.mark = ",", prefix = "R$ ",
  accuracy = .01
)

# probabilidade do evento acontecer
prob_multa <- pnorm(
  q = transf_boxcox(vl_caso, lambda),
  mean = transf_boxcox(multa_pred[[1]], lambda),
  sd = sd(fit_multa$residuals),
  lower.tail = FALSE
)

# resultados finais
multa_max_lab
scales::percent(prob_multa, .1)
as.numeric(razao_multa <- floor(vl_caso / multa_max))


# grafico -----------------------------------------------------------------

labs <- forcats::as_factor(c(
  "Multa média",
  "Multa mediana",
  "Multa limite",
  "Multa estimada\npelo modelo",
  "Proposta\nNelson Tanure",
  "Proposta do CTC"
))

# Formatação dos valores
format_vl_multa <- scales::dollar_format(
  prefix = "R$ ",
  big.mark = ".",
  decimal.mark = ","
)

dados_plot <- tibble::tibble(
  valor = c(
    mean(dados$multa),
    median(dados$multa),
    500000,
    multa_max,
    600000,
    vl_caso
  ),
  tipo = labs
)

p_modelo <- dados_plot |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tipo, y = valor, label = format_vl_multa(valor)) +
  ggplot2::geom_col(
    width = .5,
    fill = c(rep("#524243", 3), rep("#007A74", 2), "#E17605")
  ) +
  ggplot2::geom_text(vjust = -1) +
  ggplot2::scale_y_continuous(
    labels = format_vl_multa,
    breaks = scales::pretty_breaks(10),
    limits = c(0, 16e6)
  ) +
  ggplot2::theme_minimal(14) +
  ggplot2::labs(x = "", y = "Valor")

ggplot2::ggsave(
  "p_modelo.png",
  p_modelo,
  width = 8,
  height = 5,
  dpi = 400,
  bg = "white"
)
