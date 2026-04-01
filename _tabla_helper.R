# _tabla_helper.R ─────────────────────────────────────────────────────────────
# tabla_fmt(): tabla numerada con caption para bookdown::gitbook Y word_document2
#
# MECANISMO:
#   Word  → flextable  (estilo booktabs, Times New Roman)
#   HTML  → knitr::kable(format="pipe") puro, SIN kable_styling()
#
# Por qué sin kable_styling():
#   kable(format="pipe") produce markdown que bookdown envuelve con el caption
#   de tab.cap= (Cuadro N.M). kable_styling() convierte ese markdown a HTML
#   y añade su propio <caption> con el mismo texto → duplicado "Cuadro N.M:
#   Cuadro N.M: título". Eliminando kable_styling() el caption aparece una
#   sola vez, correctamente numerado, y \@ref() se resuelve en ambos formatos.
#
# Uso:
#   ```{r tab-mi-tabla, tab.cap="Título de la tabla"}
#   mis_datos %>% tabla_fmt()
#   ```
# ─────────────────────────────────────────────────────────────────────────────

tabla_fmt <- function(df, digits = 3, col_names = NA) {

  fmt     <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  is_word <- !is.null(fmt) && fmt %in% c("docx", "odt")

  # Redondear columnas numéricas
  num_cols <- which(sapply(df, is.numeric))
  if (length(num_cols) > 0)
    df[num_cols] <- lapply(df[num_cols], round, digits)

  cn    <- if (identical(col_names, NA)) colnames(df) else col_names
  ncols <- ncol(df)
  align <- c("l", rep("c", max(ncols - 1L, 0L)))

  if (is_word) {
    # ── Word: flextable ───────────────────────────────────────────────────────
    library(flextable)
    library(officer)

    flextable(df) %>%
      border_remove() %>%
      hline_top(    border = fp_border(width = 1.5), part = "header") %>%
      hline_bottom( border = fp_border(width = 1.5), part = "header") %>%
      hline_bottom( border = fp_border(width = 1.5), part = "body"  ) %>%
      font(fontname = "Times New Roman", part = "all") %>%
      fontsize(size = 10, part = "all") %>%
      bold(part = "header") %>%
      align(align = "center", part = "all") %>%
      align(j = 1, align = "left", part = "body") %>%
      autofit() %>%
      set_table_properties(width = 1, layout = "autofit")

  } else {
    # ── HTML / gitbook: kable "pipe" puro ────────────────────────────────────
    # SIN kable_styling(): kable_styling convierte el markdown a HTML y añade
    # su propio <caption>, duplicando el que bookdown ya inserta desde tab.cap=.
    library(knitr)

    knitr::kable(df,
                 format    = "pipe",
                 col.names = cn,
                 digits    = digits,
                 align     = align)
  }
}
