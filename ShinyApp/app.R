#### DRUID D1: Shiny App — Crop Yield Loss Explorer ############################
# Author: Dr Peter King (p.king1@Leeds.ac.uk)
# Purpose: Interactive tool based on King et al. (2024). Allows readers to
#          explore how pest pressure, natural enemy presence, and crop economics
#          interact to determine yield losses for Barley, Wheat, and OSR.

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(plotly)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ── Historical defaults from market data (2011-2021) ─────────────────────────

parse_crop_defaults <- function(raw, crop_name, price_col, sprays_col, insect_col) {
  crop <- raw[raw$Crop == crop_name, ]
  yr   <- colnames(crop)[3:51] |> gsub(pattern = "X", replacement = "") |> as.numeric()
  colnames(crop) <- c("Crop", "Measure", yr)
  crop_trim <- crop[, c(1:2, 41:51)]
  df        <- as.data.frame(t(crop_trim[, 3:13]), stringsAsFactors = FALSE)
  colnames(df) <- crop_trim$Measure
  needed <- intersect(c("Area", "Yield", price_col, sprays_col, insect_col), colnames(df))
  df <- df |> mutate(across(all_of(needed), as.numeric))
  list(
    yield  = round(mean(df$Yield,         na.rm = TRUE), 2),
    price  = round(mean(df[[price_col]],  na.rm = TRUE), 0),
    sprays = round(mean(df[[sprays_col]], na.rm = TRUE), 0),
    insect = round(mean(df[[insect_col]], na.rm = TRUE), 0)
  )
}

raw_data <- readxl::read_xlsx("data/AllCropData_V2.xlsx", sheet = "Sheet1") |>
  as.data.frame()

CROP_DEFAULTS <- list(
  Barley = parse_crop_defaults(raw_data, "Barley",
                               "Malting barley real", "MaltingSpraysCostsReal",
                               "InsecticidesOnlyReal"),
  OSR    = parse_crop_defaults(raw_data, "OSR",
                               "PriceReal", "WinterSpraysCostsReal",
                               "InsecticidesOnlyReal"),
  Wheat  = parse_crop_defaults(raw_data, "Wheat",
                               "Milling wheat real", "MillingSprayCostsReal",
                               "InsecticidesOnlyReal")
)

# ── REA distributions — pre-computed once at startup ─────────────────────────
# Reduction in Effective Aphids (REA) captures the pest-suppression effect of
# natural enemies. Crop-specific, derived from published field experiments.

set.seed(42)
DIST_N <- 500

REA_DIST <- list(
  Barley = {
    ctrl <- rnorm(DIST_N, 67.8,  16.07 * sqrt(72))
    trt  <- rnorm(DIST_N, 32.2,  10.38 * sqrt(72))
    pmin(pmax(abs((trt - ctrl) / ctrl), 0), 1)
  },
  OSR = {
    ctrl <- rnorm(DIST_N, 137.9444, 0.394268819)
    trt  <- rnorm(DIST_N,  77.1667, 0.227835165)
    pmin(pmax(abs((trt - ctrl) / ctrl), 0), 1)
  },
  Wheat = {
    ctrl <- rnorm(DIST_N, 366, 27.0  * sqrt(32))
    trt  <- rnorm(DIST_N,  83,  7.60 * sqrt(32))
    pmin(pmax(abs((trt - ctrl) / ctrl), 0), 1)
  }
)

# ── Simulation constants ──────────────────────────────────────────────────────

NE_VALS     <- c(0.01, seq(0.1, 1.0, by = 0.1))  # 11 levels
DENSITIES   <- c(5, 7.5, 10)
DENSITY_LAB <- c("Low (5)", "Medium (7.5)", "High (10)")

col_ne      <- rep(NE_VALS,   times = length(DENSITIES))
col_density <- rep(DENSITIES, each  = length(NE_VALS))
N_COMBOS    <- length(col_ne)                         # 33

# Column index of the NE=1.0 scenario within each density group
MAX_NE_IDX  <- c(11L, 22L, 33L)

# ── Core simulation ───────────────────────────────────────────────────────────

run_simulation <- function(crop, price, yield_tha, sprays,
                           threshold, field_ns, field_et) {

  field_as   <- max(0, 1 - field_ns - field_et)
  rea        <- REA_DIST[[crop]]
  yield_kgha <- yield_tha * 1000
  value_ha   <- yield_tha * price
  insect_only <- CROP_DEFAULTS[[crop]]$insect  # used in Wheat ET

  lph_ns_mat <- matrix(0, DIST_N, N_COMBOS)
  lph_et_mat <- matrix(0, DIST_N, N_COMBOS)
  lph_as_mat <- matrix(sprays, DIST_N, N_COMBOS)
  yl_ns_mat  <- matrix(0, DIST_N, N_COMBOS)
  yl_et_mat  <- matrix(0, DIST_N, N_COMBOS)

  for (i in seq_len(N_COMBOS)) {
    eff <- pmax(col_density[i] * (1 - rea * col_ne[i]), 0.001)

    # ── Never Spray ──────────────────────────────────────────────────────────
    if (crop == "Barley") {
      yl_new    <- (eff * 12.70)^0.65
      yl_change <- pmax(yield_kgha - yl_new, 0.001)
      yl_ns     <- pmax((yl_new / yl_change) * 100, 0)
      yl_ns[is.infinite(yl_ns)] <- 0
      lph_ns <- value_ha * yl_ns

    } else if (crop == "Wheat") {
      yl_ns <- pmax(4.5 * log(eff) - 5.5, 0)
      yl_ns[is.infinite(yl_ns)] <- 0
      lph_ns <- value_ha * (yl_ns / 100)

    } else {  # OSR
      effects <- pmin(pmax(rnorm(DIST_N, 0.132, 0.099), 0), 1)
      yl_ns   <- pmax(eff * (1 - effects) / 100, 0)
      lph_ns  <- value_ha * yl_ns
    }

    # ── Economic Threshold ────────────────────────────────────────────────────
    # Draws with eff >= threshold → farmer sprays → no yield loss, pays sprays.
    # Draws with eff <  threshold → no spray     → yield loss occurs.

    eff_et            <- eff
    eff_et[eff >= threshold] <- 0

    if (crop == "Barley") {
      yl_new_et    <- (eff_et * 12.70)^0.65
      yl_change_et <- pmax(yield_kgha - yl_new_et, 0.001)
      yl_et        <- (yl_new_et / yl_change_et) * 100
      yl_et[is.infinite(yl_et) | yl_et < 0] <- 0.0000001
      lph_et <- ifelse(yl_et <= 0.0000001, sprays, value_ha * yl_et)
      yl_et[yl_et == 0.0000001] <- 0

    } else if (crop == "Wheat") {
      yl_et <- 4.5 * log(pmax(eff_et, 0.001)) - 5.5
      yl_et[is.infinite(yl_et) | yl_et < 0] <- 0.0000001
      lph_et <- ifelse(yl_et <= 0.0000001,
                       sprays,
                       (yl_et / 100) * value_ha + insect_only)
      yl_et[yl_et == 0.0000001] <- 0

    } else {  # OSR
      effects_et <- pmin(pmax(rnorm(DIST_N, 0.132, 0.099), 0), 1)
      yl_et      <- pmax(eff_et * (1 - effects_et) / 100, 0)
      yl_et[yl_et < 0] <- 0.0000001
      lph_et <- ifelse(yl_et <= 0.0000001, sprays, value_ha * yl_et)
      yl_et[yl_et == 0.0000001] <- 0
    }

    lph_ns_mat[, i] <- lph_ns
    lph_et_mat[, i] <- lph_et
    yl_ns_mat[, i]  <- yl_ns
    yl_et_mat[, i]  <- yl_et
  }

  # ── Summary stats ─────────────────────────────────────────────────────────

  lph_ns_mean <- colMeans(lph_ns_mat)
  lph_et_mean <- colMeans(lph_et_mat)
  lph_as_mean <- rep(sprays, N_COMBOS)
  lph_ns_sd   <- apply(lph_ns_mat, 2, sd)
  lph_et_sd   <- apply(lph_et_mat, 2, sd)
  lph_as_sd   <- rep(0, N_COMBOS)

  # Yield loss as % of crop value (LPH / value_ha × 100).
  # This gives a consistent, interpretable percentage across all three crops.
  yl_ns_mean <- colMeans(lph_ns_mat) / value_ha * 100
  yl_et_mean <- colMeans(lph_et_mat) / value_ha * 100
  yl_as_mean <- rep(sprays / value_ha * 100, N_COMBOS)
  yl_ns_sd   <- apply(lph_ns_mat, 2, sd) / value_ha * 100
  yl_et_sd   <- apply(lph_et_mat, 2, sd) / value_ha * 100
  yl_as_sd   <- rep(0, N_COMBOS)

  # ── Change in LPH vs full NE (NE=1.0) reference ──────────────────────────
  # (LPH at NE=1.0) − (LPH at current NE): negative = more loss than at full NE.

  clph_ns_mean <- clph_et_mean <- clph_as_mean <- numeric(N_COMBOS)
  clph_ns_sd   <- clph_et_sd   <- clph_as_sd   <- numeric(N_COMBOS)

  for (g in seq_along(DENSITIES)) {
    ref <- MAX_NE_IDX[g]
    for (j in seq_len(length(NE_VALS))) {
      idx <- (g - 1L) * length(NE_VALS) + j
      dn  <- lph_ns_mat[, ref] - lph_ns_mat[, idx]
      de  <- lph_et_mat[, ref] - lph_et_mat[, idx]
      da  <- lph_as_mat[, ref] - lph_as_mat[, idx]
      clph_ns_mean[idx] <- mean(dn); clph_ns_sd[idx] <- sd(dn)
      clph_et_mean[idx] <- mean(de); clph_et_sd[idx] <- sd(de)
      clph_as_mean[idx] <- mean(da); clph_as_sd[idx] <- sd(da)
    }
  }

  # ── Assemble tidy frames ───────────────────────────────────────────────────

  make_long <- function(ns_m, et_m, as_m, ns_s, et_s, as_s, bound_fn) {
    data.frame(Density = col_density, NE = col_ne,
               NS_Mean = ns_m, NS_SD = ns_s,
               ET_Mean = et_m, ET_SD = et_s,
               AS_Mean = as_m, AS_SD = as_s) |>
      pivot_longer(cols = -c(Density, NE),
                   names_to  = c("Type", ".value"),
                   names_sep = "_") |>
      mutate(
        Density = factor(Density, levels = DENSITIES, labels = DENSITY_LAB),
        Ymin    = bound_fn(Mean - SD),
        Ymax    = bound_fn(Mean + SD)
      )
  }

  list(
    lph  = make_long(lph_ns_mean,  lph_et_mean,  lph_as_mean,
                     lph_ns_sd,   lph_et_sd,   lph_as_sd,   function(x) pmax(x, 0)),
    yl   = make_long(yl_ns_mean,   yl_et_mean,   yl_as_mean,
                     yl_ns_sd,   yl_et_sd,   yl_as_sd,   function(x) pmax(x, 0)),
    clph = make_long(clph_ns_mean, clph_et_mean, clph_as_mean,
                     clph_ns_sd,  clph_et_sd,  clph_as_sd,  function(x) pmin(x, 0))
  )
}

# ── Plot helpers ──────────────────────────────────────────────────────────────

SCENARIO_COLS  <- c("Default" = "#2166ac", "Custom" = "#d6604d")
SCENARIO_LINES <- c("Default" = "solid",   "Custom" = "dashed")
SCENARIO_SHAPE <- c("Default" = 16L,        "Custom" = 1L)

app_theme <- function() {
  theme_bw(base_size = 13) +
    theme(
      strip.background  = element_rect(fill = "white"),
      panel.grid        = element_blank(),
      legend.position   = "bottom",
      legend.background = element_blank(),
      panel.spacing     = unit(0.75, "cm"),
      plot.title        = element_text(size = 11, colour = "grey40")
    )
}

# ggplotly creates one trace per facet, giving duplicate legend entries — collapse them.
dedupe_legend <- function(p_ly) {
  seen <- character(0)
  for (i in seq_along(p_ly$x$data)) {
    nm <- p_ly$x$data[[i]]$name
    if (!is.null(nm) && nchar(nm) > 0) {
      if (nm %in% seen) p_ly$x$data[[i]]$showlegend <- FALSE
      else seen <- c(seen, nm)
    }
  }
  p_ly
}

make_plot <- function(dat_default, y_lab, type_filter, crop_title,
                      density_val, show_ci = TRUE,
                      dat_custom = NULL, y_format = NULL) {

  dens_lbl  <- DENSITY_LAB[match(density_val, DENSITIES)]
  fmt_fn    <- if (!is.null(y_format)) y_format else function(x) round(x, 2)
  type_full <- c(NS = "Never Spray", ET = "Economic Threshold", AS = "Always Spray")

  prep <- function(df, scenario) {
    df |>
      filter(Type %in% type_filter, Density == dens_lbl) |>
      mutate(
        Scenario = scenario,
        text     = paste0(
          "<b>", type_full[Type], "</b> [", scenario, "]<br>",
          "NE: ",      scales::percent(NE, accuracy = 1), "<br>",
          "Mean: ",    fmt_fn(Mean), "<br>",
          "±SD: ", fmt_fn(SD)
        )
      )
  }

  pd <- prep(dat_default, "Default")

  p <- ggplot(pd, aes(x = NE, group = interaction(Type, Scenario)))

  if (show_ci)
    p <- p + geom_ribbon(aes(ymin = Ymin, ymax = Ymax, fill = Scenario),
                         alpha = 0.2, show.legend = FALSE)

  p <- p +
    geom_line( aes(y = Mean, colour = Scenario, linetype = Scenario, text = text),
               linewidth = 1) +
    geom_point(aes(y = Mean, colour = Scenario, shape    = Scenario, text = text),
               size = 2)

  if (!is.null(dat_custom)) {
    pd_c <- prep(dat_custom, "Custom")
    if (show_ci)
      p <- p + geom_ribbon(data = pd_c,
                           aes(ymin = Ymin, ymax = Ymax, fill = Scenario,
                               group = interaction(Type, Scenario)),
                           alpha = 0.15, show.legend = FALSE)
    p <- p +
      geom_line( data = pd_c,
                 aes(y = Mean, colour = Scenario, linetype = Scenario,
                     text = text, group = interaction(Type, Scenario)),
                 linewidth = 1) +
      geom_point(data = pd_c,
                 aes(y = Mean, colour = Scenario, shape    = Scenario,
                     text = text, group = interaction(Type, Scenario)),
                 size = 2)
  }

  p <- p +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dotted", alpha = 0.5) +
    facet_wrap(~ Type, scales = "free_y",
               labeller = labeller(Type = c(
                 NS = "Never Spray",
                 ET = "Economic Threshold",
                 AS = "Always Spray"))) +
    scale_colour_manual(  name = NULL, values = SCENARIO_COLS) +
    scale_fill_manual(    name = NULL, values = SCENARIO_COLS) +
    scale_linetype_manual(name = NULL, values = SCENARIO_LINES) +
    scale_shape_manual(   name = NULL, values = SCENARIO_SHAPE) +
    guides(
      fill     = "none",
      linetype = "none",
      shape    = "none",
      colour   = guide_legend(
        override.aes = list(linetype = c("solid", "dashed"), shape = c(16L, 1L))
      )
    ) +
    scale_x_continuous(
      name   = "Natural enemy presence",
      breaks = c(0.01, 0.25, 0.50, 0.75, 1.00),
      labels = c("1%", "25%", "50%", "75%", "100%")
    ) +
    labs(y = y_lab, title = crop_title) +
    app_theme()

  if (!is.null(y_format)) p <- p + scale_y_continuous(name = y_lab, labels = y_format)
  p
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(tags$style(HTML("
    .flag-text  { color: #c0392b; font-size: 0.8em; }
    .hint-text  { color: #777;    font-size: 0.82em; margin-bottom: 6px; }
    .section-lbl { font-weight: bold; margin-top: 4px; }
    .shiny-output-error { color: #c0392b; }
  "))),

  titlePanel("DRUID D1: Crop Yield Loss Explorer"),

  tags$p(class = "hint-text",
         style = "margin-top:-8px;",
         "Based on King et al. (2025). Explore how pest pressure, natural enemy presence,",
         "and farm economics interact to determine yield losses across Barley, Wheat, and OSR.",
         tags$a(href = "https://doi.org/10.1016/j.ecoser.2025.101776",
                target = "_blank",
                "doi:10.1016/j.ecoser.2025.101776")),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput("crop", "Crop", choices = c("Barley", "Wheat", "OSR")),

      selectInput("density", "Pest density (aphids/tiller)",
                  choices  = c("Low — 5" = "5", "Medium — 7.5" = "7.5", "High — 10" = "10"),
                  selected = "7.5"),

      checkboxInput("show_ci", "Show confidence intervals", value = TRUE),

      hr(),
      tags$p(class = "section-lbl", "Custom economic parameters"),
      tags$p(class = "hint-text",
             "Defaults are 2011–2021 historical means (solid curve). Adjust to overlay your scenario (dashed)."),

      sliderInput("price",  "Crop price (£/tonne)",      min = 50,  max = 700, step = 5,   pre = "£",
                  value = CROP_DEFAULTS[["Barley"]]$price),
      uiOutput("flag_price"),
      sliderInput("yield",  "Yield (t/ha)",               min = 1,   max = 12,  step = 0.1, post = " t/ha",
                  value = CROP_DEFAULTS[["Barley"]]$yield),
      uiOutput("flag_yield"),
      sliderInput("sprays", "Full spray costs (£/ha)",    min = 0,   max = 250, step = 5,   pre = "£",
                  value = CROP_DEFAULTS[["Barley"]]$sprays),
      uiOutput("flag_sprays"),

      actionButton("btn_reset", "Reset to historical means",
                   class = "btn-sm btn-default"),

      hr(),
      tags$p(class = "section-lbl", "Model parameters"),

      sliderInput("threshold", "Economic threshold (aphids/tiller)",
                  min = 1, max = 15, value = 5, step = 0.5),

      sliderInput("field_ns", "Never Spray (%)",
                  min = 0, max = 50, value = 0.7, step = 0.1, post = "%"),

      sliderInput("field_et", "Economic Threshold (%)",
                  min = 0, max = 80, value = 37, step = 1, post = "%"),

      uiOutput("ui_field_as"),

      hr(),
      tags$p(class = "hint-text",
             "Solid = paper defaults. Dashed = your values. Hover over lines to read values.")
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        tabPanel(
          "Loss per Hectare (£/ha)",
          br(),
          plotlyOutput("plot_lph", height = "420px"),
          br(),
          tags$details(tags$summary("What is this?"),
            tags$p(class = "hint-text",
              "Estimated monetary loss per hectare (£/ha) for each management strategy",
              "as natural enemy presence varies from 1% to 100%.",
              "Never Spray (NS): all costs come from pest damage.",
              "Economic Threshold (ET): spray only when pest density exceeds the threshold.",
              "Always Spray (AS): full spray costs, no yield loss."))
        ),

        tabPanel(
          "Yield Loss (%)",
          br(),
          plotlyOutput("plot_yl", height = "420px"),
          br(),
          tags$details(tags$summary("What is this?"),
            tags$p(class = "hint-text",
              "Monetary loss as a percentage of total crop value (LPH ÷ crop value × 100).",
              "This is consistent across all three crops.",
              "Never Spray (NS): costs from pest damage only.",
              "Economic Threshold (ET): mixed — some draws pay for sprays, others lose yield.",
              "Always Spray (AS) is excluded here (it is shown in the LPH tab)."))
        ),

        tabPanel(
          "Change in LPH vs Full NE Presence",
          br(),
          plotlyOutput("plot_clph", height = "420px"),
          br(),
          tags$details(tags$summary("What is this?"),
            tags$p(class = "hint-text",
              "Change in loss per hectare compared to the 100% natural enemy scenario.",
              "Negative values = higher monetary loss than under full natural enemy presence.",
              "This metric directly quantifies the economic value of natural enemies:",
              "how much does it cost farmers when NE communities are below full capacity?"))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Dynamic input defaults per crop ──────────────────────────────────────

  observeEvent(input$crop, {
    d <- CROP_DEFAULTS[[input$crop]]
    updateSliderInput(session, "price",  value = d$price)
    updateSliderInput(session, "yield",  value = d$yield)
    updateSliderInput(session, "sprays", value = d$sprays)
  }, ignoreInit = TRUE)

  observeEvent(input$btn_reset, {
    d <- CROP_DEFAULTS[[input$crop]]
    updateSliderInput(session, "price",  value = d$price)
    updateSliderInput(session, "yield",  value = d$yield)
    updateSliderInput(session, "sprays", value = d$sprays)
  })

  def <- reactive(CROP_DEFAULTS[[input$crop]])

  flag <- function(val, ref, unit = "") {
    if (is.null(val) || is.null(ref) || length(val) == 0 || is.na(val)) return(NULL)
    if (abs(val - ref) > 0.5)
      tags$span(class = "flag-text",
                sprintf(" (historical mean: %s%s)", round(ref, 0), unit))
    else
      NULL
  }

  output$flag_price  <- renderUI(flag(input$price,  def()$price,  " £/t"))
  output$flag_yield  <- renderUI(flag(input$yield,  def()$yield,  " t/ha"))
  output$flag_sprays <- renderUI(flag(input$sprays, def()$sprays, " £/ha"))

  output$ui_field_as <- renderUI({
    as_pct <- max(0, 100 - (input$field_ns %||% 0.7) - (input$field_et %||% 37))
    tags$p(class = "hint-text",
           sprintf("Always Spray (remainder): %.1f%%", as_pct))
  })

  # ── Debounced reactive simulation ─────────────────────────────────────────
  # Collect all inputs into one reactive, debounce to avoid running on every
  # slider tick, then run the simulation.

  all_inputs <- reactive({
    list(
      crop      = input$crop,
      price     = input$price,
      yield     = input$yield,
      sprays    = input$sprays,
      threshold = input$threshold,
      field_ns  = input$field_ns,
      field_et  = input$field_et
    )
  }) |> debounce(800)

  sim_both <- reactive({
    inp <- all_inputs()

    req(inp$price, inp$yield, inp$sprays,
        inp$threshold, inp$field_ns, inp$field_et)

    validate(
      need(inp$price  > 0,  "Crop price must be positive."),
      need(inp$yield  > 0,  "Yield must be positive."),
      need(inp$sprays >= 0, "Spray cost cannot be negative."),
      need((inp$field_ns + inp$field_et) <= 100,
           "Never Spray + Economic Threshold cannot exceed 100%.")
    )

    d <- CROP_DEFAULTS[[inp$crop]]

    withProgress(message = "Running simulation…", value = 0.5, {
      list(
        default = run_simulation(
          crop      = inp$crop,
          price     = d$price,
          yield_tha = d$yield,
          sprays    = d$sprays,
          threshold = inp$threshold,
          field_ns  = inp$field_ns / 100,
          field_et  = inp$field_et / 100
        ),
        custom = run_simulation(
          crop      = inp$crop,
          price     = inp$price,
          yield_tha = inp$yield,
          sprays    = inp$sprays,
          threshold = inp$threshold,
          field_ns  = inp$field_ns / 100,
          field_et  = inp$field_et / 100
        )
      )
    })
  })

  # ── Plot subtitle with current parameter values ────────────────────────────

  plot_title <- reactive({
    inp <- all_inputs()
    req(inp$crop, inp$price, inp$yield, inp$sprays, inp$threshold)
    d <- CROP_DEFAULTS[[inp$crop]]
    sprintf("%s  |  Default: £%d/t, %.1f t/ha, £%d/ha  |  Custom: £%d/t, %.1f t/ha, £%d/ha  |  threshold: %.1f",
            inp$crop,
            as.integer(d$price),  d$yield,  as.integer(d$sprays),
            as.integer(inp$price), inp$yield, as.integer(inp$sprays),
            inp$threshold)
  })

  # ── Plots ──────────────────────────────────────────────────────────────────

  to_plotly <- function(p) {
    ggplotly(p, tooltip = "text") |>
      dedupe_legend() |>
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15))
  }

  output$plot_lph <- renderPlotly({
    req(sim_both())
    to_plotly(make_plot(sim_both()$default$lph,
                        dat_custom  = sim_both()$custom$lph,
                        density_val = as.numeric(input$density),
                        show_ci     = input$show_ci,
                        y_lab       = "Loss per hectare (£/ha)",
                        type_filter = c("NS", "ET", "AS"),
                        crop_title  = plot_title(),
                        y_format    = dollar_format(prefix = "£")))
  })

  output$plot_yl <- renderPlotly({
    req(sim_both())
    to_plotly(make_plot(sim_both()$default$yl,
                        dat_custom  = sim_both()$custom$yl,
                        density_val = as.numeric(input$density),
                        show_ci     = input$show_ci,
                        y_lab       = "Yield loss (%)",
                        type_filter = c("NS", "ET"),
                        crop_title  = plot_title(),
                        y_format    = function(x) paste0(round(x, 1), "%")))
  })

  output$plot_clph <- renderPlotly({
    req(sim_both())
    to_plotly(make_plot(sim_both()$default$clph,
                        dat_custom  = sim_both()$custom$clph,
                        density_val = as.numeric(input$density),
                        show_ci     = input$show_ci,
                        y_lab       = "Change in LPH vs full NE presence (£/ha)",
                        type_filter = c("NS", "ET"),
                        crop_title  = plot_title(),
                        y_format    = dollar_format(prefix = "£")))
  })
}

shinyApp(ui, server)
