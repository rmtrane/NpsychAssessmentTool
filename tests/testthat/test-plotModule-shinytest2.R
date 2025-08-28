library(shinytest2)

test_that("plotModule works", {
  skip_on_cran()
  skip_on_ci()
  skip_if(is.null(getOption("panda_api_key")))

  app <- AppDriver$new(
    app_dir = plotApp(testing = TRUE),
    variant = platform_variant(),
    name = "plotModule",
    height = 968,
    width = 1619
  )

  app$wait_for_idle(timeout = 20000)

  app$expect_screenshot()

  app$set_inputs(
    `Executive Functioning-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(`Mood-showPlot` = "yes", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  app$set_window_size(width = 1619, height = 968)
  # Update output value
  app$set_inputs(studyid = "NACC012002")

  app$wait_for_idle()

  app$set_inputs(
    `plotly_afterplot-General Cognition` = "\"General Cognition-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )

  app$set_inputs(
    `plotly_afterplot-General Cognition` = "\"General Cognition-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )

  app$set_inputs(
    `General Cognition-MOCATOTS_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )

  # fmt: skip
  app$set_inputs(
    `General Cognition-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "MoCA", 15, "MOCATOTS", TRUE, "MoCA", 16, "MOCATOTS", TRUE),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(`plotly_afterplot-General Cognition` = "\"General Cognition-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-General Cognition` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-04-01\",\"2018-10-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-21.97737325842806,\"maxallowed\":21.97737325842806,\"range\":[-2.55,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-TRAILA_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-DIGFORCT_visibility` = "legendonly", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-DIGFORSL_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-DIGBACCT_visibility` = "legendonly", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-DIGBACLS_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Trailmaking Part A", 15, "TRAILA", TRUE, "Trailmaking Part A", 16, "TRAILA", TRUE, "Number Span Forward - Total", 17, "DIGFORCT", "legendonly", "Number Span Forward - Total", 18, "DIGFORCT", "legendonly", "Number Span Forward - Span Length", 19, "DIGFORSL", TRUE, "Number Span Forward - Span Length", 20, "DIGFORSL", TRUE, "Number Span Backward - Total", 21, "DIGBACCT", "legendonly", "Number Span Backward - Total", 22, "DIGBACCT", "legendonly", "Number Span Backward - Span Length", 23, "DIGBACLS", TRUE, "Number Span Backward - Span Length", 24, "DIGBACLS", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Attention/Processing` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-04-01\",\"2018-10-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-11.5973261887968,\"maxallowed\":11.5973261887968,\"range\":[-11.30660265578259,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Language` = "\"Language-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Language` = "\"Language-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-MINTTOTS_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-ANIMALS_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-VEG_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-UDSVERTN_visibility` = "legendonly", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-UDSVERFC_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-UDSVERLC_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "MINT", 15, "MINTTOTS", TRUE, "MINT", 16, "MINTTOTS", TRUE, "Animal Fluency", 17, "ANIMALS", TRUE, "Animal Fluency", 18, "ANIMALS", TRUE, "Vegetable Fluency", 19, "VEG", TRUE, "Vegetable Fluency", 20, "VEG", TRUE, "F+L Words", 21, "UDSVERTN", "legendonly", "F+L Words", 22, "UDSVERTN", "legendonly", "F Words", 23, "UDSVERFC", TRUE, "F Words", 24, "UDSVERFC", TRUE, "L Words", 25, "UDSVERLC", TRUE, "L Words", 26, "UDSVERLC", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Language` = "\"Language-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Language` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-04-01\",\"2018-10-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-8.855835844601796,\"maxallowed\":8.855835844601796,\"range\":[-2.55,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Visuospatial-UDSBENTC_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Visuospatial-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Benson Figure Copy", 15, "UDSBENTC", TRUE, "Benson Figure Copy", 16, "UDSBENTC", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Visuospatial` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-04-01\",\"2018-10-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-11.81518343430822,\"maxallowed\":11.81518343430822,\"range\":[-4.326729566403935,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Memory` = "\"Memory-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Memory` = "\"Memory-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-UDSBENTD_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-CRAFTVRS_visibility` = "legendonly", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-CRAFTURS_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-CRAFTDVR_visibility` = "legendonly", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-CRAFTDRE_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Benson Delay", 15, "UDSBENTD", TRUE, "Benson Delay", 16, "UDSBENTD", TRUE, "Craft Immediate - Verbatim", 17, "CRAFTVRS", "legendonly", "Craft Immediate - Verbatim", 18, "CRAFTVRS", "legendonly", "Craft Immediate - Paraphrase", 19, "CRAFTURS", TRUE, "Craft Immediate - Paraphrase", 20, "CRAFTURS", TRUE, "Craft Delay - Verbatim", 21, "CRAFTDVR", "legendonly", "Craft Delay - Verbatim", 22, "CRAFTDVR", "legendonly", "Craft Delay - Paraphrase", 23, "CRAFTDRE", TRUE, "Craft Delay - Paraphrase", 24, "CRAFTDRE", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Memory` = "\"Memory-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Memory` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-04-01\",\"2018-10-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-4.362600596975307,\"maxallowed\":4.362600596975307,\"range\":[-3.511787237507306,2.687385522421333]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Executive Functioning-TRAILB_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Executive Functioning-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Trailmaking Part B", 15, "TRAILB", TRUE, "Trailmaking Part B", 16, "TRAILB", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Executive Functioning` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2018-01-01\",\"2018-10-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-6.538649286204794,\"maxallowed\":6.538649286204794,\"range\":[-2.55,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Mood` = "\"Mood-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Mood-showPlot` = "no", allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # Update output value
  app$expect_screenshot()

  # fmt: skip
  app$set_inputs(`plotly_hover-General Cognition` = "[{\"curveNumber\":16,\"pointNumber\":0,\"x\":\"2007-07-27\",\"y\":-0.9277594317856837}]", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_hover-General Cognition` = character(0), allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_hover-General Cognition` = "[{\"curveNumber\":16,\"pointNumber\":0,\"x\":\"2007-07-27\",\"y\":-0.9277594317856837}]", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_hover-General Cognition` = character(0), allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Attention/Processing` = "{\"yaxis.range[0]\":-2.616868786901982,\"yaxis.range[1]\":2.4912855819670234}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_window_size(width = 1619, height = 968)
  app$set_inputs(
    `plotly_hover-Attention/Processing` = "[{\"curveNumber\":20,\"pointNumber\":0,\"x\":\"2007-07-27\",\"y\":1.120683184032226}]",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(`plotly_hover-Attention/Processing` = character(0), allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_click-Attention/Processing` = character(0), allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_doubleclick-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Attention/Processing` = "{\"xaxis.range[0]\":\"2004-10-01\",\"xaxis.range[1]\":\"2025-04-01\",\"yaxis.autorange\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_legendclick-Attention/Processing` = "[{\"type\":\"scatter\",\"mode\":\"lines\",\"x\":[\"2018-06-06\"],\"y\":[1.821154956940917],\"text\":[\"Number Span Forward - Total\"],\"line\":{\"color\":\"#33A02C\",\"line\":{\"color\":\"#33A02C\"}},\"name\":\"Number Span Forward - Total\",\"visible\":\"legendonly\",\"legendgroup\":\"Number Span Forward - Total\",\"customdata\":\"DIGFORCT\",\"hovertemplate\":\"<span style=\\\"font-weight:bold;\\\">%{text}</span><br>Visit Date: %{x}<br>Scores:<br> - raw: 12<br> - std: %{y:.2f}<extra><br></extra>\"},{\"type\":\"scatter\",\"mode\":\"markers\",\"x\":[\"2018-06-06\"],\"y\":[1.821154956940917],\"text\":[\"Number Span Forward - Total\"],\"marker\":{\"color\":\"#33A02C\",\"symbol\":\"o\",\"size\":8},\"name\":\"Number Span Forward - Total\",\"visible\":\"legendonly\",\"showlegend\":false,\"legendgroup\":\"Number Span Forward - Total\",\"customdata\":\"DIGFORCT\",\"hovertemplate\":\"<span style=\\\"font-weight:bold\\\">%{text}</span><br>Visit Date: %{x}<br>Scores:<br> - raw: 12<br> - std: %{y:.2f}<extra><br></extra>\"}]", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-DIGFORCT_visibility` = TRUE, allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Trailmaking Part A", 15, "TRAILA", TRUE, "Trailmaking Part A", 16, "TRAILA", TRUE, "Number Span Forward - Total", 17, "DIGFORCT", TRUE, "Number Span Forward - Total", 18, "DIGFORCT", TRUE, "Number Span Forward - Span Length", 19, "DIGFORSL", TRUE, "Number Span Forward - Span Length", 20, "DIGFORSL", TRUE, "Number Span Backward - Total", 21, "DIGBACCT", "legendonly", "Number Span Backward - Total", 22, "DIGBACCT", "legendonly", "Number Span Backward - Span Length", 23, "DIGBACLS", TRUE, "Number Span Backward - Span Length", 24, "DIGBACLS", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip

  app$expect_screenshot()

  app$set_inputs(studyid = "NACC093132")
  app$wait_for_idle()
  app$expect_screenshot()

  # fmt: skip
  app$set_inputs(`plotly_afterplot-General Cognition` = "\"General Cognition-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`General Cognition-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-General Cognition` = "\"General Cognition-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`General Cognition-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "MoCA", 15, "MOCATOTS", TRUE, "MoCA", 16, "MOCATOTS", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-General Cognition` = "\"General Cognition-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-General Cognition` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2015-10-01\",\"2021-07-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-21.97737325842806,\"maxallowed\":21.97737325842806,\"range\":[-2.55,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Attention/Processing-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Trailmaking Part A", 15, "TRAILA", TRUE, "Trailmaking Part A", 16, "TRAILA", TRUE, "Number Span Forward - Total", 17, "DIGFORCT", TRUE, "Number Span Forward - Total", 18, "DIGFORCT", TRUE, "Number Span Forward - Span Length", 19, "DIGFORSL", TRUE, "Number Span Forward - Span Length", 20, "DIGFORSL", TRUE, "Number Span Backward - Total", 21, "DIGBACCT", "legendonly", "Number Span Backward - Total", 22, "DIGBACCT", "legendonly", "Number Span Backward - Span Length", 23, "DIGBACLS", TRUE, "Number Span Backward - Span Length", 24, "DIGBACLS", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Attention/Processing` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2015-10-01\",\"2021-07-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-11.5973261887968,\"maxallowed\":11.5973261887968,\"range\":[-4.15889531042212,2.613295177431199]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Language` = "\"Language-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Language` = "\"Language-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Language-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "MINT", 15, "MINTTOTS", TRUE, "MINT", 16, "MINTTOTS", TRUE, "Animal Fluency", 17, "ANIMALS", TRUE, "Animal Fluency", 18, "ANIMALS", TRUE, "Vegetable Fluency", 19, "VEG", TRUE, "Vegetable Fluency", 20, "VEG", TRUE, "F+L Words", 21, "UDSVERTN", "legendonly", "F+L Words", 22, "UDSVERTN", "legendonly", "F Words", 23, "UDSVERFC", TRUE, "F Words", 24, "UDSVERFC", TRUE, "L Words", 25, "UDSVERLC", TRUE, "L Words", 26, "UDSVERLC", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Language` = "\"Language-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Language` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2015-10-01\",\"2021-07-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-8.855835844601796,\"maxallowed\":8.855835844601796,\"range\":[-9.03295256149383,3.202932372635507]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Visuospatial-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Visuospatial-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Benson Figure Copy", 15, "UDSBENTC", TRUE, "Benson Figure Copy", 16, "UDSBENTC", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Visuospatial` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2015-10-01\",\"2021-07-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-11.81518343430822,\"maxallowed\":11.81518343430822,\"range\":[-12.05148710299439,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Memory` = "\"Memory-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Memory` = "\"Memory-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Memory-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Benson Delay", 15, "UDSBENTD", TRUE, "Benson Delay", 16, "UDSBENTD", TRUE, "Craft Immediate - Verbatim", 17, "CRAFTVRS", "legendonly", "Craft Immediate - Verbatim", 18, "CRAFTVRS", "legendonly", "Craft Immediate - Paraphrase", 19, "CRAFTURS", TRUE, "Craft Immediate - Paraphrase", 20, "CRAFTURS", TRUE, "Craft Delay - Verbatim", 21, "CRAFTDVR", "legendonly", "Craft Delay - Verbatim", 22, "CRAFTDVR", "legendonly", "Craft Delay - Paraphrase", 23, "CRAFTDRE", TRUE, "Craft Delay - Paraphrase", 24, "CRAFTDRE", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Memory` = "\"Memory-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Memory` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2015-10-01\",\"2021-07-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-4.362600596975307,\"maxallowed\":4.362600596975307,\"range\":[-3.606959485405861,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Executive Functioning-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`Executive Functioning-TraceMapping` = c("", 0, "", "", "dashed", 1, "", "", "#D7302733", 2, "", TRUE, "dashed", 3, "", "", "#FC8D5933", 4, "", TRUE, "dashed", 5, "", "", "#FEE08B33", 6, "", TRUE, "dashed", 7, "", "", "#FFFFBF33", 8, "", TRUE, "dashed", 9, "", "", "#D9EF8B33", 10, "", TRUE, "dashed", 11, "", "", "#91CF6033", 12, "", TRUE, "dashed", 13, "", "", "#1A985033", 14, "", TRUE, "Trailmaking Part B", 15, "TRAILB", TRUE, "Trailmaking Part B", 16, "TRAILB", TRUE), allow_no_input_binding_ = TRUE, wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"", allow_no_input_binding_ = TRUE, priority_ = "event", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`plotly_relayout-Executive Functioning` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2015-10-01\",\"2021-07-01\"],\"minallowed\":\"2004-10-01\",\"maxallowed\":\"2025-04-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-6.538649286204794,\"maxallowed\":6.538649286204794,\"range\":[-2.55,2.55]},\"showlegend\":true}", allow_no_input_binding_ = TRUE, priority_ = "event")

  app$set_inputs(
    `plotly_afterplot-Mood` = "\"Mood-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  # Update output value
  app$expect_screenshot()

  app$stop()
})
