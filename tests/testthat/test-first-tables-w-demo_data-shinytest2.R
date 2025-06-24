library(shinytest2)

test_that("{shinytest2} First visit to 'Scoring Tables and Figures' using demo_data", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = shinyAssessmentApp(),
    variant = platform_variant(),
    name = "shinyApp",
    height = 968,
    width = 1619,
    wait = TRUE
  )

  app$set_inputs(
    `General Cognition-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Visuospatial-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Executive Functioning-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Mood-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_window_size(width = 1619, height = 968)
  app$wait_for_idle(duration = 1000)
  app$expect_values()
  app$set_inputs(main_navbar = "dataSelect")
  # Update output value
  app$set_window_size(width = 1619, height = 968)
  app$set_inputs(`dataSelect-data_source` = "demo")
  # Update output value
  app$set_inputs(`dataSelect-fetch_data_button` = "click") # c(1, 1))
  app$wait_for_idle(duration = 1000)
  # Update output value
  app$set_inputs(
    `colSelect-varstableDrawn` = "on",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$wait_for_idle(duration = 1000)
  app$set_inputs(
    `colSelect-vars_table_output_rows_current` = c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-vars_table_output_rows_all` = c(
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-vars_table_output_state` = c(
      1750189181240,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(FALSE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-MOCATOTSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-NACCMMSEmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(`colSelect-TRAILAmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colSelect-OTRAILAmethod` = "regression (updated)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGFORCTmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGFORSLmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGBACCTmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGBACLSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colSelect-WAISmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(
    `colSelect-DIGIFmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGIFLENmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGIBmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-DIGIBLENmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-MINTTOTSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colSelect-ANIMALSmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(`colSelect-VEGmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colSelect-UDSVERTNmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-UDSVERFCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-UDSVERLCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-BOSTONmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-UDSBENTCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-UDSBENTDmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-CRAFTVRSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-CRAFTURSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-CRAFTDVRmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-CRAFTDREmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colSelect-REYDLISTmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colSelect-REY6RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colSelect-REYDRECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(
    `colSelect-LOGIMEMmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-MEMUNITSmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(`colSelect-REYTOTALmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colSelect-REYARECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colSelect-TRAILBmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colSelect-OTRAILBmethod` = "regression (updated)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colSelect-assign` = "click",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    moveToTables = "click",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_window_size(width = 1619, height = 968)
  app$click("goToColSelect")
  app$set_window_size(width = 1619, height = 968)
  app$set_inputs(current_studyid = "NACC000460", wait_ = FALSE)
  # Update output value
  app$set_inputs(
    `plotly_afterplot-General Cognition` = "\"General Cognition-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-General Cognition` = "\"General Cognition-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `General Cognition-MOCATOTS_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `General Cognition-TraceMapping` = c(
      "",
      0,
      "",
      "",
      "dashed",
      1,
      "",
      "",
      "Impaired",
      2,
      "",
      TRUE,
      "dashed",
      3,
      "",
      "",
      "Borderline",
      4,
      "",
      TRUE,
      "dashed",
      5,
      "",
      "",
      "Low Average",
      6,
      "",
      TRUE,
      "dashed",
      7,
      "",
      "",
      "Average",
      8,
      "",
      TRUE,
      "dashed",
      9,
      "",
      "",
      "High Average",
      10,
      "",
      TRUE,
      "dashed",
      11,
      "",
      "",
      "Superior",
      12,
      "",
      TRUE,
      "dashed",
      13,
      "",
      "",
      "Very Superior",
      14,
      "",
      TRUE,
      "MoCA",
      15,
      "MOCATOTS",
      TRUE,
      "MoCA",
      16,
      "MOCATOTS",
      TRUE
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-General Cognition` = "\"General Cognition-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_relayout-General Cognition` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-01-01\",\"2018-04-01\"],\"minallowed\":\"2005-04-01\",\"maxallowed\":\"2024-10-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-20.33601882261425,\"maxallowed\":20.33601882261425,\"range\":[-2.55,2.55]},\"showlegend\":true}",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-TRAILA_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-DIGFORCT_visibility` = "legendonly",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-DIGFORSL_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-DIGBACCT_visibility` = "legendonly",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-DIGBACLS_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-TraceMapping` = c(
      "",
      0,
      "",
      "",
      "dashed",
      1,
      "",
      "",
      "Impaired",
      2,
      "",
      TRUE,
      "dashed",
      3,
      "",
      "",
      "Borderline",
      4,
      "",
      TRUE,
      "dashed",
      5,
      "",
      "",
      "Low Average",
      6,
      "",
      TRUE,
      "dashed",
      7,
      "",
      "",
      "Average",
      8,
      "",
      TRUE,
      "dashed",
      9,
      "",
      "",
      "High Average",
      10,
      "",
      TRUE,
      "dashed",
      11,
      "",
      "",
      "Superior",
      12,
      "",
      TRUE,
      "dashed",
      13,
      "",
      "",
      "Very Superior",
      14,
      "",
      TRUE,
      "Trailmaking Part A",
      15,
      "TRAILA",
      TRUE,
      "Trailmaking Part A",
      16,
      "TRAILA",
      TRUE,
      "Number Span Forward - Total",
      17,
      "DIGFORCT",
      "legendonly",
      "Number Span Forward - Total",
      18,
      "DIGFORCT",
      "legendonly",
      "Number Span Forward - Span Length",
      19,
      "DIGFORSL",
      TRUE,
      "Number Span Forward - Span Length",
      20,
      "DIGFORSL",
      TRUE,
      "Number Span Backward - Total",
      21,
      "DIGBACCT",
      "legendonly",
      "Number Span Backward - Total",
      22,
      "DIGBACCT",
      "legendonly",
      "Number Span Backward - Span Length",
      23,
      "DIGBACLS",
      TRUE,
      "Number Span Backward - Span Length",
      24,
      "DIGBACLS",
      TRUE
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Attention/Processing` = "\"Attention/Processing-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_relayout-Attention/Processing` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-01-01\",\"2018-04-01\"],\"minallowed\":\"2005-04-01\",\"maxallowed\":\"2024-10-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-11.14159248448284,\"maxallowed\":11.14159248448284,\"range\":[-5.461210129109061,2.55]},\"showlegend\":true}",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Language` = "\"Language-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Language` = "\"Language-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `Language-MINTTOTS_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-ANIMALS_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-VEG_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-UDSVERTN_visibility` = "legendonly",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-UDSVERFC_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-UDSVERLC_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-TraceMapping` = c(
      "",
      0,
      "",
      "",
      "dashed",
      1,
      "",
      "",
      "Impaired",
      2,
      "",
      TRUE,
      "dashed",
      3,
      "",
      "",
      "Borderline",
      4,
      "",
      TRUE,
      "dashed",
      5,
      "",
      "",
      "Low Average",
      6,
      "",
      TRUE,
      "dashed",
      7,
      "",
      "",
      "Average",
      8,
      "",
      TRUE,
      "dashed",
      9,
      "",
      "",
      "High Average",
      10,
      "",
      TRUE,
      "dashed",
      11,
      "",
      "",
      "Superior",
      12,
      "",
      TRUE,
      "dashed",
      13,
      "",
      "",
      "Very Superior",
      14,
      "",
      TRUE,
      "MINT",
      15,
      "MINTTOTS",
      TRUE,
      "MINT",
      16,
      "MINTTOTS",
      TRUE,
      "Animal Fluency",
      17,
      "ANIMALS",
      TRUE,
      "Animal Fluency",
      18,
      "ANIMALS",
      TRUE,
      "Vegetable Fluency",
      19,
      "VEG",
      TRUE,
      "Vegetable Fluency",
      20,
      "VEG",
      TRUE,
      "F+L Words",
      21,
      "UDSVERTN",
      "legendonly",
      "F+L Words",
      22,
      "UDSVERTN",
      "legendonly",
      "F Words",
      23,
      "UDSVERFC",
      TRUE,
      "F Words",
      24,
      "UDSVERFC",
      TRUE,
      "L Words",
      25,
      "UDSVERLC",
      TRUE,
      "L Words",
      26,
      "UDSVERLC",
      TRUE
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Language` = "\"Language-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_relayout-Language` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-01-01\",\"2018-04-01\"],\"minallowed\":\"2005-04-01\",\"maxallowed\":\"2024-10-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-13.36127317535263,\"maxallowed\":13.36127317535263,\"range\":[-2.55,2.55]},\"showlegend\":true}",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `Visuospatial-UDSBENTC_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Visuospatial-TraceMapping` = c(
      "",
      0,
      "",
      "",
      "dashed",
      1,
      "",
      "",
      "Impaired",
      2,
      "",
      TRUE,
      "dashed",
      3,
      "",
      "",
      "Borderline",
      4,
      "",
      TRUE,
      "dashed",
      5,
      "",
      "",
      "Low Average",
      6,
      "",
      TRUE,
      "dashed",
      7,
      "",
      "",
      "Average",
      8,
      "",
      TRUE,
      "dashed",
      9,
      "",
      "",
      "High Average",
      10,
      "",
      TRUE,
      "dashed",
      11,
      "",
      "",
      "Superior",
      12,
      "",
      TRUE,
      "dashed",
      13,
      "",
      "",
      "Very Superior",
      14,
      "",
      TRUE,
      "Benson Figure Copy",
      15,
      "UDSBENTC",
      TRUE,
      "Benson Figure Copy",
      16,
      "UDSBENTC",
      TRUE
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Visuospatial` = "\"Visuospatial-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_relayout-Visuospatial` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-01-01\",\"2018-04-01\"],\"minallowed\":\"2005-04-01\",\"maxallowed\":\"2024-10-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-11.81291099230906,\"maxallowed\":11.81291099230906,\"range\":[-2.55,2.55]},\"showlegend\":true}",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Memory` = "\"Memory-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Memory` = "\"Memory-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `Memory-UDSBENTD_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-CRAFTVRS_visibility` = "legendonly",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-CRAFTURS_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-CRAFTDVR_visibility` = "legendonly",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-CRAFTDRE_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-TraceMapping` = c(
      "",
      0,
      "",
      "",
      "dashed",
      1,
      "",
      "",
      "Impaired",
      2,
      "",
      TRUE,
      "dashed",
      3,
      "",
      "",
      "Borderline",
      4,
      "",
      TRUE,
      "dashed",
      5,
      "",
      "",
      "Low Average",
      6,
      "",
      TRUE,
      "dashed",
      7,
      "",
      "",
      "Average",
      8,
      "",
      TRUE,
      "dashed",
      9,
      "",
      "",
      "High Average",
      10,
      "",
      TRUE,
      "dashed",
      11,
      "",
      "",
      "Superior",
      12,
      "",
      TRUE,
      "dashed",
      13,
      "",
      "",
      "Very Superior",
      14,
      "",
      TRUE,
      "Benson Delay",
      15,
      "UDSBENTD",
      TRUE,
      "Benson Delay",
      16,
      "UDSBENTD",
      TRUE,
      "Craft Immediate - Verbatim",
      17,
      "CRAFTVRS",
      "legendonly",
      "Craft Immediate - Verbatim",
      18,
      "CRAFTVRS",
      "legendonly",
      "Craft Immediate - Paraphrase",
      19,
      "CRAFTURS",
      TRUE,
      "Craft Immediate - Paraphrase",
      20,
      "CRAFTURS",
      TRUE,
      "Craft Delay - Verbatim",
      21,
      "CRAFTDVR",
      "legendonly",
      "Craft Delay - Verbatim",
      22,
      "CRAFTDVR",
      "legendonly",
      "Craft Delay - Paraphrase",
      23,
      "CRAFTDRE",
      TRUE,
      "Craft Delay - Paraphrase",
      24,
      "CRAFTDRE",
      TRUE
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Memory` = "\"Memory-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_relayout-Memory` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-01-01\",\"2018-04-01\"],\"minallowed\":\"2005-04-01\",\"maxallowed\":\"2024-10-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-4.434964746156817,\"maxallowed\":4.434964746156817,\"range\":[-3.72915302651414,2.55]},\"showlegend\":true}",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `Executive Functioning-TRAILB_visibility` = TRUE,
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Executive Functioning-TraceMapping` = c(
      "",
      0,
      "",
      "",
      "dashed",
      1,
      "",
      "",
      "Impaired",
      2,
      "",
      TRUE,
      "dashed",
      3,
      "",
      "",
      "Borderline",
      4,
      "",
      TRUE,
      "dashed",
      5,
      "",
      "",
      "Low Average",
      6,
      "",
      TRUE,
      "dashed",
      7,
      "",
      "",
      "Average",
      8,
      "",
      TRUE,
      "dashed",
      9,
      "",
      "",
      "High Average",
      10,
      "",
      TRUE,
      "dashed",
      11,
      "",
      "",
      "Superior",
      12,
      "",
      TRUE,
      "dashed",
      13,
      "",
      "",
      "Very Superior",
      14,
      "",
      TRUE,
      "Trailmaking Part B",
      15,
      "TRAILB",
      TRUE,
      "Trailmaking Part B",
      16,
      "TRAILB",
      TRUE
    ),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `plotly_afterplot-Executive Functioning` = "\"Executive Functioning-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_relayout-Executive Functioning` = "{\"xaxis\":{\"automargin\":true,\"range\":[\"2007-01-01\",\"2018-04-01\"],\"minallowed\":\"2005-04-01\",\"maxallowed\":\"2024-10-01\"},\"yaxis\":{\"title\":\"z-score\",\"y\":[0],\"yanchor\":\"bottom\",\"yref\":\"container\",\"minallowed\":-5.679846558243188,\"maxallowed\":5.679846558243188,\"range\":[-2.55,2.55]},\"showlegend\":true}",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `plotly_afterplot-Mood` = "\"Mood-plot\"",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `Mood-showPlot` = "no",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$wait_for_idle(duration = 1000)
  # Update output value
  app$expect_values()
  app$stop()
})
