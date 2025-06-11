---
toc-title: Table of contents
---

## Introduction

This R Shiny application is build on the R package `NpsychBatteryNorms`,
which implements a handful of methods to standardize various
neuropsychological scores. The main focus is to standardize scores that
are part of UDS 3, but with some legacy and a few additional scores
included as well.

## Standardization Methods

:::: cell
::: cell-output-display
<div id="caaohzflhs" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#caaohzflhs table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#caaohzflhs thead, #caaohzflhs tbody, #caaohzflhs tfoot, #caaohzflhs tr, #caaohzflhs td, #caaohzflhs th {
  border-style: none;
}

#caaohzflhs p {
  margin: 0;
  padding: 0;
}

#caaohzflhs .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#caaohzflhs .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#caaohzflhs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#caaohzflhs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#caaohzflhs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#caaohzflhs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#caaohzflhs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#caaohzflhs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#caaohzflhs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#caaohzflhs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#caaohzflhs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#caaohzflhs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#caaohzflhs .gt_spanner_row {
  border-bottom-style: hidden;
}

#caaohzflhs .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#caaohzflhs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#caaohzflhs .gt_from_md > :first-child {
  margin-top: 0;
}

#caaohzflhs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#caaohzflhs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#caaohzflhs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#caaohzflhs .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#caaohzflhs .gt_row_group_first td {
  border-top-width: 2px;
}

#caaohzflhs .gt_row_group_first th {
  border-top-width: 2px;
}

#caaohzflhs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#caaohzflhs .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#caaohzflhs .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#caaohzflhs .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#caaohzflhs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#caaohzflhs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#caaohzflhs .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#caaohzflhs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#caaohzflhs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#caaohzflhs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#caaohzflhs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#caaohzflhs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#caaohzflhs .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#caaohzflhs .gt_left {
  text-align: left;
}

#caaohzflhs .gt_center {
  text-align: center;
}

#caaohzflhs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#caaohzflhs .gt_font_normal {
  font-weight: normal;
}

#caaohzflhs .gt_font_bold {
  font-weight: bold;
}

#caaohzflhs .gt_font_italic {
  font-style: italic;
}

#caaohzflhs .gt_super {
  font-size: 65%;
}

#caaohzflhs .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#caaohzflhs .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#caaohzflhs .gt_indent_1 {
  text-indent: 5px;
}

#caaohzflhs .gt_indent_2 {
  text-indent: 10px;
}

#caaohzflhs .gt_indent_3 {
  text-indent: 15px;
}

#caaohzflhs .gt_indent_4 {
  text-indent: 20px;
}

#caaohzflhs .gt_indent_5 {
  text-indent: 25px;
}

#caaohzflhs .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#caaohzflhs div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

  variables   short_descriptor                                                                     norms                                                                                                               regression                                                                                                          T-score
  ----------- ------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------
  ANIMALS     Animals - Total number of animals named in 60 seconds                                nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  BOSTON      Boston Naming Test (30) - Total score                                                nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  CRAFTCUE    Craft Story 21 Recall (Delayed) - Cue (boy) needed                                   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  CRAFTDRE    Craft Story 21 Recall (Delayed) - Total story units recalled, paraphrase scoring     nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  CRAFTDVR    Craft Story 21 Recall (Delayed) - Total story units recalled, verbatim scoring       nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  CRAFTURS    Craft Story 21 Recall (Immediate) - Total story units recalled, paraphrase scoring   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  CRAFTVRS    Craft Story 21 Recall (Immediate) - Total story units recalled, verbatim scoring     nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGBACCT    Number Span Test: Backward - Number of correct trials                                nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGBACLS    Number Span Test: Backward - Longest span backward                                   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGFORCT    Number Span Test: Forward - Number of correct trials                                 nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGFORSL    Number Span Test: Forward - Longest span forward                                     nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGIB       Digit span backward trials correct                                                   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGIBLEN    Digit span backward length                                                           nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGIF       Digit span forward trials correct                                                    nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  DIGIFLEN    Digit span forward length                                                            nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  LOGIMEM     Total number of story units recalled from this current test administration           nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  MEMUNITS    Logical Memory IIA - Delayed - Total number of story units recalled                  nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  MINTPCNG    Multilingual Naming Test (MINT) - Phonemic cues: Number given                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  MINTSCNG    Multilingual Naming Test (MINT) - Semantic cues: Number given                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  MINTTOTS    Multilingual Naming Test (MINT) - Total score                                        nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  MINTTOTW    Multilingual Naming Test (MINT) - Total correct without semantic cue                 nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  MOCATOTS    MoCA Total Raw Score - uncorrected                                                   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  NACCMMSE    Total MMSE score (using D-L-R-O-W)                                                   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  OTRAILA     Oral Trail Making Test Part A � Total number of seconds to complete                  nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  OTRAILB     Oral Trail Making Test Part B � Total number of seconds to complete                  nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  OTRLALI     Oral Trail Making Test Part A � Number of correct lines                              nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  OTRLARR     Oral Trail Making Test Part A � Number of commission errors                          nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  OTRLBLI     Oral Trail Making Test Part B � Number of correct lines                              nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  OTRLBRR     Oral Trail Making Test Part B � Number of commission errors                          nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  REY1REC     Rey Auditory Verbal Learning (Immediate) Trial 1 Total recall                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REY2REC     Rey Auditory Verbal Learning (Immediate) Trial 2 Total recall                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REY3REC     Rey Auditory Verbal Learning (Immediate) Trial 3 Total recall                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REY4REC     Rey Auditory Verbal Learning (Immediate) Trial 4 Total recall                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REY5REC     Rey Auditory Verbal Learning (Immediate) Trial 5 Total recall                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REY6REC     Rey Auditory Verbal Learning (Immediate) Trial 6 Total recall                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REYAREC     RAVLT Recognition Percentage (not in NACC data)                                      nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REYDLIST    RAVLT Distractor List (not in NACC data)                                             nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REYDREC     Rey Auditory Verbal Learning (Delayed) - Total Recall                                nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  REYTOTAL    Sum of REY1REC, \..., REY5REC                                                        nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  TRAILA      Trail Making Test Part A - Total number of seconds to complete                       nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  TRAILALI    Part A - Number of correct lines                                                     nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  TRAILARR    Part A - Number of commission errors                                                 nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  TRAILB      Trail Making Test Part B - Total number of seconds to complete                       nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  TRAILBLI    Part B - Number of correct lines                                                     nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  TRAILBRR    Part B - Number of commission errors                                                 nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSBENRS    Benson Complex Figure Recall - Recognized original stimulus among four options       nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSBENTC    Total Score for copy of Benson figure                                                nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSBENTD    Total score for 10- to 15-minute delayed drawing of Benson figure                    nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSVERFC    Number of correct F-words generated in 1 minute                                      nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSVERLC    Number of correct L-words generated in 1 minute                                      nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSVERTE    Total number of F-word and L-word repetition errors                                  nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSVERTI    Number of non-F/L-words and rule violation errors                                    nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  UDSVERTN    Total number of correct F-words and L-words                                          nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✗]{style="font-weight: bold; color: #B61717;"})
  VEG         Vegetables - Total number of vegetables named in 60 seconds                          nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   nacc ([✓]{style="font-weight: bold; color: #1D8A28;"}), updated ([✓]{style="font-weight: bold; color: #1D8A28;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})
  WAIS        WAIS-R Digit Symbol                                                                  nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   nacc ([✗]{style="font-weight: bold; color: #B61717;"}), updated ([✗]{style="font-weight: bold; color: #B61717;"})   ([✓]{style="font-weight: bold; color: #1D8A28;"})

  : Implemented standardization methods for variables.
  `short_descriptor` is the short description included in the RDD while
  `norms`, `regression`, and `T-score` columns refer to implemented
  methods.

</div>
:::
::::
