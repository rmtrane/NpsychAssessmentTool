# Wrapper to prepare raw data

This is a wrapper function that returns only the desired columns, adds
variables that can be derived to the data, and adds standardized scores.

## Usage

``` r
prepare_data(
  dat,
  selected_cols = c("RACE", "CDRGLOB", "MOCATOTS", "MOCBTOTS", "TRAILA", "TRAILARR",
    "TRAILALI", "OTRAILA", "OTRLARR", "DIGFORCT", "DIGFORSL", "DIGBACCT", "DIGBACLS",
    "WAIS", "MINTTOTS", "ANIMALS", "VEG", "UDSVERTN", "UDSVERFC", "UDSVERLC", "UDSBENTC",
    "UDSBENTD", "CRAFTVRS", "CRAFTURS", "CRAFTDVR", "CRAFTDRE", "REY1REC", "REY2REC",
    "REY3REC", "REY4REC", "REY5REC", "REY6REC", "REYDREC", "REYTCOR", "TRAILB",
    "TRAILBLI", "TRAILBRR", "MOCACLOC", "MOCACLOH", "MOCACLON", "OTRAILB", "OTRLBRR",
    "OTRLBLI", "NACCGDS", "CDRSUM", 
     "UDSBENRS", "NACCID", "SEX", "EDUC", "HANDED",
    "NACCAGE", "BIRTHYR", "VISITYR", "VISITMO", "VISITDAY", "NACCUDSD", "NACCMMSE",
    "BOSTON", "LOGIMEM", "MEMUNITS", "MEMTIME", "DIGIF", "DIGIFLEN", "DIGIB", "DIGIBLEN"),
  methods = NULL,
  print_messages = F,
  with_diags = T
)
```

## Arguments

- dat:

  data set similar to the NACC data. For an example, see
  [`?demo_data`](https://rmtrane.github.io/NpsychAssessmentTool/reference/demo_data.md).

- selected_cols:

  vector with columns to keep. If named, the names must correspond to
  NACC variable names and entries columns in the data set `dat`.

- methods:

  NULL (default) or list of named entries specifying which model to use
  for standardizing cognitive scores. If NULL, defaults are applied for
  all variables in the dataset for which methods have been implemented
  (see
  [`NpsychBatteryNorms::default_methods`](https://rmtrane.github.io/NpsychBatteryNorms/reference/default_methods.html))

- print_messages:

  logical; should messages be printed? If TRUE (default), a message will
  be print with the methods used if methods is omitted (or NULL)

- with_diags:

  logical (default `TRUE`); should diagnoses be included?

## Examples

``` r
prepare_data(demo_data)
#>       RACE CDRGLOB raw_MOCATOTS MOCBTOTS raw_TRAILA TRAILARR TRAILALI
#>      <num>   <num>        <num>    <num>      <num>    <num>    <num>
#>   1:     1     1.0           -4       -4         27       -4       -4
#>   2:     1     0.5           -4       -4         31        0       24
#>   3:     1     0.5           -4       -4         27        0       24
#>   4:     1     2.0           -4       -4        100        0       24
#>   5:     1     0.0           -4       -4         30        0       24
#>  ---                                                                 
#> 340:     1     2.0           -4       -4         73        0       24
#> 341:     1     0.5           -4       -4        997        0       24
#> 342:     1     0.0           -4       -4         73        0       96
#> 343:     1     0.0           -4       -4         40        0       24
#> 344:     1     0.0           -4       -4         23        0       24
#>      raw_OTRAILA raw_OTRLARR raw_DIGFORCT raw_DIGFORSL raw_DIGBACCT
#>            <num>       <num>        <num>        <num>        <num>
#>   1:          -4          -4           -4           -4           -4
#>   2:          -4          -4           -4           -4           -4
#>   3:          -4          -4           -4           -4           -4
#>   4:          -4          -4           -4           -4           -4
#>   5:          -4          -4           -4           -4           -4
#>  ---                                                               
#> 340:          -4          -4           -4           -4           -4
#> 341:          -4          -4           -4           -4           -4
#> 342:          -4          -4           -4           -4           -4
#> 343:          -4          -4           -4           -4           -4
#> 344:          -4          -4           -4           -4           -4
#>      raw_DIGBACLS raw_WAIS raw_MINTTOTS raw_ANIMALS raw_VEG raw_UDSVERTN
#>             <num>    <num>        <num>       <num>   <num>        <num>
#>   1:           -4        0           -4          26       3           -4
#>   2:           -4       49           -4          27      18           -4
#>   3:           -4       43           -4          23      21           -4
#>   4:           -4       51           -4          12      11           -4
#>   5:           -4       43           -4          22      17           -4
#>  ---                                                                    
#> 340:           -4       59           -4          15       6           -4
#> 341:           -4       59           -4          22      12           -4
#> 342:           -4       -4           -4          19       5           31
#> 343:           -4       -4           -4          31       6           15
#> 344:           -4       -4           -4          18      15           16
#>      raw_UDSVERFC raw_UDSVERLC raw_UDSBENTC raw_UDSBENTD raw_CRAFTVRS
#>             <num>        <num>        <num>        <num>        <num>
#>   1:           -4           -4           -4           -4           -4
#>   2:           -4           -4           -4           -4           -4
#>   3:           -4           -4           -4           -4           -4
#>   4:           -4           -4           -4           -4           -4
#>   5:           -4           -4           -4           -4           -4
#>  ---                                                                 
#> 340:           -4           -4           -4           -4           -4
#> 341:           -4           -4           -4           -4           -4
#> 342:           22           18           13            9           -4
#> 343:           23           22           14           13           -4
#> 344:           16            7           17            7           -4
#>      raw_CRAFTURS raw_CRAFTDVR raw_CRAFTDRE raw_REY1REC raw_REY2REC raw_REY3REC
#>             <num>        <num>        <num>       <num>       <num>       <num>
#>   1:           -4           -4           -4          -4          -4          -4
#>   2:           -4           -4           -4          -4          -4          -4
#>   3:           -4           -4           -4          -4          -4          -4
#>   4:           -4           -4           -4          -4          -4          -4
#>   5:           -4           -4           -4          -4          -4          -4
#>  ---                                                                           
#> 340:           -4           -4           -4          -4          -4          -4
#> 341:           -4           -4           -4          -4          -4          -4
#> 342:           -4           -4           -4          -4          -4          -4
#> 343:           -4           -4           -4          -4          -4          -4
#> 344:           -4           -4           -4          -4          -4          -4
#>      raw_REY4REC raw_REY5REC raw_REY6REC raw_REYDREC REYTCOR raw_TRAILB
#>            <num>       <num>       <num>       <num>   <num>      <num>
#>   1:          -4          -4          -4          -4      -4        150
#>   2:          -4          -4          -4          -4      -4         98
#>   3:          -4          -4          -4          -4      -4        996
#>   4:          -4          -4          -4          -4      -4        997
#>   5:          -4          -4          -4          -4      -4         39
#>  ---                                                                   
#> 340:          -4          -4          -4          -4      -4         97
#> 341:          -4          -4          -4          -4      -4         77
#> 342:          -4          -4          -4          -4      -4         50
#> 343:          -4          -4          -4          -4      -4         51
#> 344:          -4          -4          -4          -4      -4        300
#>      TRAILBLI TRAILBRR MOCACLOC MOCACLOH MOCACLON raw_OTRAILB raw_OTRLBRR
#>         <num>    <num>    <num>    <num>    <num>       <num>       <num>
#>   1:       -4       -4       -4       -4       -4          -4          -4
#>   2:       96        0       -4       -4       -4          -4          -4
#>   3:        6        1       -4       -4       -4          -4          -4
#>   4:       24        0       -4       -4       -4          -4          -4
#>   5:       24        0       -4       -4       -4          -4          -4
#>  ---                                                                     
#> 340:       24        1       -4       -4       -4          -4          -4
#> 341:       24        6       -4       -4       -4          -4          -4
#> 342:       24        0       -4       -4       -4          -4          -4
#> 343:       24        1       -4       -4       -4          -4          -4
#> 344:       24        5       -4       -4       -4          -4          -4
#>      OTRLBLI NACCGDS CDRSUM UDSBENRS     NACCID   SEX  EDUC HANDED NACCAGE
#>        <num>   <num>  <num>    <num>     <char> <num> <num>  <num>   <num>
#>   1:      -4       1    2.0       -4 NACC074283     2    18      2      84
#>   2:      -4       0    8.0       -4 NACC005366     2    16      2      65
#>   3:      -4       1   13.0       -4 NACC005366     2    16      2      51
#>   4:      -4      88    2.5       -4 NACC005366     2    16      2      56
#>   5:      -4       5    0.0       -4 NACC005366     2    16      2      58
#>  ---                                                                      
#> 340:      -4       1    0.0       -4 NACC005149     2    18      2      91
#> 341:      -4      10    0.0       -4 NACC005149     2    18      2      89
#> 342:      -4       5    6.0        1 NACC005149     2    18      2      92
#> 343:      -4       0    3.5        1 NACC005149     2    18      2      83
#> 344:      -4       3    0.0        1 NACC005149     2    18      2      91
#>      BIRTHYR VISITYR VISITMO VISITDAY NACCUDSD raw_NACCMMSE raw_BOSTON
#>        <num>   <num>   <num>    <num>    <num>        <num>      <num>
#>   1:    1927    2011      10        5        1           30         29
#>   2:    1957    2023       5       10        1           30         26
#>   3:    1957    2009       5       15        1           30         24
#>   4:    1957    2013       9        4        4           29          3
#>   5:    1957    2015      10       27        4           28         27
#>  ---                                                                  
#> 340:    1924    2016       3       21        1           29         95
#> 341:    1924    2013      10       11        4           30         29
#> 342:    1924    2016      10       22        1           30         14
#> 343:    1924    2008       2       28        4           25         30
#> 344:    1924    2016       6       11        3           28          8
#>      raw_LOGIMEM raw_MEMUNITS MEMTIME raw_DIGIF raw_DIGIFLEN raw_DIGIB
#>            <num>        <num>   <num>     <num>        <num>     <num>
#>   1:          17            3      35         8            3         3
#>   2:          17            2      -4        97            6         8
#>   3:          16           13      18         6            5         8
#>   4:          17            5       8        10            6         4
#>   5:           3            0      10        10           96         3
#>  ---                                                                  
#> 340:          15           12      21         8            8        96
#> 341:           6           15      26         6            7         0
#> 342:          13            0      30         7           97         6
#> 343:           6           12      15         7            6         7
#> 344:          15           16      18         7            6         6
#>      raw_DIGIBLEN ALCDEM_etiology ALCDEM_contribution ANXIET_etiology
#>             <num>           <num>               <num>           <num>
#>   1:            5               0                   7              -4
#>   2:            4               0                   8              -4
#>   3:            4               0                   7              -4
#>   4:            7               0                   8              -4
#>   5:            5               0                   8              -4
#>  ---                                                                 
#> 340:            3               8                   7              -4
#> 341:            3               0                   8              -4
#> 342:            5               8                   8               0
#> 343:            6               0                   7               0
#> 344:            5               0                   8               0
#>      ANXIET_contribution BIPOLDX_contribution BIPOLDX_etiology BRNINJ_etiology
#>                    <num>                <num>            <num>           <num>
#>   1:                  -4                   -4               -4               0
#>   2:                  -4                   -4               -4               0
#>   3:                  -4                   -4               -4               0
#>   4:                  -4                   -4               -4               0
#>   5:                  -4                   -4               -4               0
#>  ---                                                                          
#> 340:                  -4                   -4               -4               0
#> 341:                  -4                   -4               -4               0
#> 342:                   7                    7                0               0
#> 343:                   7                    8                0               0
#> 344:                   8                    8                0               0
#>      BRNINJ_contribution COGOTH_etiology COGOTH2_etiology COGOTH2_contribution
#>                    <num>          <char>           <char>                <num>
#>   1:                   7               0               -4                    8
#>   2:                   7               0                0                    8
#>   3:                   8       delusions                0                    8
#>   4:                   7               0                0                    8
#>   5:                   7               0                0                    7
#>  ---                                                                          
#> 340:                   8               0                0                    8
#> 341:                   7 B 12 Deficiency                0                    8
#> 342:                   7               0                0                    7
#> 343:                   7               0                0                    7
#> 344:                   8               0                0                    8
#>      COGOTH2X COGOTH3_etiology COGOTH3X COGOTH_contribution         COGOTHX
#>        <char>           <char>   <char>               <num>          <char>
#>   1:                        -4                            7                
#>   2:                         0                            7                
#>   3:                         0                            7       delusions
#>   4:                         0                            8                
#>   5:                         0                            7                
#>  ---                                                                       
#> 340:                         0                            8                
#> 341:                         0                            8 B 12 Deficiency
#> 342:                         0                            8                
#> 343:                         0                            7                
#> 344:                         0                            7                
#>      CORT_etiology CORT_contribution CVD_etiology CVD_contribution
#>              <num>             <num>        <num>            <num>
#>   1:             0                 8           -4               -4
#>   2:             0                 8           -4               -4
#>   3:             0                 7           -4               -4
#>   4:             0                 7           -4               -4
#>   5:             0                 7           -4               -4
#>  ---                                                              
#> 340:             0                 7           -4               -4
#> 341:             0                 8           -4               -4
#> 342:             0                 7            0                7
#> 343:             0                 7            0                7
#> 344:             0                 8            0                8
#>      DELIR_etiology DELIR_contribution DEMUN_etiology DEMUN_contribution
#>               <num>              <num>          <num>              <num>
#>   1:             -4                 -4              8                  7
#>   2:             -4                 -4              8                  8
#>   3:             -4                 -4              0                  8
#>   4:             -4                 -4              0                  7
#>   5:             -4                 -4              8                  8
#>  ---                                                                    
#> 340:             -4                 -4              0                  8
#> 341:             -4                 -4              0                  8
#> 342:              0                  7             -4                 -4
#> 343:              0                  8             -4                 -4
#> 344:              0                  7             -4                 -4
#>      DEP_etiology DEP_contribution DOWNS_etiology DOWNS_contribution
#>             <num>            <num>          <num>              <num>
#>   1:            0                8              0                  8
#>   2:            0                7              0                  8
#>   3:            0                8              0                  7
#>   4:            0                8              0                  8
#>   5:            1                7              0                  7
#>  ---                                                                
#> 340:            0                3              0                  7
#> 341:            0                8              0                  8
#> 342:            0                7              0                  7
#> 343:            0                8              0                  8
#> 344:            0                8              0                  8
#>      DYSILL_etiology DYSILL_contribution EPILEP_etiology EPILEP_contribution
#>                <num>               <num>           <num>               <num>
#>   1:               0                   8              -4                  -4
#>   2:               0                   7              -4                  -4
#>   3:               0                   7              -4                  -4
#>   4:               0                   8              -4                  -4
#>   5:               0                   8              -4                  -4
#>  ---                                                                        
#> 340:               0                   8              -4                  -4
#> 341:               0                   7              -4                  -4
#> 342:               0                   7               0                   7
#> 343:               0                   8               0                   8
#> 344:               0                   7               0                   7
#>      ESSTREM_contribution ESSTREM_etiology FTLDMO_etiology FTLDMO_contribution
#>                     <num>            <num>           <num>               <num>
#>   1:                   -4               -4              -4                  -4
#>   2:                   -4               -4              -4                  -4
#>   3:                   -4               -4              -4                  -4
#>   4:                   -4               -4              -4                  -4
#>   5:                   -4               -4              -4                  -4
#>  ---                                                                          
#> 340:                   -4               -4              -4                  -4
#> 341:                   -4               -4              -4                  -4
#> 342:                    8                0               0                   7
#> 343:                    8                0               0                   7
#> 344:                    7                0               0                   8
#>      FTLDNOS_contribution FTLDNOS_etiology HIV_etiology HIV_contribution
#>                     <num>            <num>        <num>            <num>
#>   1:                   -4               -4           -4               -4
#>   2:                   -4               -4           -4               -4
#>   3:                   -4               -4           -4               -4
#>   4:                   -4               -4           -4               -4
#>   5:                   -4               -4           -4               -4
#>  ---                                                                    
#> 340:                   -4               -4           -4               -4
#> 341:                   -4               -4           -4               -4
#> 342:                    8                0            0                8
#> 343:                    8                0            0                7
#> 344:                    7                0            0                8
#>      HUNT_etiology HUNT_contribution HYCEPH_etiology HYCEPH_contribution
#>              <num>             <num>           <num>               <num>
#>   1:             0                 8               0                   7
#>   2:             0                 8               0                   7
#>   3:             0                 8               0                   7
#>   4:             0                 8               0                   8
#>   5:             0                 8               0                   7
#>  ---                                                                    
#> 340:             0                 7               0                   7
#> 341:             0                 7               0                   8
#> 342:             0                 8               0                   7
#> 343:             0                 8               0                   8
#> 344:             0                 7               0                   7
#>      IMPSUB_etiology IMPSUB_contribution MEDS_etiology MEDS_contribution
#>                <num>               <num>         <num>             <num>
#>   1:              -4                  -4             0                 7
#>   2:              -4                  -4             0                 8
#>   3:              -4                  -4             0                 8
#>   4:              -4                  -4             0                 8
#>   5:              -4                  -4             0                 8
#>  ---                                                                    
#> 340:              -4                  -4             0                 8
#> 341:              -4                  -4             0                 8
#> 342:               0                   8             0                 8
#> 343:               0                   7             0                 7
#> 344:               0                   7             0                 8
#>      MSA_etiology MSA_contribution NACCALZD_etiology NACCALZD_contribution
#>             <num>            <num>             <num>                 <num>
#>   1:           -4               -4                 1                     1
#>   2:           -4               -4                 8                     7
#>   3:           -4               -4                 8                     7
#>   4:           -4               -4                 1                     7
#>   5:           -4               -4                 8                     1
#>  ---                                                                      
#> 340:           -4               -4                 1                     1
#> 341:           -4               -4                 8                     1
#> 342:            0                8                 0                     8
#> 343:            0                7                 8                     8
#> 344:            0                7                 0                     7
#>      NACCLBDE_etiology NACCLBDE_contribution NEOP_etiology NEOP_contribution
#>                  <num>                 <num>         <num>             <num>
#>   1:                 0                     7             0                 8
#>   2:                 0                     7             0                 8
#>   3:                 0                     7             0                 7
#>   4:                 8                     7             0                 7
#>   5:                 0                     7             0                 8
#>  ---                                                                        
#> 340:                 8                     8             0                 8
#> 341:                 8                     7             0                 7
#> 342:                 8                     8             0                 8
#> 343:                 0                     8             0                 8
#> 344:                 0                     3             0                 8
#>      OTHCOG_etiology OTHCOG_contribution OTHCOGX OTHPSY_etiology
#>               <char>               <num>  <char>          <char>
#>   1:              -4                  -4                       0
#>   2:              -4                  -4                       0
#>   3:              -4                  -4                       0
#>   4:              -4                  -4                       0
#>   5:              -4                  -4                DYSTHMIA
#>  ---                                                            
#> 340:              -4                  -4                       0
#> 341:              -4                  -4                       0
#> 342:               0                   8                       0
#> 343:               0                   8                       0
#> 344:               0                   7                       0
#>      OTHPSY_contribution  OTHPSYX POSSAD_etiology POSSAD_contribution
#>                    <num>   <char>           <num>               <num>
#>   1:                   8                        0                   7
#>   2:                   8                        0                   8
#>   3:                   7                        8                   7
#>   4:                   7                        1                   7
#>   5:                   8 DYSTHMIA               0                   1
#>  ---                                                                 
#> 340:                   7                        0                   7
#> 341:                   8                        8                   8
#> 342:                   7                       -4                  -4
#> 343:                   7                       -4                  -4
#> 344:                   8                       -4                  -4
#>      PPAPH_etiology PPAPH_contribution PRION_etiology PRION_contribution
#>               <num>              <num>          <num>              <num>
#>   1:              1                  7              0                  8
#>   2:              1                  7              0                  7
#>   3:              0                  8              0                  8
#>   4:              0                  8              0                  7
#>   5:              0                  8              0                  7
#>  ---                                                                    
#> 340:              0                  8              0                  7
#> 341:              0                  7              0                  7
#> 342:             -4                 -4              0                  7
#> 343:             -4                 -4              0                  8
#> 344:             -4                 -4              0                  7
#>      PROBAD_etiology PROBAD_contribution PSP_etiology PSP_contribution
#>                <num>               <num>        <num>            <num>
#>   1:               0                   7            0                7
#>   2:               0                   1            0                7
#>   3:               8                   7            0                8
#>   4:               1                   8            0                8
#>   5:               1                   8            0                8
#>  ---                                                                  
#> 340:               1                   8            0                7
#> 341:               8                   1            0                8
#> 342:              -4                  -4            0                8
#> 343:              -4                  -4            0                8
#> 344:              -4                  -4            0                8
#>      PTSDDX_etiology PTSDDX_contribution SCHIZOP_contribution SCHIZOP_etiology
#>                <num>               <num>                <num>            <num>
#>   1:              -4                  -4                   -4               -4
#>   2:              -4                  -4                   -4               -4
#>   3:              -4                  -4                   -4               -4
#>   4:              -4                  -4                   -4               -4
#>   5:              -4                  -4                   -4               -4
#>  ---                                                                          
#> 340:              -4                  -4                   -4               -4
#> 341:              -4                  -4                   -4               -4
#> 342:               0                   8                    8                0
#> 343:               0                   7                    8                0
#> 344:               0                   8                    8                0
#>      STROKE_etiology STROKE_contribution VASC_etiology VASC_contribution
#>                <num>               <num>         <num>             <num>
#>   1:               0                   7             0                 7
#>   2:               0                   7             0                 1
#>   3:               0                   2             8                 8
#>   4:               0                   8             0                 8
#>   5:               0                   7             8                 7
#>  ---                                                                    
#> 340:               0                   8             0                 7
#> 341:               0                   8             0                 7
#> 342:              -4                  -4            -4                -4
#> 343:              -4                  -4            -4                -4
#> 344:              -4                  -4            -4                -4
#>      VASCPS_etiology VASCPS_contribution BILLS TAXES SHOPPING GAMES STOVE
#>                <num>               <num> <num> <num>    <num> <num> <num>
#>   1:              -4                  -4     0     0        0     0     0
#>   2:               8                   7     0     0        0     0     0
#>   3:               8                   8     0     0        0     0     0
#>   4:               8                   8     0     0        0     0     0
#>   5:               0                   7     0     0        0     0     0
#>  ---                                                                     
#> 340:               8                   8     1     0        0     0     0
#> 341:               8                   8     8     8        0     8     0
#> 342:              -4                  -4     0     3        0     0     1
#> 343:              -4                  -4     1     0        0     0     0
#> 344:              -4                  -4     0     0        0     3     0
#>      MEALPREP EVENTS PAYATTN REMDATES TRAVEL REYFPOS  VISITDATE REYTOTAL
#>         <num>  <num>   <num>    <num>  <num>   <num>     <Date>    <num>
#>   1:        0      0       0        2      0      -4 2011-10-05       NA
#>   2:        1      0       0        0      0      -4 2023-05-10       NA
#>   3:        8      0       0        0      0      -4 2009-05-15       NA
#>   4:        0      2       0        3      0      -4 2013-09-04       NA
#>   5:        8      3       2        0      0      -4 2015-10-27       NA
#>  ---                                                                    
#> 340:        0      0       0        0      3      -4 2016-03-21       NA
#> 341:        0      0       0        2      3      -4 2013-10-11       NA
#> 342:        0      0       0        0      3      -4 2016-10-22       NA
#> 343:        1      0       0        0      3      -4 2008-02-28       NA
#> 344:        3      0       0        0      2      -4 2016-06-11       NA
#>      REYAREC   FAS MOCACLOCK std_MOCATOTS std_OTRAILA std_OTRAILB std_OTRLARR
#>        <num> <num>     <num>        <num>       <num>       <num>       <num>
#>   1:      NA     2        NA           NA          NA          NA          NA
#>   2:      NA     1        NA           NA          NA          NA          NA
#>   3:      NA     0        NA           NA          NA          NA          NA
#>   4:      NA     2        NA           NA          NA          NA          NA
#>   5:      NA     2        NA           NA          NA          NA          NA
#>  ---                                                                         
#> 340:      NA     1        NA           NA          NA          NA          NA
#> 341:      NA     2        NA           NA          NA          NA          NA
#> 342:      NA     1        NA           NA          NA          NA          NA
#> 343:      NA     2        NA           NA          NA          NA          NA
#> 344:      NA     2        NA           NA          NA          NA          NA
#>      std_OTRLBRR std_DIGFORCT std_DIGFORSL std_DIGBACCT std_DIGBACLS std_TRAILA
#>            <num>        <num>        <num>        <num>        <num>      <num>
#>   1:          NA           NA           NA           NA           NA  0.7082273
#>   2:          NA           NA           NA           NA           NA -0.3168948
#>   3:          NA           NA           NA           NA           NA -0.5267330
#>   4:          NA           NA           NA           NA           NA -7.1029964
#>   5:          NA           NA           NA           NA           NA -0.5147449
#>  ---                                                                           
#> 340:          NA           NA           NA           NA           NA -3.2758183
#> 341:          NA           NA           NA           NA           NA         NA
#> 342:          NA           NA           NA           NA           NA -3.2342782
#> 343:          NA           NA           NA           NA           NA -0.5414161
#> 344:          NA           NA           NA           NA           NA  1.3707325
#>      std_TRAILB  std_WAIS std_MINTTOTS std_ANIMALS      std_VEG std_UDSVERFC
#>           <num>     <num>        <num>       <num>        <num>        <num>
#>   1: -1.3560479  9.936317           NA   1.0078416 -3.129586776           NA
#>   2: -0.6202545 42.777352           NA   0.8784501  0.366620115           NA
#>   3:         NA 31.805875           NA  -0.2803483  0.824086354           NA
#>   4:         NA 42.616302           NA  -2.2383969 -1.561557773           NA
#>   5:  0.5369585 34.813298           NA  -0.2734779 -0.026880497           NA
#>  ---                                                                        
#> 340:  0.2121699 72.448842           NA  -0.8937175 -2.238299179           NA
#> 341:  0.6192812 70.727242           NA   0.3856936 -0.786254543           NA
#> 342:  1.3956639        NA           NA  -0.1021010 -2.466534512    1.5625754
#> 343:  1.0132519        NA           NA   1.9338115 -2.403564458    1.6800390
#> 344: -4.7276735        NA           NA  -0.3211887  0.001742254    0.2263162
#>      std_UDSVERLC std_UDSVERTN std_UDSBENTC std_UDSBENTD std_CRAFTVRS
#>             <num>        <num>        <num>        <num>        <num>
#>   1:           NA           NA           NA           NA           NA
#>   2:           NA           NA           NA           NA           NA
#>   3:           NA           NA           NA           NA           NA
#>   4:           NA           NA           NA           NA           NA
#>   5:           NA           NA           NA           NA           NA
#>  ---                                                                 
#> 340:           NA           NA           NA           NA           NA
#> 341:           NA           NA           NA           NA           NA
#> 342:    0.8863316    0.2318152    -1.883097   -0.2266991           NA
#> 343:    1.7183836   -1.8311439    -1.214242    0.8711626           NA
#> 344:   -1.7103944   -1.6157296     1.136978   -0.9342854           NA
#>      std_CRAFTURS std_CRAFTDVR std_CRAFTDRE std_REY1REC std_REY2REC std_REY3REC
#>             <num>        <num>        <num>       <num>       <num>       <num>
#>   1:           NA           NA           NA          NA          NA          NA
#>   2:           NA           NA           NA          NA          NA          NA
#>   3:           NA           NA           NA          NA          NA          NA
#>   4:           NA           NA           NA          NA          NA          NA
#>   5:           NA           NA           NA          NA          NA          NA
#>  ---                                                                           
#> 340:           NA           NA           NA          NA          NA          NA
#> 341:           NA           NA           NA          NA          NA          NA
#> 342:           NA           NA           NA          NA          NA          NA
#> 343:           NA           NA           NA          NA          NA          NA
#> 344:           NA           NA           NA          NA          NA          NA
#>      std_REY4REC std_REY5REC std_REY6REC std_REYDREC std_NACCMMSE std_BOSTON
#>            <num>       <num>       <num>       <num>        <num>      <num>
#>   1:          NA          NA          NA          NA   0.95543662  0.3840990
#>   2:          NA          NA          NA          NA   0.87774907 -0.7197370
#>   3:          NA          NA          NA          NA   0.65008478 -1.6285342
#>   4:          NA          NA          NA          NA  -0.07554362 -8.5591135
#>   5:          NA          NA          NA          NA  -0.84995722 -0.5058981
#>  ---                                                                        
#> 340:          NA          NA          NA          NA   0.26233169         NA
#> 341:          NA          NA          NA          NA   1.03674529  0.4700132
#> 342:          NA          NA          NA          NA   1.08553050 -4.4902193
#> 343:          NA          NA          NA          NA  -3.09551050  0.7010348
#> 344:          NA          NA          NA          NA  -0.54460539 -6.5121146
#>      std_LOGIMEM std_MEMUNITS  std_DIGIF std_DIGIFLEN   std_DIGIB std_DIGIBLEN
#>            <num>        <num>      <num>        <num>       <num>        <num>
#>   1:   0.8677302   -2.2377054 -0.4392518   -3.6153325 -1.93835631  -0.02343810
#>   2:   0.9498944           NA         NA   -0.8234689  0.45910628  -0.89001987
#>   3:   0.5953452    0.1181994 -1.6168298   -1.8600080  0.31021555  -1.04045203
#>   4:   0.8944381   -1.8769464  0.4434189   -0.8922863 -1.55873798   1.55481308
#>   5:  -2.8492126   -3.0847345  0.4641260           NA -2.01800008  -0.11805617
#>  ---                                                                          
#> 340:   0.3742951   -0.0537305 -0.3667768    1.0856413          NA  -1.64258159
#> 341:  -2.0525835    0.7006790 -1.3917243    0.1408587 -3.32677764  -1.66407189
#> 342:  -0.1561109   -2.9533354 -0.8585434           NA -0.41167929   0.06252313
#> 343:  -2.0895543   -0.1392177 -0.9517256   -0.8345094 -0.02686256   0.81299653
#> 344:   0.3742951    0.9149589 -0.8688970   -0.7733384 -0.42231434   0.05177798
```
