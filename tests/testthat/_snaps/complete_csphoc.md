# csphoc example runs and completes as expected

    Code
      complete_csphoc(count.df)
    Output
      # A tibble: 12 x 15
         header_id species        location ad_female_count ad_male_count ad_unk_count
             <dbl> <chr>          <chr>              <dbl>         <dbl>        <dbl>
       1         1 Crabeater seal test                   5            NA            0
       2         1 Elephant seal  test                   3             4            0
       3         1 Leopard seal   test                   0             0            0
       4         1 Weddell seal   test                   0             0            0
       5         2 Crabeater seal test                   7             2            0
       6         2 Elephant seal  test                   3             3            0
       7         2 Leopard seal   test                   3            NA            0
       8         2 Weddell seal   test                   0             0            0
       9         3 Crabeater seal test                   0             0            0
      10         3 Elephant seal  test                   0             0            0
      11         3 Leopard seal   test                   0             0            0
      12         3 Weddell seal   test                   6             0            0
      # i 9 more variables: juv_female_count <dbl>, juv_male_count <dbl>,
      #   juv_unk_count <dbl>, pup_live_count <dbl>, unk_female_count <int>,
      #   unk_male_count <int>, unk_unk_count <int>, research_program <chr>,
      #   census_date_start <date>

