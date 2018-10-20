hw05\_factor\_figure\_mgmt
================
Alejandra
17/10/2018

## Factor and figure management

This is an R Markdown document elaborated to serve as a personal
cheatsheet on the topic of factor and figure management. For the
exercises contained in this assignment, I chose to work with the
`gapminder` dataset.

## Loading data and required libraries

``` r
library(gapminder)
library(tidyverse)
library(plotly)
library(knitr)
library(kableExtra)
library(gridExtra)
library(scales)
```

## Part 1: Factor management

*Factor inspection*

First, let’s ensure the variables that I’ll explore are factors. This
can be done by looking at the class of the variables.

``` r
# showing str() output as a table
data.frame(variable = names(gapminder),
           class = sapply(gapminder, class),
           levels = sapply(gapminder, nlevels),
           first_values = sapply(gapminder, function(x) paste0(head(x),  collapse = ", ")),
           row.names = NULL) %>%
  kable() %>%
  kable_styling(full_width = F, position = "center")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

variable

</th>

<th style="text-align:left;">

class

</th>

<th style="text-align:right;">

levels

</th>

<th style="text-align:left;">

first\_values

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

country

</td>

<td style="text-align:left;">

factor

</td>

<td style="text-align:right;">

142

</td>

<td style="text-align:left;">

Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan,
Afghanistan

</td>

</tr>

<tr>

<td style="text-align:left;">

continent

</td>

<td style="text-align:left;">

factor

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

Asia, Asia, Asia, Asia, Asia, Asia

</td>

</tr>

<tr>

<td style="text-align:left;">

year

</td>

<td style="text-align:left;">

integer

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

1952, 1957, 1962, 1967, 1972, 1977

</td>

</tr>

<tr>

<td style="text-align:left;">

lifeExp

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

28.801, 30.332, 31.997, 34.02, 36.088, 38.438

</td>

</tr>

<tr>

<td style="text-align:left;">

pop

</td>

<td style="text-align:left;">

integer

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

8425333, 9240934, 10267083, 11537966, 13079460, 14880372

</td>

</tr>

<tr>

<td style="text-align:left;">

gdpPercap

</td>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

779.4453145, 820.8530296, 853.10071, 836.1971382, 739.9811058, 786.11336

</td>

</tr>

</tbody>

</table>

In the table above we can see the classes of all variables. The
variables `country` and `continent` are indeed factors, `country` has
142 levels and `continent` has 5 levels.

**Explore the effects of `arrange()`**

Does merely arranging the data have any effect on a figure?

Here, I’ll explore what happens to a figure after sorting the data with
the `arrange()` function. I’ll order the `continent` variable according
to the minimum value of life expectancy.

First let’s check that `arrange()` works:

``` r
#new dataset with continent and minimum lifeExp
gap_minlifeExp <- gapminder %>% 
  group_by(continent) %>%
  summarize(minlife = min(lifeExp)) %>%
  arrange(minlife)

kable(gap_minlifeExp) %>%
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

continent

</th>

<th style="text-align:right;">

minlife

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Africa

</td>

<td style="text-align:right;">

23.599

</td>

</tr>

<tr>

<td style="text-align:left;">

Asia

</td>

<td style="text-align:right;">

28.801

</td>

</tr>

<tr>

<td style="text-align:left;">

Americas

</td>

<td style="text-align:right;">

37.579

</td>

</tr>

<tr>

<td style="text-align:left;">

Europe

</td>

<td style="text-align:right;">

43.585

</td>

</tr>

<tr>

<td style="text-align:left;">

Oceania

</td>

<td style="text-align:right;">

69.120

</td>

</tr>

</tbody>

</table>

We can see that the data in the table was indeed arranged by the minimum
value of life expectancy in each continent in ascending order.

Now let’s try arranging the data in a figure. For the purposes of
plotting, I will join the dataset `gap_minlifeExp` created above with
the `gapminder` dataset and arrange the data using `minlife` as
before.

``` r
gap_new <- left_join(gapminder, gap_minlifeExp, by = "continent") #join datasets to create a boxplot


gap_new %>% 
  arrange(minlife) %>% 
  ggplot(aes(x = continent, y = lifeExp, fill = continent)) +
  scale_fill_brewer(palette = "Pastel2") + #change color palette
  guides(fill=FALSE) + #remove unnecessary color legend
  geom_boxplot() +
  labs(x = "Continent", y = "Life Expectancy") +
  theme_bw() #select theme
```

![](hw05_factor_figure_mgmt_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

As we can see, in the figure the data wasn’t arranged as it was in the
output table, instead, continents are sorted alphabetically.

**Explore the effects of reordering a factor**

What effect does this have on a figure?

Here I will try to sort the data of the same figure but using
`reorder()` instead.

``` r
gap_new %>%
  ggplot(aes(x = reorder(continent, minlife), y = lifeExp, fill = continent)) +
  scale_fill_brewer(palette = "Pastel2") + #change color palette
  guides(fill=FALSE) + #remove unnecessary color legend
  geom_boxplot() +
  labs(x = "Continent", y = "Life Expectancy") +
  theme_bw() #select theme
```

![](hw05_factor_figure_mgmt_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

As the `reorder()` function does reorders the levels of the factor, the
continents in the figure are now ordered as desired (by minimum life
expectancy).

### Drop Oceania

In this section, I’ll flter the `gpminder` data to remove observations
associated with the continent of Oceania. Additionally, remove unused
factor levels. Provide concrete information on the data before and after
removing these rows and Oceania; address the number of rows and the
levels of the affected factors.

``` r
gap_no_oceania <- gapminder %>%
  filter(continent != "Oceania")

str(gap_no_oceania)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

Looking at the structure, we can see that the number of observations in
`gap_no_oceania` is different from the original `gapminder` dataset:
1680 vs. 1704 respectiveley, however the number of levels in both
`country` and `continent` is still the same.

Let’s look at the unique values in `gap_no_oceania`

``` r
summary(gap_no_oceania$continent)
```

    ##   Africa Americas     Asia   Europe  Oceania 
    ##      624      300      396      360        0

We can see that the observations from Oceania were removed but is still
in the factor levels.

Let’s remove Oceania from the levels:

``` r
gap_no_oceania <- droplevels(gap_no_oceania)
nlevels(gap_no_oceania$continent) #check the number of levels
```

    ## [1] 4

``` r
summary(gap_no_oceania$continent) #check which are the remaining levels 
```

    ##   Africa Americas     Asia   Europe 
    ##      624      300      396      360

As noted, we are now left with 4 levels. The Oceania level was removed
by using `droplevels()`, this function drops unused levels from a
factor, since Oceania didn’t have observations anymore it was dropped.

### Reorder the levels of `continent`

Here, I will use the forcats package to change the order of the factor
levels, based on the maxmimum value of gdp per capita. I’m interested in
looking at the growth in gdp in countries from the Americas from the
last 20 years of data (1987-2007)

First let’s calculate the growth in this period:

``` r
gap_gdp <- gapminder %>%
  filter(continent == "Americas", year %in% c(1987, 2007)) %>%
  select(country, year, gdpPercap) %>%
  spread(key = year, value = gdpPercap) %>%
  mutate(growth = (`2007` - `1987`)/`1987`*100) %>% #calculate percentage of change
  arrange(growth)

kable(gap_gdp) %>%
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

country

</th>

<th style="text-align:right;">

1987

</th>

<th style="text-align:right;">

2007

</th>

<th style="text-align:right;">

growth

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Haiti

</td>

<td style="text-align:right;">

1823.016

</td>

<td style="text-align:right;">

1201.637

</td>

<td style="text-align:right;">

\-34.085211

</td>

</tr>

<tr>

<td style="text-align:left;">

Nicaragua

</td>

<td style="text-align:right;">

2955.984

</td>

<td style="text-align:right;">

2749.321

</td>

<td style="text-align:right;">

\-6.991357

</td>

</tr>

<tr>

<td style="text-align:left;">

Paraguay

</td>

<td style="text-align:right;">

3998.876

</td>

<td style="text-align:right;">

4172.838

</td>

<td style="text-align:right;">

4.350292

</td>

</tr>

<tr>

<td style="text-align:left;">

Ecuador

</td>

<td style="text-align:right;">

6481.777

</td>

<td style="text-align:right;">

6873.262

</td>

<td style="text-align:right;">

6.039784

</td>

</tr>

<tr>

<td style="text-align:left;">

Jamaica

</td>

<td style="text-align:right;">

6351.237

</td>

<td style="text-align:right;">

7320.880

</td>

<td style="text-align:right;">

15.266990

</td>

</tr>

<tr>

<td style="text-align:left;">

Venezuela

</td>

<td style="text-align:right;">

9883.585

</td>

<td style="text-align:right;">

11415.806

</td>

<td style="text-align:right;">

15.502686

</td>

</tr>

<tr>

<td style="text-align:left;">

Brazil

</td>

<td style="text-align:right;">

7807.096

</td>

<td style="text-align:right;">

9065.801

</td>

<td style="text-align:right;">

16.122577

</td>

</tr>

<tr>

<td style="text-align:left;">

Peru

</td>

<td style="text-align:right;">

6360.943

</td>

<td style="text-align:right;">

7408.906

</td>

<td style="text-align:right;">

16.474948

</td>

</tr>

<tr>

<td style="text-align:left;">

Honduras

</td>

<td style="text-align:right;">

3023.097

</td>

<td style="text-align:right;">

3548.331

</td>

<td style="text-align:right;">

17.374044

</td>

</tr>

<tr>

<td style="text-align:left;">

Cuba

</td>

<td style="text-align:right;">

7532.925

</td>

<td style="text-align:right;">

8948.103

</td>

<td style="text-align:right;">

18.786570

</td>

</tr>

<tr>

<td style="text-align:left;">

Guatemala

</td>

<td style="text-align:right;">

4246.486

</td>

<td style="text-align:right;">

5186.050

</td>

<td style="text-align:right;">

22.125683

</td>

</tr>

<tr>

<td style="text-align:left;">

Canada

</td>

<td style="text-align:right;">

26626.515

</td>

<td style="text-align:right;">

36319.235

</td>

<td style="text-align:right;">

36.402511

</td>

</tr>

<tr>

<td style="text-align:left;">

Mexico

</td>

<td style="text-align:right;">

8688.156

</td>

<td style="text-align:right;">

11977.575

</td>

<td style="text-align:right;">

37.860956

</td>

</tr>

<tr>

<td style="text-align:left;">

El Salvador

</td>

<td style="text-align:right;">

4140.442

</td>

<td style="text-align:right;">

5728.354

</td>

<td style="text-align:right;">

38.351253

</td>

</tr>

<tr>

<td style="text-align:left;">

Bolivia

</td>

<td style="text-align:right;">

2753.691

</td>

<td style="text-align:right;">

3822.137

</td>

<td style="text-align:right;">

38.800483

</td>

</tr>

<tr>

<td style="text-align:left;">

Panama

</td>

<td style="text-align:right;">

7034.779

</td>

<td style="text-align:right;">

9809.186

</td>

<td style="text-align:right;">

39.438430

</td>

</tr>

<tr>

<td style="text-align:left;">

Argentina

</td>

<td style="text-align:right;">

9139.671

</td>

<td style="text-align:right;">

12779.380

</td>

<td style="text-align:right;">

39.823185

</td>

</tr>

<tr>

<td style="text-align:left;">

Uruguay

</td>

<td style="text-align:right;">

7452.399

</td>

<td style="text-align:right;">

10611.463

</td>

<td style="text-align:right;">

42.389894

</td>

</tr>

<tr>

<td style="text-align:left;">

Colombia

</td>

<td style="text-align:right;">

4903.219

</td>

<td style="text-align:right;">

7006.580

</td>

<td style="text-align:right;">

42.897559

</td>

</tr>

<tr>

<td style="text-align:left;">

United States

</td>

<td style="text-align:right;">

29884.350

</td>

<td style="text-align:right;">

42951.653

</td>

<td style="text-align:right;">

43.726240

</td>

</tr>

<tr>

<td style="text-align:left;">

Puerto Rico

</td>

<td style="text-align:right;">

12281.342

</td>

<td style="text-align:right;">

19328.709

</td>

<td style="text-align:right;">

57.382712

</td>

</tr>

<tr>

<td style="text-align:left;">

Costa Rica

</td>

<td style="text-align:right;">

5629.915

</td>

<td style="text-align:right;">

9645.061

</td>

<td style="text-align:right;">

71.318055

</td>

</tr>

<tr>

<td style="text-align:left;">

Dominican Republic

</td>

<td style="text-align:right;">

2899.842

</td>

<td style="text-align:right;">

6025.375

</td>

<td style="text-align:right;">

107.782851

</td>

</tr>

<tr>

<td style="text-align:left;">

Chile

</td>

<td style="text-align:right;">

5547.064

</td>

<td style="text-align:right;">

13171.639

</td>

<td style="text-align:right;">

137.452451

</td>

</tr>

<tr>

<td style="text-align:left;">

Trinidad and Tobago

</td>

<td style="text-align:right;">

7388.598

</td>

<td style="text-align:right;">

18008.509

</td>

<td style="text-align:right;">

143.733787

</td>

</tr>

</tbody>

</table>

As we can see, the continents were arranged by the growth in GDP, but
let’s remember that it doesn’t change the order of levels. I will use
`fct_reorder` to make my plot look nicer and compare with ploting
without reordering.

``` r
p1 <- ggplot(gap_gdp, aes(x = country, y = growth)) +
  geom_bar(stat = 'identity', fill = "plum") +
  coord_flip() +
  theme_bw()+
  labs(x = "", y = "Change in GDP (%)", title = "GDP growth in the Americas, 1987-2007")
p2 <- ggplot(gap_gdp, aes(x = fct_reorder(country, growth), y = growth )) +
  geom_bar(stat = 'identity', fill = "plum") +
  coord_flip() +
  theme_bw()+
  labs(x = "", y = "Change in GDP (%)", title = "Ordered by economic growth")
  
grid.arrange(p1, p2, nrow = 1)
```

![](hw05_factor_figure_mgmt_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### Part 2: File I/O

#### `write_csv()`

First, I’ll check again that levels are saved

``` r
class(gap_gdp$country)  # corroborate that it is factor
```

    ## [1] "factor"

``` r
gap_gdp <- gap_gdp %>%
            mutate(country = fct_reorder(country, growth)) # Change factor levels to non-alphabetical

levels(gap_gdp$country) # corroborate reordering
```

    ##   [1] "Haiti"                    "Nicaragua"               
    ##   [3] "Paraguay"                 "Ecuador"                 
    ##   [5] "Jamaica"                  "Venezuela"               
    ##   [7] "Brazil"                   "Peru"                    
    ##   [9] "Honduras"                 "Cuba"                    
    ##  [11] "Guatemala"                "Canada"                  
    ##  [13] "Mexico"                   "El Salvador"             
    ##  [15] "Bolivia"                  "Panama"                  
    ##  [17] "Argentina"                "Uruguay"                 
    ##  [19] "Colombia"                 "United States"           
    ##  [21] "Puerto Rico"              "Costa Rica"              
    ##  [23] "Dominican Republic"       "Chile"                   
    ##  [25] "Trinidad and Tobago"      "Afghanistan"             
    ##  [27] "Albania"                  "Algeria"                 
    ##  [29] "Angola"                   "Australia"               
    ##  [31] "Austria"                  "Bahrain"                 
    ##  [33] "Bangladesh"               "Belgium"                 
    ##  [35] "Benin"                    "Bosnia and Herzegovina"  
    ##  [37] "Botswana"                 "Bulgaria"                
    ##  [39] "Burkina Faso"             "Burundi"                 
    ##  [41] "Cambodia"                 "Cameroon"                
    ##  [43] "Central African Republic" "Chad"                    
    ##  [45] "China"                    "Comoros"                 
    ##  [47] "Congo, Dem. Rep."         "Congo, Rep."             
    ##  [49] "Cote d'Ivoire"            "Croatia"                 
    ##  [51] "Czech Republic"           "Denmark"                 
    ##  [53] "Djibouti"                 "Egypt"                   
    ##  [55] "Equatorial Guinea"        "Eritrea"                 
    ##  [57] "Ethiopia"                 "Finland"                 
    ##  [59] "France"                   "Gabon"                   
    ##  [61] "Gambia"                   "Germany"                 
    ##  [63] "Ghana"                    "Greece"                  
    ##  [65] "Guinea"                   "Guinea-Bissau"           
    ##  [67] "Hong Kong, China"         "Hungary"                 
    ##  [69] "Iceland"                  "India"                   
    ##  [71] "Indonesia"                "Iran"                    
    ##  [73] "Iraq"                     "Ireland"                 
    ##  [75] "Israel"                   "Italy"                   
    ##  [77] "Japan"                    "Jordan"                  
    ##  [79] "Kenya"                    "Korea, Dem. Rep."        
    ##  [81] "Korea, Rep."              "Kuwait"                  
    ##  [83] "Lebanon"                  "Lesotho"                 
    ##  [85] "Liberia"                  "Libya"                   
    ##  [87] "Madagascar"               "Malawi"                  
    ##  [89] "Malaysia"                 "Mali"                    
    ##  [91] "Mauritania"               "Mauritius"               
    ##  [93] "Mongolia"                 "Montenegro"              
    ##  [95] "Morocco"                  "Mozambique"              
    ##  [97] "Myanmar"                  "Namibia"                 
    ##  [99] "Nepal"                    "Netherlands"             
    ## [101] "New Zealand"              "Niger"                   
    ## [103] "Nigeria"                  "Norway"                  
    ## [105] "Oman"                     "Pakistan"                
    ## [107] "Philippines"              "Poland"                  
    ## [109] "Portugal"                 "Reunion"                 
    ## [111] "Romania"                  "Rwanda"                  
    ## [113] "Sao Tome and Principe"    "Saudi Arabia"            
    ## [115] "Senegal"                  "Serbia"                  
    ## [117] "Sierra Leone"             "Singapore"               
    ## [119] "Slovak Republic"          "Slovenia"                
    ## [121] "Somalia"                  "South Africa"            
    ## [123] "Spain"                    "Sri Lanka"               
    ## [125] "Sudan"                    "Swaziland"               
    ## [127] "Sweden"                   "Switzerland"             
    ## [129] "Syria"                    "Taiwan"                  
    ## [131] "Tanzania"                 "Thailand"                
    ## [133] "Togo"                     "Tunisia"                 
    ## [135] "Turkey"                   "Uganda"                  
    ## [137] "United Kingdom"           "Vietnam"                 
    ## [139] "West Bank and Gaza"       "Yemen, Rep."             
    ## [141] "Zambia"                   "Zimbabwe"

``` r
# Save the dataframe into a csv file
write_csv(gap_gdp, "hw05_factor_figure_mgmt_files/gap_gdp.csv")
```

#### `read_csv()`

``` r
# Read in the data frame
gap_growth_new <- read_csv(file = "hw05_factor_figure_mgmt_files/gap_gdp.csv", col_names = TRUE)
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   `1987` = col_double(),
    ##   `2007` = col_double(),
    ##   growth = col_double()
    ## )

``` r
levels(gap_growth_new$country)
```

    ## NULL

``` r
gap_growth_new
```

    ## # A tibble: 25 x 4
    ##    country   `1987` `2007` growth
    ##    <chr>      <dbl>  <dbl>  <dbl>
    ##  1 Haiti      1823.  1202. -34.1 
    ##  2 Nicaragua  2956.  2749.  -6.99
    ##  3 Paraguay   3999.  4173.   4.35
    ##  4 Ecuador    6482.  6873.   6.04
    ##  5 Jamaica    6351.  7321.  15.3 
    ##  6 Venezuela  9884. 11416.  15.5 
    ##  7 Brazil     7807.  9066.  16.1 
    ##  8 Peru       6361.  7409.  16.5 
    ##  9 Honduras   3023.  3548.  17.4 
    ## 10 Cuba       7533.  8948.  18.8 
    ## # ... with 15 more rows

``` r
class(gap_growth_new$country)  # check class
```

    ## [1] "character"

The dataframe before generating the csv file considered `country` as a
factor. After opening the same file, the `country` variable is
considered as character.

#### `saveRDS()`

Now lets try with
`saveRDS`

``` r
saveRDS(gap_gdp, "hw05_factor_figure_mgmt_files/gap_gdp_factor.rds")
```

#### `readRDS()`

``` r
gap_growth_rds <- readRDS("hw05_factor_figure_mgmt_files/gap_gdp_factor.rds")
levels(gap_growth_rds$country)
```

    ##   [1] "Haiti"                    "Nicaragua"               
    ##   [3] "Paraguay"                 "Ecuador"                 
    ##   [5] "Jamaica"                  "Venezuela"               
    ##   [7] "Brazil"                   "Peru"                    
    ##   [9] "Honduras"                 "Cuba"                    
    ##  [11] "Guatemala"                "Canada"                  
    ##  [13] "Mexico"                   "El Salvador"             
    ##  [15] "Bolivia"                  "Panama"                  
    ##  [17] "Argentina"                "Uruguay"                 
    ##  [19] "Colombia"                 "United States"           
    ##  [21] "Puerto Rico"              "Costa Rica"              
    ##  [23] "Dominican Republic"       "Chile"                   
    ##  [25] "Trinidad and Tobago"      "Afghanistan"             
    ##  [27] "Albania"                  "Algeria"                 
    ##  [29] "Angola"                   "Australia"               
    ##  [31] "Austria"                  "Bahrain"                 
    ##  [33] "Bangladesh"               "Belgium"                 
    ##  [35] "Benin"                    "Bosnia and Herzegovina"  
    ##  [37] "Botswana"                 "Bulgaria"                
    ##  [39] "Burkina Faso"             "Burundi"                 
    ##  [41] "Cambodia"                 "Cameroon"                
    ##  [43] "Central African Republic" "Chad"                    
    ##  [45] "China"                    "Comoros"                 
    ##  [47] "Congo, Dem. Rep."         "Congo, Rep."             
    ##  [49] "Cote d'Ivoire"            "Croatia"                 
    ##  [51] "Czech Republic"           "Denmark"                 
    ##  [53] "Djibouti"                 "Egypt"                   
    ##  [55] "Equatorial Guinea"        "Eritrea"                 
    ##  [57] "Ethiopia"                 "Finland"                 
    ##  [59] "France"                   "Gabon"                   
    ##  [61] "Gambia"                   "Germany"                 
    ##  [63] "Ghana"                    "Greece"                  
    ##  [65] "Guinea"                   "Guinea-Bissau"           
    ##  [67] "Hong Kong, China"         "Hungary"                 
    ##  [69] "Iceland"                  "India"                   
    ##  [71] "Indonesia"                "Iran"                    
    ##  [73] "Iraq"                     "Ireland"                 
    ##  [75] "Israel"                   "Italy"                   
    ##  [77] "Japan"                    "Jordan"                  
    ##  [79] "Kenya"                    "Korea, Dem. Rep."        
    ##  [81] "Korea, Rep."              "Kuwait"                  
    ##  [83] "Lebanon"                  "Lesotho"                 
    ##  [85] "Liberia"                  "Libya"                   
    ##  [87] "Madagascar"               "Malawi"                  
    ##  [89] "Malaysia"                 "Mali"                    
    ##  [91] "Mauritania"               "Mauritius"               
    ##  [93] "Mongolia"                 "Montenegro"              
    ##  [95] "Morocco"                  "Mozambique"              
    ##  [97] "Myanmar"                  "Namibia"                 
    ##  [99] "Nepal"                    "Netherlands"             
    ## [101] "New Zealand"              "Niger"                   
    ## [103] "Nigeria"                  "Norway"                  
    ## [105] "Oman"                     "Pakistan"                
    ## [107] "Philippines"              "Poland"                  
    ## [109] "Portugal"                 "Reunion"                 
    ## [111] "Romania"                  "Rwanda"                  
    ## [113] "Sao Tome and Principe"    "Saudi Arabia"            
    ## [115] "Senegal"                  "Serbia"                  
    ## [117] "Sierra Leone"             "Singapore"               
    ## [119] "Slovak Republic"          "Slovenia"                
    ## [121] "Somalia"                  "South Africa"            
    ## [123] "Spain"                    "Sri Lanka"               
    ## [125] "Sudan"                    "Swaziland"               
    ## [127] "Sweden"                   "Switzerland"             
    ## [129] "Syria"                    "Taiwan"                  
    ## [131] "Tanzania"                 "Thailand"                
    ## [133] "Togo"                     "Tunisia"                 
    ## [135] "Turkey"                   "Uganda"                  
    ## [137] "United Kingdom"           "Vietnam"                 
    ## [139] "West Bank and Gaza"       "Yemen, Rep."             
    ## [141] "Zambia"                   "Zimbabwe"

``` r
class(gap_growth_rds$country)
```

    ## [1] "factor"

We can see that `saveRDS()` and `readRDS()` conserve the datatypes when
saving and reading an rds
file.

#### `dput()`

``` r
dput(gap_gdp, "hw05_factor_figure_mgmt_files/gap_gdp_factor.txt")
```

#### `dget()`

``` r
gap_growth_dget <- dget("hw05_factor_figure_mgmt_files/gap_gdp_factor.txt")
levels(gap_growth_dget$country)
```

    ##   [1] "Haiti"                    "Nicaragua"               
    ##   [3] "Paraguay"                 "Ecuador"                 
    ##   [5] "Jamaica"                  "Venezuela"               
    ##   [7] "Brazil"                   "Peru"                    
    ##   [9] "Honduras"                 "Cuba"                    
    ##  [11] "Guatemala"                "Canada"                  
    ##  [13] "Mexico"                   "El Salvador"             
    ##  [15] "Bolivia"                  "Panama"                  
    ##  [17] "Argentina"                "Uruguay"                 
    ##  [19] "Colombia"                 "United States"           
    ##  [21] "Puerto Rico"              "Costa Rica"              
    ##  [23] "Dominican Republic"       "Chile"                   
    ##  [25] "Trinidad and Tobago"      "Afghanistan"             
    ##  [27] "Albania"                  "Algeria"                 
    ##  [29] "Angola"                   "Australia"               
    ##  [31] "Austria"                  "Bahrain"                 
    ##  [33] "Bangladesh"               "Belgium"                 
    ##  [35] "Benin"                    "Bosnia and Herzegovina"  
    ##  [37] "Botswana"                 "Bulgaria"                
    ##  [39] "Burkina Faso"             "Burundi"                 
    ##  [41] "Cambodia"                 "Cameroon"                
    ##  [43] "Central African Republic" "Chad"                    
    ##  [45] "China"                    "Comoros"                 
    ##  [47] "Congo, Dem. Rep."         "Congo, Rep."             
    ##  [49] "Cote d'Ivoire"            "Croatia"                 
    ##  [51] "Czech Republic"           "Denmark"                 
    ##  [53] "Djibouti"                 "Egypt"                   
    ##  [55] "Equatorial Guinea"        "Eritrea"                 
    ##  [57] "Ethiopia"                 "Finland"                 
    ##  [59] "France"                   "Gabon"                   
    ##  [61] "Gambia"                   "Germany"                 
    ##  [63] "Ghana"                    "Greece"                  
    ##  [65] "Guinea"                   "Guinea-Bissau"           
    ##  [67] "Hong Kong, China"         "Hungary"                 
    ##  [69] "Iceland"                  "India"                   
    ##  [71] "Indonesia"                "Iran"                    
    ##  [73] "Iraq"                     "Ireland"                 
    ##  [75] "Israel"                   "Italy"                   
    ##  [77] "Japan"                    "Jordan"                  
    ##  [79] "Kenya"                    "Korea, Dem. Rep."        
    ##  [81] "Korea, Rep."              "Kuwait"                  
    ##  [83] "Lebanon"                  "Lesotho"                 
    ##  [85] "Liberia"                  "Libya"                   
    ##  [87] "Madagascar"               "Malawi"                  
    ##  [89] "Malaysia"                 "Mali"                    
    ##  [91] "Mauritania"               "Mauritius"               
    ##  [93] "Mongolia"                 "Montenegro"              
    ##  [95] "Morocco"                  "Mozambique"              
    ##  [97] "Myanmar"                  "Namibia"                 
    ##  [99] "Nepal"                    "Netherlands"             
    ## [101] "New Zealand"              "Niger"                   
    ## [103] "Nigeria"                  "Norway"                  
    ## [105] "Oman"                     "Pakistan"                
    ## [107] "Philippines"              "Poland"                  
    ## [109] "Portugal"                 "Reunion"                 
    ## [111] "Romania"                  "Rwanda"                  
    ## [113] "Sao Tome and Principe"    "Saudi Arabia"            
    ## [115] "Senegal"                  "Serbia"                  
    ## [117] "Sierra Leone"             "Singapore"               
    ## [119] "Slovak Republic"          "Slovenia"                
    ## [121] "Somalia"                  "South Africa"            
    ## [123] "Spain"                    "Sri Lanka"               
    ## [125] "Sudan"                    "Swaziland"               
    ## [127] "Sweden"                   "Switzerland"             
    ## [129] "Syria"                    "Taiwan"                  
    ## [131] "Tanzania"                 "Thailand"                
    ## [133] "Togo"                     "Tunisia"                 
    ## [135] "Turkey"                   "Uganda"                  
    ## [137] "United Kingdom"           "Vietnam"                 
    ## [139] "West Bank and Gaza"       "Yemen, Rep."             
    ## [141] "Zambia"                   "Zimbabwe"

``` r
class(gap_growth_dget$country)
```

    ## [1] "factor"

Similar with `RDS`, We can see that `dput()` and `dget()` conserve the
datatypes when saving and reading a txt file. The variable `country` is
still considered a factor and the factor levels remain.

### Part 3: Visualization design

In this section I’ll create a figure and improve its design.

``` r
gap_gdp_mean <- gapminder %>%
  filter(continent != "Oceania") %>% #remove Oceania as it is only 2 countries
  group_by(continent,year) %>% #group by continent
  mutate(meanGDP = mean(gdpPercap))

plot<- gap_gdp_mean %>% 
        ggplot(aes(x = year, y = meanGDP, color = continent)) + #group data by years
        scale_color_brewer(palette = "Dark2") + #change color palette
        guides(fill=FALSE) + #remove unnecessary color legend
        geom_point() + # plot data poings
        geom_smooth(method = lm, size = 0.5) + #add linear regression line
        facet_wrap(~continent) + #create subplots by continent
        guides(color=FALSE) + #remove unnecessary color legend
        theme_bw() + #select theme
        scale_y_log10(labels=dollar_format()) +
        scale_x_continuous(breaks = seq(1952,2007, 5)) + # change scale breaks in x axis
        labs(y = "Mean GDP", x = "", title = "GDP per capita") + #modify labels and title
        #scale_y_continuous(labels=dollar_format()) + # add commas and $ sign to the y axis ticks labels
        theme(strip.text = element_text(face="bold", size=10), # change text size of the strips
              axis.text = element_text(size=10), #lticj labels size
              axis.text.x = element_text(angle = 45, hjust = 1), #change x axis tick labels angle
              strip.background = element_rect(fill = "lavender")) #change strip background color

plot
```

![](hw05_factor_figure_mgmt_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Not let make it interactive with plotly

``` r
#install.packages("plotly")
library(plotly)
ggplotly(p2)
```

<!--html_preserve-->

<div id="htmlwidget-4d21a632015a80b59aea" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-4d21a632015a80b59aea">{"x":{"data":[{"orientation":"h","width":[0.9,0.9,0.9,0.9,0.9,0.9,0.9,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999,0.899999999999999],"base":[-34.0852105908155,-6.99135664409593,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"x":[34.0852105908155,6.99135664409593,4.35029198875861,6.03978405031189,15.2669895868852,15.5026854786947,16.1225766449278,16.4749478788166,17.3740438793685,18.7865696860671,22.125683088386,36.4025106893608,37.8609564085195,38.3512528324098,38.8004828384025,39.4384302833696,39.8231850587161,42.3898939675783,42.8975592585695,43.726239656283,57.3827123423844,71.3180549832206,107.782851216722,137.452451136908,143.733786455953],"y":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],"text":["fct_reorder(country, growth): Haiti<br />growth:  34.085211","fct_reorder(country, growth): Nicaragua<br />growth:   6.991357","fct_reorder(country, growth): Paraguay<br />growth:   4.350292","fct_reorder(country, growth): Ecuador<br />growth:   6.039784","fct_reorder(country, growth): Jamaica<br />growth:  15.266990","fct_reorder(country, growth): Venezuela<br />growth:  15.502685","fct_reorder(country, growth): Brazil<br />growth:  16.122577","fct_reorder(country, growth): Peru<br />growth:  16.474948","fct_reorder(country, growth): Honduras<br />growth:  17.374044","fct_reorder(country, growth): Cuba<br />growth:  18.786570","fct_reorder(country, growth): Guatemala<br />growth:  22.125683","fct_reorder(country, growth): Canada<br />growth:  36.402511","fct_reorder(country, growth): Mexico<br />growth:  37.860956","fct_reorder(country, growth): El Salvador<br />growth:  38.351253","fct_reorder(country, growth): Bolivia<br />growth:  38.800483","fct_reorder(country, growth): Panama<br />growth:  39.438430","fct_reorder(country, growth): Argentina<br />growth:  39.823185","fct_reorder(country, growth): Uruguay<br />growth:  42.389894","fct_reorder(country, growth): Colombia<br />growth:  42.897559","fct_reorder(country, growth): United States<br />growth:  43.726240","fct_reorder(country, growth): Puerto Rico<br />growth:  57.382712","fct_reorder(country, growth): Costa Rica<br />growth:  71.318055","fct_reorder(country, growth): Dominican Republic<br />growth: 107.782851","fct_reorder(country, growth): Chile<br />growth: 137.452451","fct_reorder(country, growth): Trinidad and Tobago<br />growth: 143.733786"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(221,160,221,1)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":122.009132420091},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Ordered by economic growth","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-42.9761604431539,152.624736308291],"tickmode":"array","ticktext":["0","50","100","150"],"tickvals":[0,50,100,150],"categoryorder":"array","categoryarray":["0","50","100","150"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"Change in GDP (%)","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,25.6],"tickmode":"array","ticktext":["Haiti","Nicaragua","Paraguay","Ecuador","Jamaica","Venezuela","Brazil","Peru","Honduras","Cuba","Guatemala","Canada","Mexico","El Salvador","Bolivia","Panama","Argentina","Uruguay","Colombia","United States","Puerto Rico","Costa Rica","Dominican Republic","Chile","Trinidad and Tobago"],"tickvals":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25],"categoryorder":"array","categoryarray":["Haiti","Nicaragua","Paraguay","Ecuador","Jamaica","Venezuela","Brazil","Peru","Honduras","Cuba","Guatemala","Canada","Mexico","El Salvador","Bolivia","Panama","Argentina","Uruguay","Colombia","United States","Puerto Rico","Costa Rica","Dominican Republic","Chile","Trinidad and Tobago"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"468d3acedf20":{"x":{},"y":{},"type":"bar"}},"cur_data":"468d3acedf20","visdat":{"468d3acedf20":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
ggplotly(plot)
```

<!--html_preserve-->

<div id="htmlwidget-44e23a35dec55c4b37ba" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-44e23a35dec55c4b37ba">{"x":{"data":[{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353,3.09780286076353,3.14152378913993,3.20359819697512,3.31183092578127,3.36914452224631,3.4126181934939,3.39473054838362,3.35844293922048,3.35827954251483,3.37635054579101,3.41487063501756,3.48982249230353],"text":["year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa","year: 1952<br />meanGDP:  1252.572<br />continent: Africa","year: 1957<br />meanGDP:  1385.236<br />continent: Africa","year: 1962<br />meanGDP:  1598.079<br />continent: Africa","year: 1967<br />meanGDP:  2050.364<br />continent: Africa","year: 1972<br />meanGDP:  2339.616<br />continent: Africa","year: 1977<br />meanGDP:  2585.939<br />continent: Africa","year: 1982<br />meanGDP:  2481.593<br />continent: Africa","year: 1987<br />meanGDP:  2282.669<br />continent: Africa","year: 1992<br />meanGDP:  2281.810<br />continent: Africa","year: 1997<br />meanGDP:  2378.760<br />continent: Africa","year: 2002<br />meanGDP:  2599.385<br />continent: Africa","year: 2007<br />meanGDP:  3089.033<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(27,158,119,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(27,158,119,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794,3.61056036525017,3.66426991474475,3.69033271685992,3.75344926455876,3.81233396485541,3.86640591933869,3.87545120563652,3.89172698173537,3.90552250742363,3.94886760534905,3.96790710843737,4.04151236122794],"text":["year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas","year: 1952<br />meanGDP:  4079.063<br />continent: Americas","year: 1957<br />meanGDP:  4616.044<br />continent: Americas","year: 1962<br />meanGDP:  4901.542<br />continent: Americas","year: 1967<br />meanGDP:  5668.253<br />continent: Americas","year: 1972<br />meanGDP:  6491.334<br />continent: Americas","year: 1977<br />meanGDP:  7352.007<br />continent: Americas","year: 1982<br />meanGDP:  7506.737<br />continent: Americas","year: 1987<br />meanGDP:  7793.400<br />continent: Americas","year: 1992<br />meanGDP:  8044.934<br />continent: Americas","year: 1997<br />meanGDP:  8889.301<br />continent: Americas","year: 2002<br />meanGDP:  9287.677<br />continent: Americas","year: 2007<br />meanGDP: 11003.032<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(217,95,2,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(217,95,2,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":true,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762,3.7156260120457,3.76250848350692,3.7581068412446,3.77605968114783,3.9131496527027,3.89161070844018,3.87123045259187,3.88128343368659,3.93649817232693,3.99273432409034,4.0074955920098,4.09597185792762],"text":["year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia","year: 1952<br />meanGDP:  5195.484<br />continent: Asia","year: 1957<br />meanGDP:  5787.733<br />continent: Asia","year: 1962<br />meanGDP:  5729.370<br />continent: Asia","year: 1967<br />meanGDP:  5971.173<br />continent: Asia","year: 1972<br />meanGDP:  8187.469<br />continent: Asia","year: 1977<br />meanGDP:  7791.314<br />continent: Asia","year: 1982<br />meanGDP:  7434.135<br />continent: Asia","year: 1987<br />meanGDP:  7608.227<br />continent: Asia","year: 1992<br />meanGDP:  8639.690<br />continent: Asia","year: 1997<br />meanGDP:  9834.093<br />continent: Asia","year: 2002<br />meanGDP: 10174.090<br />continent: Asia","year: 2007<br />meanGDP: 12473.027<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(117,112,179,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(117,112,179,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":true,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"x":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007,1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"y":[3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467,3.75289756108011,3.84279719452701,3.9224912190118,4.00620169497082,4.09619980399585,4.15484920636308,4.193622541852,4.23588963792581,4.23201894353838,4.28050511231307,4.33669447805992,4.3988854218467],"text":["year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe","year: 1952<br />meanGDP:  5661.057<br />continent: Europe","year: 1957<br />meanGDP:  6963.013<br />continent: Europe","year: 1962<br />meanGDP:  8365.487<br />continent: Europe","year: 1967<br />meanGDP: 10143.824<br />continent: Europe","year: 1972<br />meanGDP: 12479.575<br />continent: Europe","year: 1977<br />meanGDP: 14283.979<br />continent: Europe","year: 1982<br />meanGDP: 15617.897<br />continent: Europe","year: 1987<br />meanGDP: 17214.311<br />continent: Europe","year: 1992<br />meanGDP: 17061.568<br />continent: Europe","year: 1997<br />meanGDP: 19076.782<br />continent: Europe","year: 2002<br />meanGDP: 21711.732<br />continent: Europe","year: 2007<br />meanGDP: 25054.482<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(231,41,138,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(231,41,138,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":true,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007],"y":[3.17042072703917,3.17439533983909,3.178369952639,3.18234456543892,3.18631917823884,3.19029379103876,3.19426840383867,3.19824301663859,3.20221762943851,3.20619224223842,3.21016685503834,3.21414146783826,3.21811608063818,3.22209069343809,3.22606530623801,3.23003991903793,3.23401453183785,3.23798914463776,3.24196375743768,3.2459383702376,3.24991298303751,3.25388759583743,3.25786220863735,3.26183682143727,3.26581143423718,3.2697860470371,3.27376065983702,3.27773527263693,3.28170988543685,3.28568449823677,3.28965911103669,3.29363372383661,3.29760833663652,3.30158294943644,3.30555756223636,3.30953217503627,3.31350678783619,3.31748140063611,3.32145601343603,3.32543062623594,3.32940523903586,3.33337985183578,3.33735446463569,3.34132907743561,3.34530369023553,3.34927830303545,3.35325291583536,3.35722752863528,3.3612021414352,3.36517675423512,3.36915136703503,3.37312597983495,3.37710059263487,3.38107520543478,3.3850498182347,3.38902443103462,3.39299904383454,3.39697365663445,3.40094826943437,3.40492288223429,3.40889749503421,3.41287210783412,3.41684672063404,3.42082133343396,3.42479594623387,3.42877055903379,3.43274517183371,3.43671978463363,3.44069439743354,3.44466901023346,3.44864362303338,3.45261823583329,3.45659284863321,3.46056746143313,3.46454207423305,3.46851668703297,3.47249129983288,3.4764659126328,3.48044052543272,3.48441513823263],"text":["year: 1952.000<br />meanGDP: 3.170421<br />continent: Africa","year: 1952.696<br />meanGDP: 3.174395<br />continent: Africa","year: 1953.392<br />meanGDP: 3.178370<br />continent: Africa","year: 1954.089<br />meanGDP: 3.182345<br />continent: Africa","year: 1954.785<br />meanGDP: 3.186319<br />continent: Africa","year: 1955.481<br />meanGDP: 3.190294<br />continent: Africa","year: 1956.177<br />meanGDP: 3.194268<br />continent: Africa","year: 1956.873<br />meanGDP: 3.198243<br />continent: Africa","year: 1957.570<br />meanGDP: 3.202218<br />continent: Africa","year: 1958.266<br />meanGDP: 3.206192<br />continent: Africa","year: 1958.962<br />meanGDP: 3.210167<br />continent: Africa","year: 1959.658<br />meanGDP: 3.214141<br />continent: Africa","year: 1960.354<br />meanGDP: 3.218116<br />continent: Africa","year: 1961.051<br />meanGDP: 3.222091<br />continent: Africa","year: 1961.747<br />meanGDP: 3.226065<br />continent: Africa","year: 1962.443<br />meanGDP: 3.230040<br />continent: Africa","year: 1963.139<br />meanGDP: 3.234015<br />continent: Africa","year: 1963.835<br />meanGDP: 3.237989<br />continent: Africa","year: 1964.532<br />meanGDP: 3.241964<br />continent: Africa","year: 1965.228<br />meanGDP: 3.245938<br />continent: Africa","year: 1965.924<br />meanGDP: 3.249913<br />continent: Africa","year: 1966.620<br />meanGDP: 3.253888<br />continent: Africa","year: 1967.316<br />meanGDP: 3.257862<br />continent: Africa","year: 1968.013<br />meanGDP: 3.261837<br />continent: Africa","year: 1968.709<br />meanGDP: 3.265811<br />continent: Africa","year: 1969.405<br />meanGDP: 3.269786<br />continent: Africa","year: 1970.101<br />meanGDP: 3.273761<br />continent: Africa","year: 1970.797<br />meanGDP: 3.277735<br />continent: Africa","year: 1971.494<br />meanGDP: 3.281710<br />continent: Africa","year: 1972.190<br />meanGDP: 3.285684<br />continent: Africa","year: 1972.886<br />meanGDP: 3.289659<br />continent: Africa","year: 1973.582<br />meanGDP: 3.293634<br />continent: Africa","year: 1974.278<br />meanGDP: 3.297608<br />continent: Africa","year: 1974.975<br />meanGDP: 3.301583<br />continent: Africa","year: 1975.671<br />meanGDP: 3.305558<br />continent: Africa","year: 1976.367<br />meanGDP: 3.309532<br />continent: Africa","year: 1977.063<br />meanGDP: 3.313507<br />continent: Africa","year: 1977.759<br />meanGDP: 3.317481<br />continent: Africa","year: 1978.456<br />meanGDP: 3.321456<br />continent: Africa","year: 1979.152<br />meanGDP: 3.325431<br />continent: Africa","year: 1979.848<br />meanGDP: 3.329405<br />continent: Africa","year: 1980.544<br />meanGDP: 3.333380<br />continent: Africa","year: 1981.241<br />meanGDP: 3.337354<br />continent: Africa","year: 1981.937<br />meanGDP: 3.341329<br />continent: Africa","year: 1982.633<br />meanGDP: 3.345304<br />continent: Africa","year: 1983.329<br />meanGDP: 3.349278<br />continent: Africa","year: 1984.025<br />meanGDP: 3.353253<br />continent: Africa","year: 1984.722<br />meanGDP: 3.357228<br />continent: Africa","year: 1985.418<br />meanGDP: 3.361202<br />continent: Africa","year: 1986.114<br />meanGDP: 3.365177<br />continent: Africa","year: 1986.810<br />meanGDP: 3.369151<br />continent: Africa","year: 1987.506<br />meanGDP: 3.373126<br />continent: Africa","year: 1988.203<br />meanGDP: 3.377101<br />continent: Africa","year: 1988.899<br />meanGDP: 3.381075<br />continent: Africa","year: 1989.595<br />meanGDP: 3.385050<br />continent: Africa","year: 1990.291<br />meanGDP: 3.389024<br />continent: Africa","year: 1990.987<br />meanGDP: 3.392999<br />continent: Africa","year: 1991.684<br />meanGDP: 3.396974<br />continent: Africa","year: 1992.380<br />meanGDP: 3.400948<br />continent: Africa","year: 1993.076<br />meanGDP: 3.404923<br />continent: Africa","year: 1993.772<br />meanGDP: 3.408897<br />continent: Africa","year: 1994.468<br />meanGDP: 3.412872<br />continent: Africa","year: 1995.165<br />meanGDP: 3.416847<br />continent: Africa","year: 1995.861<br />meanGDP: 3.420821<br />continent: Africa","year: 1996.557<br />meanGDP: 3.424796<br />continent: Africa","year: 1997.253<br />meanGDP: 3.428771<br />continent: Africa","year: 1997.949<br />meanGDP: 3.432745<br />continent: Africa","year: 1998.646<br />meanGDP: 3.436720<br />continent: Africa","year: 1999.342<br />meanGDP: 3.440694<br />continent: Africa","year: 2000.038<br />meanGDP: 3.444669<br />continent: Africa","year: 2000.734<br />meanGDP: 3.448644<br />continent: Africa","year: 2001.430<br />meanGDP: 3.452618<br />continent: Africa","year: 2002.127<br />meanGDP: 3.456593<br />continent: Africa","year: 2002.823<br />meanGDP: 3.460567<br />continent: Africa","year: 2003.519<br />meanGDP: 3.464542<br />continent: Africa","year: 2004.215<br />meanGDP: 3.468517<br />continent: Africa","year: 2004.911<br />meanGDP: 3.472491<br />continent: Africa","year: 2005.608<br />meanGDP: 3.476466<br />continent: Africa","year: 2006.304<br />meanGDP: 3.480441<br />continent: Africa","year: 2007.000<br />meanGDP: 3.484415<br />continent: Africa"],"type":"scatter","mode":"lines","name":"Africa","line":{"width":1.88976377952756,"color":"rgba(27,158,119,1)","dash":"solid"},"hoveron":"points","legendgroup":"Africa","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007],"y":[3.63779996468847,3.64280996540398,3.6478199661195,3.65282996683502,3.65783996755054,3.66284996826606,3.66785996898158,3.6728699696971,3.67787997041262,3.68288997112813,3.68789997184365,3.69290997255917,3.69791997327469,3.70292997399021,3.70793997470573,3.71294997542124,3.71795997613676,3.72296997685228,3.7279799775678,3.73298997828332,3.73799997899884,3.74300997971436,3.74801998042988,3.75302998114539,3.75803998186091,3.76304998257643,3.76805998329195,3.77306998400747,3.77807998472299,3.7830899854385,3.78809998615402,3.79310998686954,3.79811998758506,3.80312998830058,3.8081399890161,3.81314998973162,3.81815999044714,3.82316999116265,3.82817999187817,3.83318999259369,3.83819999330921,3.84320999402473,3.84821999474025,3.85322999545576,3.85823999617128,3.8632499968868,3.86825999760232,3.87326999831784,3.87827999903336,3.88328999974888,3.88830000046439,3.89331000117991,3.89832000189543,3.90333000261095,3.90834000332647,3.91335000404199,3.91836000475751,3.92337000547302,3.92838000618854,3.93339000690406,3.93840000761958,3.9434100083351,3.94842000905062,3.95343000976614,3.95844001048165,3.96345001119717,3.96846001191269,3.97347001262821,3.97848001334373,3.98349001405925,3.98850001477477,3.99351001549028,3.9985200162058,4.00353001692132,4.00854001763684,4.01355001835236,4.01856001906788,4.0235700197834,4.02858002049891,4.03359002121443],"text":["year: 1952.000<br />meanGDP: 3.637800<br />continent: Americas","year: 1952.696<br />meanGDP: 3.642810<br />continent: Americas","year: 1953.392<br />meanGDP: 3.647820<br />continent: Americas","year: 1954.089<br />meanGDP: 3.652830<br />continent: Americas","year: 1954.785<br />meanGDP: 3.657840<br />continent: Americas","year: 1955.481<br />meanGDP: 3.662850<br />continent: Americas","year: 1956.177<br />meanGDP: 3.667860<br />continent: Americas","year: 1956.873<br />meanGDP: 3.672870<br />continent: Americas","year: 1957.570<br />meanGDP: 3.677880<br />continent: Americas","year: 1958.266<br />meanGDP: 3.682890<br />continent: Americas","year: 1958.962<br />meanGDP: 3.687900<br />continent: Americas","year: 1959.658<br />meanGDP: 3.692910<br />continent: Americas","year: 1960.354<br />meanGDP: 3.697920<br />continent: Americas","year: 1961.051<br />meanGDP: 3.702930<br />continent: Americas","year: 1961.747<br />meanGDP: 3.707940<br />continent: Americas","year: 1962.443<br />meanGDP: 3.712950<br />continent: Americas","year: 1963.139<br />meanGDP: 3.717960<br />continent: Americas","year: 1963.835<br />meanGDP: 3.722970<br />continent: Americas","year: 1964.532<br />meanGDP: 3.727980<br />continent: Americas","year: 1965.228<br />meanGDP: 3.732990<br />continent: Americas","year: 1965.924<br />meanGDP: 3.738000<br />continent: Americas","year: 1966.620<br />meanGDP: 3.743010<br />continent: Americas","year: 1967.316<br />meanGDP: 3.748020<br />continent: Americas","year: 1968.013<br />meanGDP: 3.753030<br />continent: Americas","year: 1968.709<br />meanGDP: 3.758040<br />continent: Americas","year: 1969.405<br />meanGDP: 3.763050<br />continent: Americas","year: 1970.101<br />meanGDP: 3.768060<br />continent: Americas","year: 1970.797<br />meanGDP: 3.773070<br />continent: Americas","year: 1971.494<br />meanGDP: 3.778080<br />continent: Americas","year: 1972.190<br />meanGDP: 3.783090<br />continent: Americas","year: 1972.886<br />meanGDP: 3.788100<br />continent: Americas","year: 1973.582<br />meanGDP: 3.793110<br />continent: Americas","year: 1974.278<br />meanGDP: 3.798120<br />continent: Americas","year: 1974.975<br />meanGDP: 3.803130<br />continent: Americas","year: 1975.671<br />meanGDP: 3.808140<br />continent: Americas","year: 1976.367<br />meanGDP: 3.813150<br />continent: Americas","year: 1977.063<br />meanGDP: 3.818160<br />continent: Americas","year: 1977.759<br />meanGDP: 3.823170<br />continent: Americas","year: 1978.456<br />meanGDP: 3.828180<br />continent: Americas","year: 1979.152<br />meanGDP: 3.833190<br />continent: Americas","year: 1979.848<br />meanGDP: 3.838200<br />continent: Americas","year: 1980.544<br />meanGDP: 3.843210<br />continent: Americas","year: 1981.241<br />meanGDP: 3.848220<br />continent: Americas","year: 1981.937<br />meanGDP: 3.853230<br />continent: Americas","year: 1982.633<br />meanGDP: 3.858240<br />continent: Americas","year: 1983.329<br />meanGDP: 3.863250<br />continent: Americas","year: 1984.025<br />meanGDP: 3.868260<br />continent: Americas","year: 1984.722<br />meanGDP: 3.873270<br />continent: Americas","year: 1985.418<br />meanGDP: 3.878280<br />continent: Americas","year: 1986.114<br />meanGDP: 3.883290<br />continent: Americas","year: 1986.810<br />meanGDP: 3.888300<br />continent: Americas","year: 1987.506<br />meanGDP: 3.893310<br />continent: Americas","year: 1988.203<br />meanGDP: 3.898320<br />continent: Americas","year: 1988.899<br />meanGDP: 3.903330<br />continent: Americas","year: 1989.595<br />meanGDP: 3.908340<br />continent: Americas","year: 1990.291<br />meanGDP: 3.913350<br />continent: Americas","year: 1990.987<br />meanGDP: 3.918360<br />continent: Americas","year: 1991.684<br />meanGDP: 3.923370<br />continent: Americas","year: 1992.380<br />meanGDP: 3.928380<br />continent: Americas","year: 1993.076<br />meanGDP: 3.933390<br />continent: Americas","year: 1993.772<br />meanGDP: 3.938400<br />continent: Americas","year: 1994.468<br />meanGDP: 3.943410<br />continent: Americas","year: 1995.165<br />meanGDP: 3.948420<br />continent: Americas","year: 1995.861<br />meanGDP: 3.953430<br />continent: Americas","year: 1996.557<br />meanGDP: 3.958440<br />continent: Americas","year: 1997.253<br />meanGDP: 3.963450<br />continent: Americas","year: 1997.949<br />meanGDP: 3.968460<br />continent: Americas","year: 1998.646<br />meanGDP: 3.973470<br />continent: Americas","year: 1999.342<br />meanGDP: 3.978480<br />continent: Americas","year: 2000.038<br />meanGDP: 3.983490<br />continent: Americas","year: 2000.734<br />meanGDP: 3.988500<br />continent: Americas","year: 2001.430<br />meanGDP: 3.993510<br />continent: Americas","year: 2002.127<br />meanGDP: 3.998520<br />continent: Americas","year: 2002.823<br />meanGDP: 4.003530<br />continent: Americas","year: 2003.519<br />meanGDP: 4.008540<br />continent: Americas","year: 2004.215<br />meanGDP: 4.013550<br />continent: Americas","year: 2004.911<br />meanGDP: 4.018560<br />continent: Americas","year: 2005.608<br />meanGDP: 4.023570<br />continent: Americas","year: 2006.304<br />meanGDP: 4.028580<br />continent: Americas","year: 2007.000<br />meanGDP: 4.033590<br />continent: Americas"],"type":"scatter","mode":"lines","name":"Americas","line":{"width":1.88976377952756,"color":"rgba(217,95,2,1)","dash":"solid"},"hoveron":"points","legendgroup":"Americas","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007],"y":[3.71588266115339,3.72012671870166,3.72437077624993,3.7286148337982,3.73285889134647,3.73710294889474,3.74134700644301,3.74559106399128,3.74983512153955,3.75407917908782,3.75832323663609,3.76256729418436,3.76681135173263,3.7710554092809,3.77529946682917,3.77954352437744,3.78378758192571,3.78803163947398,3.79227569702225,3.79651975457052,3.80076381211879,3.80500786966706,3.80925192721533,3.8134959847636,3.81774004231187,3.82198409986014,3.82622815740841,3.83047221495668,3.83471627250495,3.83896033005322,3.84320438760149,3.84744844514976,3.85169250269803,3.8559365602463,3.86018061779457,3.86442467534284,3.86866873289111,3.87291279043938,3.87715684798765,3.88140090553592,3.88564496308419,3.88988902063246,3.89413307818073,3.898377135729,3.90262119327727,3.90686525082554,3.9111093083738,3.91535336592208,3.91959742347034,3.92384148101861,3.92808553856688,3.93232959611515,3.93657365366342,3.94081771121169,3.94506176875996,3.94930582630823,3.95354988385651,3.95779394140477,3.96203799895304,3.96628205650132,3.97052611404958,3.97477017159785,3.97901422914612,3.98325828669439,3.98750234424266,3.99174640179093,3.9959904593392,4.00023451688747,4.00447857443574,4.00872263198401,4.01296668953228,4.01721074708055,4.02145480462882,4.02569886217709,4.02994291972536,4.03418697727363,4.0384310348219,4.04267509237017,4.04691914991844,4.05116320746671],"text":["year: 1952.000<br />meanGDP: 3.715883<br />continent: Asia","year: 1952.696<br />meanGDP: 3.720127<br />continent: Asia","year: 1953.392<br />meanGDP: 3.724371<br />continent: Asia","year: 1954.089<br />meanGDP: 3.728615<br />continent: Asia","year: 1954.785<br />meanGDP: 3.732859<br />continent: Asia","year: 1955.481<br />meanGDP: 3.737103<br />continent: Asia","year: 1956.177<br />meanGDP: 3.741347<br />continent: Asia","year: 1956.873<br />meanGDP: 3.745591<br />continent: Asia","year: 1957.570<br />meanGDP: 3.749835<br />continent: Asia","year: 1958.266<br />meanGDP: 3.754079<br />continent: Asia","year: 1958.962<br />meanGDP: 3.758323<br />continent: Asia","year: 1959.658<br />meanGDP: 3.762567<br />continent: Asia","year: 1960.354<br />meanGDP: 3.766811<br />continent: Asia","year: 1961.051<br />meanGDP: 3.771055<br />continent: Asia","year: 1961.747<br />meanGDP: 3.775299<br />continent: Asia","year: 1962.443<br />meanGDP: 3.779544<br />continent: Asia","year: 1963.139<br />meanGDP: 3.783788<br />continent: Asia","year: 1963.835<br />meanGDP: 3.788032<br />continent: Asia","year: 1964.532<br />meanGDP: 3.792276<br />continent: Asia","year: 1965.228<br />meanGDP: 3.796520<br />continent: Asia","year: 1965.924<br />meanGDP: 3.800764<br />continent: Asia","year: 1966.620<br />meanGDP: 3.805008<br />continent: Asia","year: 1967.316<br />meanGDP: 3.809252<br />continent: Asia","year: 1968.013<br />meanGDP: 3.813496<br />continent: Asia","year: 1968.709<br />meanGDP: 3.817740<br />continent: Asia","year: 1969.405<br />meanGDP: 3.821984<br />continent: Asia","year: 1970.101<br />meanGDP: 3.826228<br />continent: Asia","year: 1970.797<br />meanGDP: 3.830472<br />continent: Asia","year: 1971.494<br />meanGDP: 3.834716<br />continent: Asia","year: 1972.190<br />meanGDP: 3.838960<br />continent: Asia","year: 1972.886<br />meanGDP: 3.843204<br />continent: Asia","year: 1973.582<br />meanGDP: 3.847448<br />continent: Asia","year: 1974.278<br />meanGDP: 3.851693<br />continent: Asia","year: 1974.975<br />meanGDP: 3.855937<br />continent: Asia","year: 1975.671<br />meanGDP: 3.860181<br />continent: Asia","year: 1976.367<br />meanGDP: 3.864425<br />continent: Asia","year: 1977.063<br />meanGDP: 3.868669<br />continent: Asia","year: 1977.759<br />meanGDP: 3.872913<br />continent: Asia","year: 1978.456<br />meanGDP: 3.877157<br />continent: Asia","year: 1979.152<br />meanGDP: 3.881401<br />continent: Asia","year: 1979.848<br />meanGDP: 3.885645<br />continent: Asia","year: 1980.544<br />meanGDP: 3.889889<br />continent: Asia","year: 1981.241<br />meanGDP: 3.894133<br />continent: Asia","year: 1981.937<br />meanGDP: 3.898377<br />continent: Asia","year: 1982.633<br />meanGDP: 3.902621<br />continent: Asia","year: 1983.329<br />meanGDP: 3.906865<br />continent: Asia","year: 1984.025<br />meanGDP: 3.911109<br />continent: Asia","year: 1984.722<br />meanGDP: 3.915353<br />continent: Asia","year: 1985.418<br />meanGDP: 3.919597<br />continent: Asia","year: 1986.114<br />meanGDP: 3.923841<br />continent: Asia","year: 1986.810<br />meanGDP: 3.928086<br />continent: Asia","year: 1987.506<br />meanGDP: 3.932330<br />continent: Asia","year: 1988.203<br />meanGDP: 3.936574<br />continent: Asia","year: 1988.899<br />meanGDP: 3.940818<br />continent: Asia","year: 1989.595<br />meanGDP: 3.945062<br />continent: Asia","year: 1990.291<br />meanGDP: 3.949306<br />continent: Asia","year: 1990.987<br />meanGDP: 3.953550<br />continent: Asia","year: 1991.684<br />meanGDP: 3.957794<br />continent: Asia","year: 1992.380<br />meanGDP: 3.962038<br />continent: Asia","year: 1993.076<br />meanGDP: 3.966282<br />continent: Asia","year: 1993.772<br />meanGDP: 3.970526<br />continent: Asia","year: 1994.468<br />meanGDP: 3.974770<br />continent: Asia","year: 1995.165<br />meanGDP: 3.979014<br />continent: Asia","year: 1995.861<br />meanGDP: 3.983258<br />continent: Asia","year: 1996.557<br />meanGDP: 3.987502<br />continent: Asia","year: 1997.253<br />meanGDP: 3.991746<br />continent: Asia","year: 1997.949<br />meanGDP: 3.995990<br />continent: Asia","year: 1998.646<br />meanGDP: 4.000235<br />continent: Asia","year: 1999.342<br />meanGDP: 4.004479<br />continent: Asia","year: 2000.038<br />meanGDP: 4.008723<br />continent: Asia","year: 2000.734<br />meanGDP: 4.012967<br />continent: Asia","year: 2001.430<br />meanGDP: 4.017211<br />continent: Asia","year: 2002.127<br />meanGDP: 4.021455<br />continent: Asia","year: 2002.823<br />meanGDP: 4.025699<br />continent: Asia","year: 2003.519<br />meanGDP: 4.029943<br />continent: Asia","year: 2004.215<br />meanGDP: 4.034187<br />continent: Asia","year: 2004.911<br />meanGDP: 4.038431<br />continent: Asia","year: 2005.608<br />meanGDP: 4.042675<br />continent: Asia","year: 2006.304<br />meanGDP: 4.046919<br />continent: Asia","year: 2007.000<br />meanGDP: 4.051163<br />continent: Asia"],"type":"scatter","mode":"lines","name":"Asia","line":{"width":1.88976377952756,"color":"rgba(117,112,179,1)","dash":"solid"},"hoveron":"points","legendgroup":"Asia","showlegend":false,"xaxis":"x","yaxis":"y2","hoverinfo":"text","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007],"y":[3.82024218936497,3.82785853228291,3.83547487520086,3.8430912181188,3.85070756103674,3.85832390395468,3.86594024687263,3.87355658979057,3.88117293270851,3.88878927562645,3.8964056185444,3.90402196146234,3.91163830438028,3.91925464729822,3.92687099021617,3.93448733313411,3.94210367605205,3.94972001897,3.95733636188794,3.96495270480588,3.97256904772382,3.98018539064177,3.98780173355971,3.99541807647765,4.0030344193956,4.01065076231354,4.01826710523148,4.02588344814942,4.03349979106737,4.04111613398531,4.04873247690325,4.0563488198212,4.06396516273914,4.07158150565708,4.07919784857502,4.08681419149297,4.09443053441091,4.10204687732885,4.10966322024679,4.11727956316474,4.12489590608268,4.13251224900062,4.14012859191856,4.14774493483651,4.15536127775445,4.16297762067239,4.17059396359033,4.17821030650828,4.18582664942622,4.19344299234416,4.20105933526211,4.20867567818005,4.21629202109799,4.22390836401593,4.23152470693388,4.23914104985182,4.24675739276976,4.2543737356877,4.26199007860565,4.26960642152359,4.27722276444153,4.28483910735947,4.29245545027742,4.30007179319536,4.3076881361133,4.31530447903124,4.32292082194919,4.33053716486713,4.33815350778507,4.34576985070301,4.35338619362096,4.3610025365389,4.36861887945684,4.37623522237479,4.38385156529273,4.39146790821067,4.39908425112861,4.40670059404656,4.4143169369645,4.42193327988244],"text":["year: 1952.000<br />meanGDP: 3.820242<br />continent: Europe","year: 1952.696<br />meanGDP: 3.827859<br />continent: Europe","year: 1953.392<br />meanGDP: 3.835475<br />continent: Europe","year: 1954.089<br />meanGDP: 3.843091<br />continent: Europe","year: 1954.785<br />meanGDP: 3.850708<br />continent: Europe","year: 1955.481<br />meanGDP: 3.858324<br />continent: Europe","year: 1956.177<br />meanGDP: 3.865940<br />continent: Europe","year: 1956.873<br />meanGDP: 3.873557<br />continent: Europe","year: 1957.570<br />meanGDP: 3.881173<br />continent: Europe","year: 1958.266<br />meanGDP: 3.888789<br />continent: Europe","year: 1958.962<br />meanGDP: 3.896406<br />continent: Europe","year: 1959.658<br />meanGDP: 3.904022<br />continent: Europe","year: 1960.354<br />meanGDP: 3.911638<br />continent: Europe","year: 1961.051<br />meanGDP: 3.919255<br />continent: Europe","year: 1961.747<br />meanGDP: 3.926871<br />continent: Europe","year: 1962.443<br />meanGDP: 3.934487<br />continent: Europe","year: 1963.139<br />meanGDP: 3.942104<br />continent: Europe","year: 1963.835<br />meanGDP: 3.949720<br />continent: Europe","year: 1964.532<br />meanGDP: 3.957336<br />continent: Europe","year: 1965.228<br />meanGDP: 3.964953<br />continent: Europe","year: 1965.924<br />meanGDP: 3.972569<br />continent: Europe","year: 1966.620<br />meanGDP: 3.980185<br />continent: Europe","year: 1967.316<br />meanGDP: 3.987802<br />continent: Europe","year: 1968.013<br />meanGDP: 3.995418<br />continent: Europe","year: 1968.709<br />meanGDP: 4.003034<br />continent: Europe","year: 1969.405<br />meanGDP: 4.010651<br />continent: Europe","year: 1970.101<br />meanGDP: 4.018267<br />continent: Europe","year: 1970.797<br />meanGDP: 4.025883<br />continent: Europe","year: 1971.494<br />meanGDP: 4.033500<br />continent: Europe","year: 1972.190<br />meanGDP: 4.041116<br />continent: Europe","year: 1972.886<br />meanGDP: 4.048732<br />continent: Europe","year: 1973.582<br />meanGDP: 4.056349<br />continent: Europe","year: 1974.278<br />meanGDP: 4.063965<br />continent: Europe","year: 1974.975<br />meanGDP: 4.071582<br />continent: Europe","year: 1975.671<br />meanGDP: 4.079198<br />continent: Europe","year: 1976.367<br />meanGDP: 4.086814<br />continent: Europe","year: 1977.063<br />meanGDP: 4.094431<br />continent: Europe","year: 1977.759<br />meanGDP: 4.102047<br />continent: Europe","year: 1978.456<br />meanGDP: 4.109663<br />continent: Europe","year: 1979.152<br />meanGDP: 4.117280<br />continent: Europe","year: 1979.848<br />meanGDP: 4.124896<br />continent: Europe","year: 1980.544<br />meanGDP: 4.132512<br />continent: Europe","year: 1981.241<br />meanGDP: 4.140129<br />continent: Europe","year: 1981.937<br />meanGDP: 4.147745<br />continent: Europe","year: 1982.633<br />meanGDP: 4.155361<br />continent: Europe","year: 1983.329<br />meanGDP: 4.162978<br />continent: Europe","year: 1984.025<br />meanGDP: 4.170594<br />continent: Europe","year: 1984.722<br />meanGDP: 4.178210<br />continent: Europe","year: 1985.418<br />meanGDP: 4.185827<br />continent: Europe","year: 1986.114<br />meanGDP: 4.193443<br />continent: Europe","year: 1986.810<br />meanGDP: 4.201059<br />continent: Europe","year: 1987.506<br />meanGDP: 4.208676<br />continent: Europe","year: 1988.203<br />meanGDP: 4.216292<br />continent: Europe","year: 1988.899<br />meanGDP: 4.223908<br />continent: Europe","year: 1989.595<br />meanGDP: 4.231525<br />continent: Europe","year: 1990.291<br />meanGDP: 4.239141<br />continent: Europe","year: 1990.987<br />meanGDP: 4.246757<br />continent: Europe","year: 1991.684<br />meanGDP: 4.254374<br />continent: Europe","year: 1992.380<br />meanGDP: 4.261990<br />continent: Europe","year: 1993.076<br />meanGDP: 4.269606<br />continent: Europe","year: 1993.772<br />meanGDP: 4.277223<br />continent: Europe","year: 1994.468<br />meanGDP: 4.284839<br />continent: Europe","year: 1995.165<br />meanGDP: 4.292455<br />continent: Europe","year: 1995.861<br />meanGDP: 4.300072<br />continent: Europe","year: 1996.557<br />meanGDP: 4.307688<br />continent: Europe","year: 1997.253<br />meanGDP: 4.315304<br />continent: Europe","year: 1997.949<br />meanGDP: 4.322921<br />continent: Europe","year: 1998.646<br />meanGDP: 4.330537<br />continent: Europe","year: 1999.342<br />meanGDP: 4.338154<br />continent: Europe","year: 2000.038<br />meanGDP: 4.345770<br />continent: Europe","year: 2000.734<br />meanGDP: 4.353386<br />continent: Europe","year: 2001.430<br />meanGDP: 4.361003<br />continent: Europe","year: 2002.127<br />meanGDP: 4.368619<br />continent: Europe","year: 2002.823<br />meanGDP: 4.376235<br />continent: Europe","year: 2003.519<br />meanGDP: 4.383852<br />continent: Europe","year: 2004.215<br />meanGDP: 4.391468<br />continent: Europe","year: 2004.911<br />meanGDP: 4.399084<br />continent: Europe","year: 2005.608<br />meanGDP: 4.406701<br />continent: Europe","year: 2006.304<br />meanGDP: 4.414317<br />continent: Europe","year: 2007.000<br />meanGDP: 4.421933<br />continent: Europe"],"type":"scatter","mode":"lines","name":"Europe","line":{"width":1.88976377952756,"color":"rgba(231,41,138,1)","dash":"solid"},"hoveron":"points","legendgroup":"Europe","showlegend":false,"xaxis":"x2","yaxis":"y2","hoverinfo":"text","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007,2007,2006.30379746835,2005.60759493671,2004.91139240506,2004.21518987342,2003.51898734177,2002.82278481013,2002.12658227848,2001.43037974684,2000.73417721519,2000.03797468354,1999.3417721519,1998.64556962025,1997.94936708861,1997.25316455696,1996.55696202532,1995.86075949367,1995.16455696203,1994.46835443038,1993.77215189873,1993.07594936709,1992.37974683544,1991.6835443038,1990.98734177215,1990.29113924051,1989.59493670886,1988.89873417722,1988.20253164557,1987.50632911392,1986.81012658228,1986.11392405063,1985.41772151899,1984.72151898734,1984.0253164557,1983.32911392405,1982.63291139241,1981.93670886076,1981.24050632911,1980.54430379747,1979.84810126582,1979.15189873418,1978.45569620253,1977.75949367089,1977.06329113924,1976.36708860759,1975.67088607595,1974.9746835443,1974.27848101266,1973.58227848101,1972.88607594937,1972.18987341772,1971.49367088608,1970.79746835443,1970.10126582278,1969.40506329114,1968.70886075949,1968.01265822785,1967.3164556962,1966.62025316456,1965.92405063291,1965.22784810127,1964.53164556962,1963.83544303797,1963.13924050633,1962.44303797468,1961.74683544304,1961.05063291139,1960.35443037975,1959.6582278481,1958.96202531646,1958.26582278481,1957.56962025316,1956.87341772152,1956.17721518987,1955.48101265823,1954.78481012658,1954.08860759494,1953.39240506329,1952.69620253165,1952,1952],"y":[3.16206385786372,3.1661896935172,3.17031438177107,3.17443785728947,3.17856005027128,3.1826808861282,3.18680028514375,3.19091816211377,3.19503442596916,3.19914897938246,3.20326171836059,3.20737253182713,3.21148130119866,3.21558789996119,3.21969219325459,3.22379403747488,3.22789327990676,3.23198975840148,3.23608330111829,3.2401737263509,3.24426084246419,3.24834444796959,3.25242433177106,3.25650027361634,3.26057204478982,3.2646394090839,3.26870212408392,3.27275994279726,3.27681261564996,3.28085989286206,3.28490152719804,3.28893727706848,3.29296690993625,3.29699020595503,3.30100696174169,3.30501699416037,3.30902014397623,3.3130162792252,3.31700529814451,3.3209871315187,3.32496174431862,3.32892913654426,3.33288934322479,3.33684243357565,3.34078850935963,3.34472770254078,3.34866017235396,3.35258610193502,3.35650569466707,3.36041917039647,3.36432676166032,3.36822871004806,3.3721252627952,3.37601666968168,3.37990318028151,3.38378504158726,3.38766249601361,3.39153577976817,3.39540512156653,3.39927074166097,3.40313285114751,3.40699165151474,3.41084733439776,3.41470008150287,3.41855006467083,3.42239744605037,3.42624237835681,3.43008500519412,3.43392546142242,3.43776387355571,3.44160036017741,3.44543503236395,3.4492679941084,3.45309934273821,3.45692916932249,3.46075755906542,3.46458459168344,3.46841034176487,3.47223487911083,3.47605826905719,3.49277200740808,3.4886461717546,3.48452148350073,3.48039800798233,3.47627581500052,3.4721549791436,3.46803558012805,3.46391770315803,3.45980143930264,3.45568688588934,3.45157414691122,3.44746333344467,3.44335456407314,3.43924796531061,3.43514367201721,3.43104182779692,3.42694258536504,3.42284610687032,3.41875256415351,3.4146621389209,3.41057502280761,3.40649141730221,3.40241153350074,3.39833559165546,3.39426382048198,3.3901964561879,3.38613374118788,3.38207592247454,3.37802324962184,3.37397597240975,3.36993433807377,3.36589858820333,3.36186895533555,3.35784565931677,3.35382890353011,3.34981887111143,3.34581572129557,3.3418195860466,3.3378305671273,3.3338487337531,3.32987412095319,3.32590672872755,3.32194652204702,3.31799343169615,3.31404735591217,3.31010816273102,3.30617569291785,3.30224976333679,3.29833017060474,3.29441669487534,3.29050910361148,3.28660715522375,3.28271060247661,3.27881919559012,3.2749326849903,3.27105082368455,3.26717336925819,3.26330008550364,3.25943074370527,3.25556512361083,3.25170301412429,3.24784421375707,3.24398853087404,3.24013578376893,3.23628580060098,3.23243841922143,3.22859348691499,3.22475086007769,3.22091040384939,3.2170719917161,3.21323550509439,3.20940083290785,3.20556787116341,3.20173652253359,3.19790669594931,3.19407830620639,3.19025127358837,3.18642552350693,3.18260098616097,3.17877759621462,3.16206385786372],"text":["year: 1952.000<br />meanGDP: 3.170421<br />continent: Africa","year: 1952.696<br />meanGDP: 3.174395<br />continent: Africa","year: 1953.392<br />meanGDP: 3.178370<br />continent: Africa","year: 1954.089<br />meanGDP: 3.182345<br />continent: Africa","year: 1954.785<br />meanGDP: 3.186319<br />continent: Africa","year: 1955.481<br />meanGDP: 3.190294<br />continent: Africa","year: 1956.177<br />meanGDP: 3.194268<br />continent: Africa","year: 1956.873<br />meanGDP: 3.198243<br />continent: Africa","year: 1957.570<br />meanGDP: 3.202218<br />continent: Africa","year: 1958.266<br />meanGDP: 3.206192<br />continent: Africa","year: 1958.962<br />meanGDP: 3.210167<br />continent: Africa","year: 1959.658<br />meanGDP: 3.214141<br />continent: Africa","year: 1960.354<br />meanGDP: 3.218116<br />continent: Africa","year: 1961.051<br />meanGDP: 3.222091<br />continent: Africa","year: 1961.747<br />meanGDP: 3.226065<br />continent: Africa","year: 1962.443<br />meanGDP: 3.230040<br />continent: Africa","year: 1963.139<br />meanGDP: 3.234015<br />continent: Africa","year: 1963.835<br />meanGDP: 3.237989<br />continent: Africa","year: 1964.532<br />meanGDP: 3.241964<br />continent: Africa","year: 1965.228<br />meanGDP: 3.245938<br />continent: Africa","year: 1965.924<br />meanGDP: 3.249913<br />continent: Africa","year: 1966.620<br />meanGDP: 3.253888<br />continent: Africa","year: 1967.316<br />meanGDP: 3.257862<br />continent: Africa","year: 1968.013<br />meanGDP: 3.261837<br />continent: Africa","year: 1968.709<br />meanGDP: 3.265811<br />continent: Africa","year: 1969.405<br />meanGDP: 3.269786<br />continent: Africa","year: 1970.101<br />meanGDP: 3.273761<br />continent: Africa","year: 1970.797<br />meanGDP: 3.277735<br />continent: Africa","year: 1971.494<br />meanGDP: 3.281710<br />continent: Africa","year: 1972.190<br />meanGDP: 3.285684<br />continent: Africa","year: 1972.886<br />meanGDP: 3.289659<br />continent: Africa","year: 1973.582<br />meanGDP: 3.293634<br />continent: Africa","year: 1974.278<br />meanGDP: 3.297608<br />continent: Africa","year: 1974.975<br />meanGDP: 3.301583<br />continent: Africa","year: 1975.671<br />meanGDP: 3.305558<br />continent: Africa","year: 1976.367<br />meanGDP: 3.309532<br />continent: Africa","year: 1977.063<br />meanGDP: 3.313507<br />continent: Africa","year: 1977.759<br />meanGDP: 3.317481<br />continent: Africa","year: 1978.456<br />meanGDP: 3.321456<br />continent: Africa","year: 1979.152<br />meanGDP: 3.325431<br />continent: Africa","year: 1979.848<br />meanGDP: 3.329405<br />continent: Africa","year: 1980.544<br />meanGDP: 3.333380<br />continent: Africa","year: 1981.241<br />meanGDP: 3.337354<br />continent: Africa","year: 1981.937<br />meanGDP: 3.341329<br />continent: Africa","year: 1982.633<br />meanGDP: 3.345304<br />continent: Africa","year: 1983.329<br />meanGDP: 3.349278<br />continent: Africa","year: 1984.025<br />meanGDP: 3.353253<br />continent: Africa","year: 1984.722<br />meanGDP: 3.357228<br />continent: Africa","year: 1985.418<br />meanGDP: 3.361202<br />continent: Africa","year: 1986.114<br />meanGDP: 3.365177<br />continent: Africa","year: 1986.810<br />meanGDP: 3.369151<br />continent: Africa","year: 1987.506<br />meanGDP: 3.373126<br />continent: Africa","year: 1988.203<br />meanGDP: 3.377101<br />continent: Africa","year: 1988.899<br />meanGDP: 3.381075<br />continent: Africa","year: 1989.595<br />meanGDP: 3.385050<br />continent: Africa","year: 1990.291<br />meanGDP: 3.389024<br />continent: Africa","year: 1990.987<br />meanGDP: 3.392999<br />continent: Africa","year: 1991.684<br />meanGDP: 3.396974<br />continent: Africa","year: 1992.380<br />meanGDP: 3.400948<br />continent: Africa","year: 1993.076<br />meanGDP: 3.404923<br />continent: Africa","year: 1993.772<br />meanGDP: 3.408897<br />continent: Africa","year: 1994.468<br />meanGDP: 3.412872<br />continent: Africa","year: 1995.165<br />meanGDP: 3.416847<br />continent: Africa","year: 1995.861<br />meanGDP: 3.420821<br />continent: Africa","year: 1996.557<br />meanGDP: 3.424796<br />continent: Africa","year: 1997.253<br />meanGDP: 3.428771<br />continent: Africa","year: 1997.949<br />meanGDP: 3.432745<br />continent: Africa","year: 1998.646<br />meanGDP: 3.436720<br />continent: Africa","year: 1999.342<br />meanGDP: 3.440694<br />continent: Africa","year: 2000.038<br />meanGDP: 3.444669<br />continent: Africa","year: 2000.734<br />meanGDP: 3.448644<br />continent: Africa","year: 2001.430<br />meanGDP: 3.452618<br />continent: Africa","year: 2002.127<br />meanGDP: 3.456593<br />continent: Africa","year: 2002.823<br />meanGDP: 3.460567<br />continent: Africa","year: 2003.519<br />meanGDP: 3.464542<br />continent: Africa","year: 2004.215<br />meanGDP: 3.468517<br />continent: Africa","year: 2004.911<br />meanGDP: 3.472491<br />continent: Africa","year: 2005.608<br />meanGDP: 3.476466<br />continent: Africa","year: 2006.304<br />meanGDP: 3.480441<br />continent: Africa","year: 2007.000<br />meanGDP: 3.484415<br />continent: Africa","year: 2007.000<br />meanGDP: 3.484415<br />continent: Africa","year: 2006.304<br />meanGDP: 3.480441<br />continent: Africa","year: 2005.608<br />meanGDP: 3.476466<br />continent: Africa","year: 2004.911<br />meanGDP: 3.472491<br />continent: Africa","year: 2004.215<br />meanGDP: 3.468517<br />continent: Africa","year: 2003.519<br />meanGDP: 3.464542<br />continent: Africa","year: 2002.823<br />meanGDP: 3.460567<br />continent: Africa","year: 2002.127<br />meanGDP: 3.456593<br />continent: Africa","year: 2001.430<br />meanGDP: 3.452618<br />continent: Africa","year: 2000.734<br />meanGDP: 3.448644<br />continent: Africa","year: 2000.038<br />meanGDP: 3.444669<br />continent: Africa","year: 1999.342<br />meanGDP: 3.440694<br />continent: Africa","year: 1998.646<br />meanGDP: 3.436720<br />continent: Africa","year: 1997.949<br />meanGDP: 3.432745<br />continent: Africa","year: 1997.253<br />meanGDP: 3.428771<br />continent: Africa","year: 1996.557<br />meanGDP: 3.424796<br />continent: Africa","year: 1995.861<br />meanGDP: 3.420821<br />continent: Africa","year: 1995.165<br />meanGDP: 3.416847<br />continent: Africa","year: 1994.468<br />meanGDP: 3.412872<br />continent: Africa","year: 1993.772<br />meanGDP: 3.408897<br />continent: Africa","year: 1993.076<br />meanGDP: 3.404923<br />continent: Africa","year: 1992.380<br />meanGDP: 3.400948<br />continent: Africa","year: 1991.684<br />meanGDP: 3.396974<br />continent: Africa","year: 1990.987<br />meanGDP: 3.392999<br />continent: Africa","year: 1990.291<br />meanGDP: 3.389024<br />continent: Africa","year: 1989.595<br />meanGDP: 3.385050<br />continent: Africa","year: 1988.899<br />meanGDP: 3.381075<br />continent: Africa","year: 1988.203<br />meanGDP: 3.377101<br />continent: Africa","year: 1987.506<br />meanGDP: 3.373126<br />continent: Africa","year: 1986.810<br />meanGDP: 3.369151<br />continent: Africa","year: 1986.114<br />meanGDP: 3.365177<br />continent: Africa","year: 1985.418<br />meanGDP: 3.361202<br />continent: Africa","year: 1984.722<br />meanGDP: 3.357228<br />continent: Africa","year: 1984.025<br />meanGDP: 3.353253<br />continent: Africa","year: 1983.329<br />meanGDP: 3.349278<br />continent: Africa","year: 1982.633<br />meanGDP: 3.345304<br />continent: Africa","year: 1981.937<br />meanGDP: 3.341329<br />continent: Africa","year: 1981.241<br />meanGDP: 3.337354<br />continent: Africa","year: 1980.544<br />meanGDP: 3.333380<br />continent: Africa","year: 1979.848<br />meanGDP: 3.329405<br />continent: Africa","year: 1979.152<br />meanGDP: 3.325431<br />continent: Africa","year: 1978.456<br />meanGDP: 3.321456<br />continent: Africa","year: 1977.759<br />meanGDP: 3.317481<br />continent: Africa","year: 1977.063<br />meanGDP: 3.313507<br />continent: Africa","year: 1976.367<br />meanGDP: 3.309532<br />continent: Africa","year: 1975.671<br />meanGDP: 3.305558<br />continent: Africa","year: 1974.975<br />meanGDP: 3.301583<br />continent: Africa","year: 1974.278<br />meanGDP: 3.297608<br />continent: Africa","year: 1973.582<br />meanGDP: 3.293634<br />continent: Africa","year: 1972.886<br />meanGDP: 3.289659<br />continent: Africa","year: 1972.190<br />meanGDP: 3.285684<br />continent: Africa","year: 1971.494<br />meanGDP: 3.281710<br />continent: Africa","year: 1970.797<br />meanGDP: 3.277735<br />continent: Africa","year: 1970.101<br />meanGDP: 3.273761<br />continent: Africa","year: 1969.405<br />meanGDP: 3.269786<br />continent: Africa","year: 1968.709<br />meanGDP: 3.265811<br />continent: Africa","year: 1968.013<br />meanGDP: 3.261837<br />continent: Africa","year: 1967.316<br />meanGDP: 3.257862<br />continent: Africa","year: 1966.620<br />meanGDP: 3.253888<br />continent: Africa","year: 1965.924<br />meanGDP: 3.249913<br />continent: Africa","year: 1965.228<br />meanGDP: 3.245938<br />continent: Africa","year: 1964.532<br />meanGDP: 3.241964<br />continent: Africa","year: 1963.835<br />meanGDP: 3.237989<br />continent: Africa","year: 1963.139<br />meanGDP: 3.234015<br />continent: Africa","year: 1962.443<br />meanGDP: 3.230040<br />continent: Africa","year: 1961.747<br />meanGDP: 3.226065<br />continent: Africa","year: 1961.051<br />meanGDP: 3.222091<br />continent: Africa","year: 1960.354<br />meanGDP: 3.218116<br />continent: Africa","year: 1959.658<br />meanGDP: 3.214141<br />continent: Africa","year: 1958.962<br />meanGDP: 3.210167<br />continent: Africa","year: 1958.266<br />meanGDP: 3.206192<br />continent: Africa","year: 1957.570<br />meanGDP: 3.202218<br />continent: Africa","year: 1956.873<br />meanGDP: 3.198243<br />continent: Africa","year: 1956.177<br />meanGDP: 3.194268<br />continent: Africa","year: 1955.481<br />meanGDP: 3.190294<br />continent: Africa","year: 1954.785<br />meanGDP: 3.186319<br />continent: Africa","year: 1954.089<br />meanGDP: 3.182345<br />continent: Africa","year: 1953.392<br />meanGDP: 3.178370<br />continent: Africa","year: 1952.696<br />meanGDP: 3.174395<br />continent: Africa","year: 1952.000<br />meanGDP: 3.170421<br />continent: Africa","year: 1952.000<br />meanGDP: 3.170421<br />continent: Africa"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(153,153,153,0.4)","hoveron":"points","hoverinfo":"x+y","name":"Africa","legendgroup":"Africa","showlegend":false,"xaxis":"x","yaxis":"y","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007,2007,2006.30379746835,2005.60759493671,2004.91139240506,2004.21518987342,2003.51898734177,2002.82278481013,2002.12658227848,2001.43037974684,2000.73417721519,2000.03797468354,1999.3417721519,1998.64556962025,1997.94936708861,1997.25316455696,1996.55696202532,1995.86075949367,1995.16455696203,1994.46835443038,1993.77215189873,1993.07594936709,1992.37974683544,1991.6835443038,1990.98734177215,1990.29113924051,1989.59493670886,1988.89873417722,1988.20253164557,1987.50632911392,1986.81012658228,1986.11392405063,1985.41772151899,1984.72151898734,1984.0253164557,1983.32911392405,1982.63291139241,1981.93670886076,1981.24050632911,1980.54430379747,1979.84810126582,1979.15189873418,1978.45569620253,1977.75949367089,1977.06329113924,1976.36708860759,1975.67088607595,1974.9746835443,1974.27848101266,1973.58227848101,1972.88607594937,1972.18987341772,1971.49367088608,1970.79746835443,1970.10126582278,1969.40506329114,1968.70886075949,1968.01265822785,1967.3164556962,1966.62025316456,1965.92405063291,1965.22784810127,1964.53164556962,1963.83544303797,1963.13924050633,1962.44303797468,1961.74683544304,1961.05063291139,1960.35443037975,1959.6582278481,1958.96202531646,1958.26582278481,1957.56962025316,1956.87341772152,1956.17721518987,1955.48101265823,1954.78481012658,1954.08860759494,1953.39240506329,1952.69620253165,1952,1952],"y":[3.63278263997522,3.63788343236695,3.64298353587913,3.64808291128517,3.65318151667767,3.65827930727503,3.66337623521683,3.66847224934792,3.67356729499191,3.67866131371502,3.68375424308156,3.6888460164031,3.69393656248412,3.69902580536775,3.7041136640862,3.70920005242204,3.71428487868758,3.71936804553139,3.72444944978317,3.72952898234949,3.73460652817582,3.73968196629179,3.74475516995887,3.74982600694127,3.75489433992196,3.75996002708585,3.76502292289121,3.77008287904781,3.77513974571561,3.78019337293088,3.78524361225755,3.79029031864936,3.79533335249492,3.80037258180215,3.80540788446322,3.81043915052643,3.81546628438982,3.82048920682437,3.82550785673342,3.83052219256117,3.83553219327669,3.84053785887998,3.84553921040196,3.85053628939845,3.85552915696609,3.86051789233393,3.8655025911039,3.8704833632277,3.87546033081318,3.8804336258524,3.88540338795677,3.89036976217253,3.89533289693578,3.90029294221021,3.90525004783589,3.91020436210304,3.91515603055338,3.92010519500202,3.92505199276598,3.92999655608105,3.93493901168575,3.93987948055047,3.94481807772973,3.94975491231695,3.95469008748246,3.95962370057764,3.96455584329023,3.96948660183765,3.97441605718766,3.97934428529716,3.98427135736166,3.98919734006958,3.99412229585662,3.99904628315658,4.00396935664581,4.00889156747949,4.01381296351803,4.01873358954302,4.02365348746188,4.02857269650118,4.03860734592768,4.03350655353594,4.02840645002377,4.02330707461772,4.01820846922523,4.01311067862787,4.00801375068607,4.00291773655498,3.99782269091099,3.99272867218788,3.98763574282134,3.9825439694998,3.97745342341877,3.97236418053515,3.9672763218167,3.96218993348085,3.95710510721532,3.9520219403715,3.94694053611973,3.94186100355341,3.93678345772708,3.93170801961111,3.92663481594403,3.92156397896163,3.91649564598094,3.91142995881705,3.90636706301169,3.90130710685508,3.89625024018729,3.89119661297202,3.88614637364535,3.88109966725353,3.87605663340798,3.87101740410075,3.86598210143968,3.86095083537647,3.85592370151308,3.85090077907853,3.84588212916948,3.84086779334173,3.83585779262621,3.83085212702292,3.82585077550093,3.82085369650446,3.81586082893681,3.81087209356897,3.805887394799,3.8009066226752,3.79592965508972,3.7909563600505,3.78598659794613,3.78102022373037,3.77605708896712,3.77109704369269,3.76613993806701,3.76118562379986,3.75623395534951,3.75128479090088,3.74633799313692,3.74139342982185,3.73645097421715,3.73151050535243,3.72657190817317,3.72163507358595,3.71669989842044,3.71176628532526,3.70683414261267,3.70190338406525,3.69697392871524,3.69204570060574,3.68711862854124,3.68219264583332,3.67726769004628,3.67234370274632,3.66742062925709,3.66249841842341,3.65757702238487,3.65265639635988,3.64773649844101,3.64281728940171,3.63278263997522],"text":["year: 1952.000<br />meanGDP: 3.637800<br />continent: Americas","year: 1952.696<br />meanGDP: 3.642810<br />continent: Americas","year: 1953.392<br />meanGDP: 3.647820<br />continent: Americas","year: 1954.089<br />meanGDP: 3.652830<br />continent: Americas","year: 1954.785<br />meanGDP: 3.657840<br />continent: Americas","year: 1955.481<br />meanGDP: 3.662850<br />continent: Americas","year: 1956.177<br />meanGDP: 3.667860<br />continent: Americas","year: 1956.873<br />meanGDP: 3.672870<br />continent: Americas","year: 1957.570<br />meanGDP: 3.677880<br />continent: Americas","year: 1958.266<br />meanGDP: 3.682890<br />continent: Americas","year: 1958.962<br />meanGDP: 3.687900<br />continent: Americas","year: 1959.658<br />meanGDP: 3.692910<br />continent: Americas","year: 1960.354<br />meanGDP: 3.697920<br />continent: Americas","year: 1961.051<br />meanGDP: 3.702930<br />continent: Americas","year: 1961.747<br />meanGDP: 3.707940<br />continent: Americas","year: 1962.443<br />meanGDP: 3.712950<br />continent: Americas","year: 1963.139<br />meanGDP: 3.717960<br />continent: Americas","year: 1963.835<br />meanGDP: 3.722970<br />continent: Americas","year: 1964.532<br />meanGDP: 3.727980<br />continent: Americas","year: 1965.228<br />meanGDP: 3.732990<br />continent: Americas","year: 1965.924<br />meanGDP: 3.738000<br />continent: Americas","year: 1966.620<br />meanGDP: 3.743010<br />continent: Americas","year: 1967.316<br />meanGDP: 3.748020<br />continent: Americas","year: 1968.013<br />meanGDP: 3.753030<br />continent: Americas","year: 1968.709<br />meanGDP: 3.758040<br />continent: Americas","year: 1969.405<br />meanGDP: 3.763050<br />continent: Americas","year: 1970.101<br />meanGDP: 3.768060<br />continent: Americas","year: 1970.797<br />meanGDP: 3.773070<br />continent: Americas","year: 1971.494<br />meanGDP: 3.778080<br />continent: Americas","year: 1972.190<br />meanGDP: 3.783090<br />continent: Americas","year: 1972.886<br />meanGDP: 3.788100<br />continent: Americas","year: 1973.582<br />meanGDP: 3.793110<br />continent: Americas","year: 1974.278<br />meanGDP: 3.798120<br />continent: Americas","year: 1974.975<br />meanGDP: 3.803130<br />continent: Americas","year: 1975.671<br />meanGDP: 3.808140<br />continent: Americas","year: 1976.367<br />meanGDP: 3.813150<br />continent: Americas","year: 1977.063<br />meanGDP: 3.818160<br />continent: Americas","year: 1977.759<br />meanGDP: 3.823170<br />continent: Americas","year: 1978.456<br />meanGDP: 3.828180<br />continent: Americas","year: 1979.152<br />meanGDP: 3.833190<br />continent: Americas","year: 1979.848<br />meanGDP: 3.838200<br />continent: Americas","year: 1980.544<br />meanGDP: 3.843210<br />continent: Americas","year: 1981.241<br />meanGDP: 3.848220<br />continent: Americas","year: 1981.937<br />meanGDP: 3.853230<br />continent: Americas","year: 1982.633<br />meanGDP: 3.858240<br />continent: Americas","year: 1983.329<br />meanGDP: 3.863250<br />continent: Americas","year: 1984.025<br />meanGDP: 3.868260<br />continent: Americas","year: 1984.722<br />meanGDP: 3.873270<br />continent: Americas","year: 1985.418<br />meanGDP: 3.878280<br />continent: Americas","year: 1986.114<br />meanGDP: 3.883290<br />continent: Americas","year: 1986.810<br />meanGDP: 3.888300<br />continent: Americas","year: 1987.506<br />meanGDP: 3.893310<br />continent: Americas","year: 1988.203<br />meanGDP: 3.898320<br />continent: Americas","year: 1988.899<br />meanGDP: 3.903330<br />continent: Americas","year: 1989.595<br />meanGDP: 3.908340<br />continent: Americas","year: 1990.291<br />meanGDP: 3.913350<br />continent: Americas","year: 1990.987<br />meanGDP: 3.918360<br />continent: Americas","year: 1991.684<br />meanGDP: 3.923370<br />continent: Americas","year: 1992.380<br />meanGDP: 3.928380<br />continent: Americas","year: 1993.076<br />meanGDP: 3.933390<br />continent: Americas","year: 1993.772<br />meanGDP: 3.938400<br />continent: Americas","year: 1994.468<br />meanGDP: 3.943410<br />continent: Americas","year: 1995.165<br />meanGDP: 3.948420<br />continent: Americas","year: 1995.861<br />meanGDP: 3.953430<br />continent: Americas","year: 1996.557<br />meanGDP: 3.958440<br />continent: Americas","year: 1997.253<br />meanGDP: 3.963450<br />continent: Americas","year: 1997.949<br />meanGDP: 3.968460<br />continent: Americas","year: 1998.646<br />meanGDP: 3.973470<br />continent: Americas","year: 1999.342<br />meanGDP: 3.978480<br />continent: Americas","year: 2000.038<br />meanGDP: 3.983490<br />continent: Americas","year: 2000.734<br />meanGDP: 3.988500<br />continent: Americas","year: 2001.430<br />meanGDP: 3.993510<br />continent: Americas","year: 2002.127<br />meanGDP: 3.998520<br />continent: Americas","year: 2002.823<br />meanGDP: 4.003530<br />continent: Americas","year: 2003.519<br />meanGDP: 4.008540<br />continent: Americas","year: 2004.215<br />meanGDP: 4.013550<br />continent: Americas","year: 2004.911<br />meanGDP: 4.018560<br />continent: Americas","year: 2005.608<br />meanGDP: 4.023570<br />continent: Americas","year: 2006.304<br />meanGDP: 4.028580<br />continent: Americas","year: 2007.000<br />meanGDP: 4.033590<br />continent: Americas","year: 2007.000<br />meanGDP: 4.033590<br />continent: Americas","year: 2006.304<br />meanGDP: 4.028580<br />continent: Americas","year: 2005.608<br />meanGDP: 4.023570<br />continent: Americas","year: 2004.911<br />meanGDP: 4.018560<br />continent: Americas","year: 2004.215<br />meanGDP: 4.013550<br />continent: Americas","year: 2003.519<br />meanGDP: 4.008540<br />continent: Americas","year: 2002.823<br />meanGDP: 4.003530<br />continent: Americas","year: 2002.127<br />meanGDP: 3.998520<br />continent: Americas","year: 2001.430<br />meanGDP: 3.993510<br />continent: Americas","year: 2000.734<br />meanGDP: 3.988500<br />continent: Americas","year: 2000.038<br />meanGDP: 3.983490<br />continent: Americas","year: 1999.342<br />meanGDP: 3.978480<br />continent: Americas","year: 1998.646<br />meanGDP: 3.973470<br />continent: Americas","year: 1997.949<br />meanGDP: 3.968460<br />continent: Americas","year: 1997.253<br />meanGDP: 3.963450<br />continent: Americas","year: 1996.557<br />meanGDP: 3.958440<br />continent: Americas","year: 1995.861<br />meanGDP: 3.953430<br />continent: Americas","year: 1995.165<br />meanGDP: 3.948420<br />continent: Americas","year: 1994.468<br />meanGDP: 3.943410<br />continent: Americas","year: 1993.772<br />meanGDP: 3.938400<br />continent: Americas","year: 1993.076<br />meanGDP: 3.933390<br />continent: Americas","year: 1992.380<br />meanGDP: 3.928380<br />continent: Americas","year: 1991.684<br />meanGDP: 3.923370<br />continent: Americas","year: 1990.987<br />meanGDP: 3.918360<br />continent: Americas","year: 1990.291<br />meanGDP: 3.913350<br />continent: Americas","year: 1989.595<br />meanGDP: 3.908340<br />continent: Americas","year: 1988.899<br />meanGDP: 3.903330<br />continent: Americas","year: 1988.203<br />meanGDP: 3.898320<br />continent: Americas","year: 1987.506<br />meanGDP: 3.893310<br />continent: Americas","year: 1986.810<br />meanGDP: 3.888300<br />continent: Americas","year: 1986.114<br />meanGDP: 3.883290<br />continent: Americas","year: 1985.418<br />meanGDP: 3.878280<br />continent: Americas","year: 1984.722<br />meanGDP: 3.873270<br />continent: Americas","year: 1984.025<br />meanGDP: 3.868260<br />continent: Americas","year: 1983.329<br />meanGDP: 3.863250<br />continent: Americas","year: 1982.633<br />meanGDP: 3.858240<br />continent: Americas","year: 1981.937<br />meanGDP: 3.853230<br />continent: Americas","year: 1981.241<br />meanGDP: 3.848220<br />continent: Americas","year: 1980.544<br />meanGDP: 3.843210<br />continent: Americas","year: 1979.848<br />meanGDP: 3.838200<br />continent: Americas","year: 1979.152<br />meanGDP: 3.833190<br />continent: Americas","year: 1978.456<br />meanGDP: 3.828180<br />continent: Americas","year: 1977.759<br />meanGDP: 3.823170<br />continent: Americas","year: 1977.063<br />meanGDP: 3.818160<br />continent: Americas","year: 1976.367<br />meanGDP: 3.813150<br />continent: Americas","year: 1975.671<br />meanGDP: 3.808140<br />continent: Americas","year: 1974.975<br />meanGDP: 3.803130<br />continent: Americas","year: 1974.278<br />meanGDP: 3.798120<br />continent: Americas","year: 1973.582<br />meanGDP: 3.793110<br />continent: Americas","year: 1972.886<br />meanGDP: 3.788100<br />continent: Americas","year: 1972.190<br />meanGDP: 3.783090<br />continent: Americas","year: 1971.494<br />meanGDP: 3.778080<br />continent: Americas","year: 1970.797<br />meanGDP: 3.773070<br />continent: Americas","year: 1970.101<br />meanGDP: 3.768060<br />continent: Americas","year: 1969.405<br />meanGDP: 3.763050<br />continent: Americas","year: 1968.709<br />meanGDP: 3.758040<br />continent: Americas","year: 1968.013<br />meanGDP: 3.753030<br />continent: Americas","year: 1967.316<br />meanGDP: 3.748020<br />continent: Americas","year: 1966.620<br />meanGDP: 3.743010<br />continent: Americas","year: 1965.924<br />meanGDP: 3.738000<br />continent: Americas","year: 1965.228<br />meanGDP: 3.732990<br />continent: Americas","year: 1964.532<br />meanGDP: 3.727980<br />continent: Americas","year: 1963.835<br />meanGDP: 3.722970<br />continent: Americas","year: 1963.139<br />meanGDP: 3.717960<br />continent: Americas","year: 1962.443<br />meanGDP: 3.712950<br />continent: Americas","year: 1961.747<br />meanGDP: 3.707940<br />continent: Americas","year: 1961.051<br />meanGDP: 3.702930<br />continent: Americas","year: 1960.354<br />meanGDP: 3.697920<br />continent: Americas","year: 1959.658<br />meanGDP: 3.692910<br />continent: Americas","year: 1958.962<br />meanGDP: 3.687900<br />continent: Americas","year: 1958.266<br />meanGDP: 3.682890<br />continent: Americas","year: 1957.570<br />meanGDP: 3.677880<br />continent: Americas","year: 1956.873<br />meanGDP: 3.672870<br />continent: Americas","year: 1956.177<br />meanGDP: 3.667860<br />continent: Americas","year: 1955.481<br />meanGDP: 3.662850<br />continent: Americas","year: 1954.785<br />meanGDP: 3.657840<br />continent: Americas","year: 1954.089<br />meanGDP: 3.652830<br />continent: Americas","year: 1953.392<br />meanGDP: 3.647820<br />continent: Americas","year: 1952.696<br />meanGDP: 3.642810<br />continent: Americas","year: 1952.000<br />meanGDP: 3.637800<br />continent: Americas","year: 1952.000<br />meanGDP: 3.637800<br />continent: Americas"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(153,153,153,0.4)","hoveron":"points","hoverinfo":"x+y","name":"Americas","legendgroup":"Americas","showlegend":false,"xaxis":"x2","yaxis":"y","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007,2007,2006.30379746835,2005.60759493671,2004.91139240506,2004.21518987342,2003.51898734177,2002.82278481013,2002.12658227848,2001.43037974684,2000.73417721519,2000.03797468354,1999.3417721519,1998.64556962025,1997.94936708861,1997.25316455696,1996.55696202532,1995.86075949367,1995.16455696203,1994.46835443038,1993.77215189873,1993.07594936709,1992.37974683544,1991.6835443038,1990.98734177215,1990.29113924051,1989.59493670886,1988.89873417722,1988.20253164557,1987.50632911392,1986.81012658228,1986.11392405063,1985.41772151899,1984.72151898734,1984.0253164557,1983.32911392405,1982.63291139241,1981.93670886076,1981.24050632911,1980.54430379747,1979.84810126582,1979.15189873418,1978.45569620253,1977.75949367089,1977.06329113924,1976.36708860759,1975.67088607595,1974.9746835443,1974.27848101266,1973.58227848101,1972.88607594937,1972.18987341772,1971.49367088608,1970.79746835443,1970.10126582278,1969.40506329114,1968.70886075949,1968.01265822785,1967.3164556962,1966.62025316456,1965.92405063291,1965.22784810127,1964.53164556962,1963.83544303797,1963.13924050633,1962.44303797468,1961.74683544304,1961.05063291139,1960.35443037975,1959.6582278481,1958.96202531646,1958.26582278481,1957.56962025316,1956.87341772152,1956.17721518987,1955.48101265823,1954.78481012658,1954.08860759494,1953.39240506329,1952.69620253165,1952,1952],"y":[3.70961130812665,3.71396884978966,3.71832553039481,3.72268130091124,3.72703610895721,3.73138989855846,3.73574260989232,3.74009417901795,3.74444453759317,3.74879361257929,3.75314132593545,3.75748759430514,3.76183232869826,3.7661754341733,3.77051680952543,3.77485634698807,3.7791939319571,3.78352944274916,3.78786275040756,3.79219371857212,3.79652220343163,3.80084805378041,3.80517111120296,3.80949121041253,3.8138081797711,3.81812184201839,3.82243201523602,3.82673851407014,3.83104115122974,3.83533973926924,3.83963409265249,3.84392403008044,3.84820937704726,3.85248996857082,3.85676565202371,3.86103628997293,3.86530176292181,3.86956197183887,3.87381684035696,3.87806631653383,3.8823103740821,3.88654901300177,3.89078225958022,3.8950101657597,3.89923280790736,3.90345028505468,3.90766271669833,3.91187024027131,3.91607300840103,3.92027118606962,3.92446494778291,3.92865447483995,3.93283995277689,3.9370215690393,3.94119951091822,3.94537396376747,3.94954510950543,3.95371312539241,3.9578781830664,3.96204044781416,3.96620007805119,3.97035722498317,3.9745120324213,3.97866463672579,3.98281516685329,3.9869637444872,3.99111048423161,3.99525549385311,3.99939887455652,4.00354072128338,4.00768112302376,4.01182016313418,4.01595791965549,4.02009446562641,4.02422986938908,4.02836419488437,4.03249750193494,4.03662984651505,4.04076128100645,4.04489185443997,4.05743456049345,4.05307701883044,4.0487203382253,4.04436456770886,4.04000975966289,4.03565597006165,4.03130325872778,4.02695168960215,4.02260133102693,4.01825225604081,4.01390454268465,4.00955827431496,4.00521353992184,4.0008704344468,3.99652905909467,3.99218952163203,3.987851936663,3.98351642587094,3.97918311821254,3.97485215004798,3.97052366518847,3.96619781483969,3.96187475741714,3.95755465820758,3.95323768884899,3.94892402660171,3.94461385338409,3.94030735454996,3.93600471739036,3.93170612935086,3.92741177596761,3.92312183853966,3.91883649157284,3.91455590004928,3.91028021659639,3.90600957864718,3.90174410569829,3.89748389678123,3.89322902826314,3.88897955208627,3.884735494538,3.88049685561833,3.87626360903988,3.8720357028604,3.86781306071275,3.86359558356543,3.85938315192177,3.85517562834879,3.85097286021907,3.84677468255048,3.8425809208372,3.83839139378015,3.83420591584321,3.8300242995808,3.82584635770188,3.82167190485263,3.81750075911467,3.81333274322769,3.8091676855537,3.80500542080595,3.80084579056892,3.79668864363694,3.7925338361988,3.78838123189432,3.78423070176681,3.78008212413291,3.77593538438849,3.771790374767,3.76764699406358,3.76350514733673,3.75936474559635,3.75522570548593,3.75108794896461,3.7469514029937,3.74281599923103,3.73868167373573,3.73454836668517,3.73041602210506,3.72628458761366,3.72215401418013,3.70961130812665],"text":["year: 1952.000<br />meanGDP: 3.715883<br />continent: Asia","year: 1952.696<br />meanGDP: 3.720127<br />continent: Asia","year: 1953.392<br />meanGDP: 3.724371<br />continent: Asia","year: 1954.089<br />meanGDP: 3.728615<br />continent: Asia","year: 1954.785<br />meanGDP: 3.732859<br />continent: Asia","year: 1955.481<br />meanGDP: 3.737103<br />continent: Asia","year: 1956.177<br />meanGDP: 3.741347<br />continent: Asia","year: 1956.873<br />meanGDP: 3.745591<br />continent: Asia","year: 1957.570<br />meanGDP: 3.749835<br />continent: Asia","year: 1958.266<br />meanGDP: 3.754079<br />continent: Asia","year: 1958.962<br />meanGDP: 3.758323<br />continent: Asia","year: 1959.658<br />meanGDP: 3.762567<br />continent: Asia","year: 1960.354<br />meanGDP: 3.766811<br />continent: Asia","year: 1961.051<br />meanGDP: 3.771055<br />continent: Asia","year: 1961.747<br />meanGDP: 3.775299<br />continent: Asia","year: 1962.443<br />meanGDP: 3.779544<br />continent: Asia","year: 1963.139<br />meanGDP: 3.783788<br />continent: Asia","year: 1963.835<br />meanGDP: 3.788032<br />continent: Asia","year: 1964.532<br />meanGDP: 3.792276<br />continent: Asia","year: 1965.228<br />meanGDP: 3.796520<br />continent: Asia","year: 1965.924<br />meanGDP: 3.800764<br />continent: Asia","year: 1966.620<br />meanGDP: 3.805008<br />continent: Asia","year: 1967.316<br />meanGDP: 3.809252<br />continent: Asia","year: 1968.013<br />meanGDP: 3.813496<br />continent: Asia","year: 1968.709<br />meanGDP: 3.817740<br />continent: Asia","year: 1969.405<br />meanGDP: 3.821984<br />continent: Asia","year: 1970.101<br />meanGDP: 3.826228<br />continent: Asia","year: 1970.797<br />meanGDP: 3.830472<br />continent: Asia","year: 1971.494<br />meanGDP: 3.834716<br />continent: Asia","year: 1972.190<br />meanGDP: 3.838960<br />continent: Asia","year: 1972.886<br />meanGDP: 3.843204<br />continent: Asia","year: 1973.582<br />meanGDP: 3.847448<br />continent: Asia","year: 1974.278<br />meanGDP: 3.851693<br />continent: Asia","year: 1974.975<br />meanGDP: 3.855937<br />continent: Asia","year: 1975.671<br />meanGDP: 3.860181<br />continent: Asia","year: 1976.367<br />meanGDP: 3.864425<br />continent: Asia","year: 1977.063<br />meanGDP: 3.868669<br />continent: Asia","year: 1977.759<br />meanGDP: 3.872913<br />continent: Asia","year: 1978.456<br />meanGDP: 3.877157<br />continent: Asia","year: 1979.152<br />meanGDP: 3.881401<br />continent: Asia","year: 1979.848<br />meanGDP: 3.885645<br />continent: Asia","year: 1980.544<br />meanGDP: 3.889889<br />continent: Asia","year: 1981.241<br />meanGDP: 3.894133<br />continent: Asia","year: 1981.937<br />meanGDP: 3.898377<br />continent: Asia","year: 1982.633<br />meanGDP: 3.902621<br />continent: Asia","year: 1983.329<br />meanGDP: 3.906865<br />continent: Asia","year: 1984.025<br />meanGDP: 3.911109<br />continent: Asia","year: 1984.722<br />meanGDP: 3.915353<br />continent: Asia","year: 1985.418<br />meanGDP: 3.919597<br />continent: Asia","year: 1986.114<br />meanGDP: 3.923841<br />continent: Asia","year: 1986.810<br />meanGDP: 3.928086<br />continent: Asia","year: 1987.506<br />meanGDP: 3.932330<br />continent: Asia","year: 1988.203<br />meanGDP: 3.936574<br />continent: Asia","year: 1988.899<br />meanGDP: 3.940818<br />continent: Asia","year: 1989.595<br />meanGDP: 3.945062<br />continent: Asia","year: 1990.291<br />meanGDP: 3.949306<br />continent: Asia","year: 1990.987<br />meanGDP: 3.953550<br />continent: Asia","year: 1991.684<br />meanGDP: 3.957794<br />continent: Asia","year: 1992.380<br />meanGDP: 3.962038<br />continent: Asia","year: 1993.076<br />meanGDP: 3.966282<br />continent: Asia","year: 1993.772<br />meanGDP: 3.970526<br />continent: Asia","year: 1994.468<br />meanGDP: 3.974770<br />continent: Asia","year: 1995.165<br />meanGDP: 3.979014<br />continent: Asia","year: 1995.861<br />meanGDP: 3.983258<br />continent: Asia","year: 1996.557<br />meanGDP: 3.987502<br />continent: Asia","year: 1997.253<br />meanGDP: 3.991746<br />continent: Asia","year: 1997.949<br />meanGDP: 3.995990<br />continent: Asia","year: 1998.646<br />meanGDP: 4.000235<br />continent: Asia","year: 1999.342<br />meanGDP: 4.004479<br />continent: Asia","year: 2000.038<br />meanGDP: 4.008723<br />continent: Asia","year: 2000.734<br />meanGDP: 4.012967<br />continent: Asia","year: 2001.430<br />meanGDP: 4.017211<br />continent: Asia","year: 2002.127<br />meanGDP: 4.021455<br />continent: Asia","year: 2002.823<br />meanGDP: 4.025699<br />continent: Asia","year: 2003.519<br />meanGDP: 4.029943<br />continent: Asia","year: 2004.215<br />meanGDP: 4.034187<br />continent: Asia","year: 2004.911<br />meanGDP: 4.038431<br />continent: Asia","year: 2005.608<br />meanGDP: 4.042675<br />continent: Asia","year: 2006.304<br />meanGDP: 4.046919<br />continent: Asia","year: 2007.000<br />meanGDP: 4.051163<br />continent: Asia","year: 2007.000<br />meanGDP: 4.051163<br />continent: Asia","year: 2006.304<br />meanGDP: 4.046919<br />continent: Asia","year: 2005.608<br />meanGDP: 4.042675<br />continent: Asia","year: 2004.911<br />meanGDP: 4.038431<br />continent: Asia","year: 2004.215<br />meanGDP: 4.034187<br />continent: Asia","year: 2003.519<br />meanGDP: 4.029943<br />continent: Asia","year: 2002.823<br />meanGDP: 4.025699<br />continent: Asia","year: 2002.127<br />meanGDP: 4.021455<br />continent: Asia","year: 2001.430<br />meanGDP: 4.017211<br />continent: Asia","year: 2000.734<br />meanGDP: 4.012967<br />continent: Asia","year: 2000.038<br />meanGDP: 4.008723<br />continent: Asia","year: 1999.342<br />meanGDP: 4.004479<br />continent: Asia","year: 1998.646<br />meanGDP: 4.000235<br />continent: Asia","year: 1997.949<br />meanGDP: 3.995990<br />continent: Asia","year: 1997.253<br />meanGDP: 3.991746<br />continent: Asia","year: 1996.557<br />meanGDP: 3.987502<br />continent: Asia","year: 1995.861<br />meanGDP: 3.983258<br />continent: Asia","year: 1995.165<br />meanGDP: 3.979014<br />continent: Asia","year: 1994.468<br />meanGDP: 3.974770<br />continent: Asia","year: 1993.772<br />meanGDP: 3.970526<br />continent: Asia","year: 1993.076<br />meanGDP: 3.966282<br />continent: Asia","year: 1992.380<br />meanGDP: 3.962038<br />continent: Asia","year: 1991.684<br />meanGDP: 3.957794<br />continent: Asia","year: 1990.987<br />meanGDP: 3.953550<br />continent: Asia","year: 1990.291<br />meanGDP: 3.949306<br />continent: Asia","year: 1989.595<br />meanGDP: 3.945062<br />continent: Asia","year: 1988.899<br />meanGDP: 3.940818<br />continent: Asia","year: 1988.203<br />meanGDP: 3.936574<br />continent: Asia","year: 1987.506<br />meanGDP: 3.932330<br />continent: Asia","year: 1986.810<br />meanGDP: 3.928086<br />continent: Asia","year: 1986.114<br />meanGDP: 3.923841<br />continent: Asia","year: 1985.418<br />meanGDP: 3.919597<br />continent: Asia","year: 1984.722<br />meanGDP: 3.915353<br />continent: Asia","year: 1984.025<br />meanGDP: 3.911109<br />continent: Asia","year: 1983.329<br />meanGDP: 3.906865<br />continent: Asia","year: 1982.633<br />meanGDP: 3.902621<br />continent: Asia","year: 1981.937<br />meanGDP: 3.898377<br />continent: Asia","year: 1981.241<br />meanGDP: 3.894133<br />continent: Asia","year: 1980.544<br />meanGDP: 3.889889<br />continent: Asia","year: 1979.848<br />meanGDP: 3.885645<br />continent: Asia","year: 1979.152<br />meanGDP: 3.881401<br />continent: Asia","year: 1978.456<br />meanGDP: 3.877157<br />continent: Asia","year: 1977.759<br />meanGDP: 3.872913<br />continent: Asia","year: 1977.063<br />meanGDP: 3.868669<br />continent: Asia","year: 1976.367<br />meanGDP: 3.864425<br />continent: Asia","year: 1975.671<br />meanGDP: 3.860181<br />continent: Asia","year: 1974.975<br />meanGDP: 3.855937<br />continent: Asia","year: 1974.278<br />meanGDP: 3.851693<br />continent: Asia","year: 1973.582<br />meanGDP: 3.847448<br />continent: Asia","year: 1972.886<br />meanGDP: 3.843204<br />continent: Asia","year: 1972.190<br />meanGDP: 3.838960<br />continent: Asia","year: 1971.494<br />meanGDP: 3.834716<br />continent: Asia","year: 1970.797<br />meanGDP: 3.830472<br />continent: Asia","year: 1970.101<br />meanGDP: 3.826228<br />continent: Asia","year: 1969.405<br />meanGDP: 3.821984<br />continent: Asia","year: 1968.709<br />meanGDP: 3.817740<br />continent: Asia","year: 1968.013<br />meanGDP: 3.813496<br />continent: Asia","year: 1967.316<br />meanGDP: 3.809252<br />continent: Asia","year: 1966.620<br />meanGDP: 3.805008<br />continent: Asia","year: 1965.924<br />meanGDP: 3.800764<br />continent: Asia","year: 1965.228<br />meanGDP: 3.796520<br />continent: Asia","year: 1964.532<br />meanGDP: 3.792276<br />continent: Asia","year: 1963.835<br />meanGDP: 3.788032<br />continent: Asia","year: 1963.139<br />meanGDP: 3.783788<br />continent: Asia","year: 1962.443<br />meanGDP: 3.779544<br />continent: Asia","year: 1961.747<br />meanGDP: 3.775299<br />continent: Asia","year: 1961.051<br />meanGDP: 3.771055<br />continent: Asia","year: 1960.354<br />meanGDP: 3.766811<br />continent: Asia","year: 1959.658<br />meanGDP: 3.762567<br />continent: Asia","year: 1958.962<br />meanGDP: 3.758323<br />continent: Asia","year: 1958.266<br />meanGDP: 3.754079<br />continent: Asia","year: 1957.570<br />meanGDP: 3.749835<br />continent: Asia","year: 1956.873<br />meanGDP: 3.745591<br />continent: Asia","year: 1956.177<br />meanGDP: 3.741347<br />continent: Asia","year: 1955.481<br />meanGDP: 3.737103<br />continent: Asia","year: 1954.785<br />meanGDP: 3.732859<br />continent: Asia","year: 1954.089<br />meanGDP: 3.728615<br />continent: Asia","year: 1953.392<br />meanGDP: 3.724371<br />continent: Asia","year: 1952.696<br />meanGDP: 3.720127<br />continent: Asia","year: 1952.000<br />meanGDP: 3.715883<br />continent: Asia","year: 1952.000<br />meanGDP: 3.715883<br />continent: Asia"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(153,153,153,0.4)","hoveron":"points","hoverinfo":"x+y","name":"Asia","legendgroup":"Asia","showlegend":false,"xaxis":"x","yaxis":"y2","frame":null},{"x":[1952,1952.69620253165,1953.39240506329,1954.08860759494,1954.78481012658,1955.48101265823,1956.17721518987,1956.87341772152,1957.56962025316,1958.26582278481,1958.96202531646,1959.6582278481,1960.35443037975,1961.05063291139,1961.74683544304,1962.44303797468,1963.13924050633,1963.83544303797,1964.53164556962,1965.22784810127,1965.92405063291,1966.62025316456,1967.3164556962,1968.01265822785,1968.70886075949,1969.40506329114,1970.10126582278,1970.79746835443,1971.49367088608,1972.18987341772,1972.88607594937,1973.58227848101,1974.27848101266,1974.9746835443,1975.67088607595,1976.36708860759,1977.06329113924,1977.75949367089,1978.45569620253,1979.15189873418,1979.84810126582,1980.54430379747,1981.24050632911,1981.93670886076,1982.63291139241,1983.32911392405,1984.0253164557,1984.72151898734,1985.41772151899,1986.11392405063,1986.81012658228,1987.50632911392,1988.20253164557,1988.89873417722,1989.59493670886,1990.29113924051,1990.98734177215,1991.6835443038,1992.37974683544,1993.07594936709,1993.77215189873,1994.46835443038,1995.16455696203,1995.86075949367,1996.55696202532,1997.25316455696,1997.94936708861,1998.64556962025,1999.3417721519,2000.03797468354,2000.73417721519,2001.43037974684,2002.12658227848,2002.82278481013,2003.51898734177,2004.21518987342,2004.91139240506,2005.60759493671,2006.30379746835,2007,2007,2007,2006.30379746835,2005.60759493671,2004.91139240506,2004.21518987342,2003.51898734177,2002.82278481013,2002.12658227848,2001.43037974684,2000.73417721519,2000.03797468354,1999.3417721519,1998.64556962025,1997.94936708861,1997.25316455696,1996.55696202532,1995.86075949367,1995.16455696203,1994.46835443038,1993.77215189873,1993.07594936709,1992.37974683544,1991.6835443038,1990.98734177215,1990.29113924051,1989.59493670886,1988.89873417722,1988.20253164557,1987.50632911392,1986.81012658228,1986.11392405063,1985.41772151899,1984.72151898734,1984.0253164557,1983.32911392405,1982.63291139241,1981.93670886076,1981.24050632911,1980.54430379747,1979.84810126582,1979.15189873418,1978.45569620253,1977.75949367089,1977.06329113924,1976.36708860759,1975.67088607595,1974.9746835443,1974.27848101266,1973.58227848101,1972.88607594937,1972.18987341772,1971.49367088608,1970.79746835443,1970.10126582278,1969.40506329114,1968.70886075949,1968.01265822785,1967.3164556962,1966.62025316456,1965.92405063291,1965.22784810127,1964.53164556962,1963.83544303797,1963.13924050633,1962.44303797468,1961.74683544304,1961.05063291139,1960.35443037975,1959.6582278481,1958.96202531646,1958.26582278481,1957.56962025316,1956.87341772152,1956.17721518987,1955.48101265823,1954.78481012658,1954.08860759494,1953.39240506329,1952.69620253165,1952,1952],"y":[3.81238690500126,3.82014539427714,3.82790280502109,3.83565907581876,3.84341414105853,3.85116793062893,3.85892036959814,3.86667137787601,3.87442086985944,3.88216875406265,3.88991493273429,3.89765930146482,3.90540174878827,3.91314215578411,3.92088039568659,3.92861633351096,3.93634982570797,3.94408071986117,3.95180885444378,3.95953405865565,3.96725615236374,3.97497494617294,3.98269024165733,3.99040183178436,3.99810950156606,4.00581302897218,4.01351218613793,4.02120674089538,4.02889645865014,4.03658110461421,4.04426044639132,4.0519342568924,4.05960231753737,4.06726442167519,4.07492037812996,4.08257001475774,4.09021318188106,4.09784975545643,4.10547963982896,4.11310276993754,4.12071911285548,4.12832866858279,4.13593147004615,4.14352758230666,4.15111710101922,4.15870015022733,4.16627687960845,4.17384746130651,4.18141208649743,4.18897096183223,4.19652430589101,4.20407234576282,4.21161531384395,4.21915344492239,4.22668697359252,4.23421613202228,4.24174114807647,4.24926224378533,4.25677963413681,4.2642935261635,4.2718041182913,4.27931159991531,4.28681615116859,4.29431794285128,4.30181713649015,4.30931388450167,4.31680833043507,4.32430060927512,4.33179084778755,4.3392791648929,4.34676567205715,4.35425047368983,4.36173366754228,4.36921534510031,4.37669559196698,4.38417448823246,4.39165210882857,4.39912852386679,4.40660379895872,4.41407799551873,4.41407799551873,4.42978856424615,4.42203007497028,4.41427266422632,4.40651639342866,4.39876132818889,4.39100753861848,4.38325509964927,4.3755040913714,4.36775459938797,4.36000671518477,4.35226053651313,4.3445161677826,4.33677372045914,4.32903331346331,4.32129507356082,4.31355913573646,4.30582564353944,4.29809474938624,4.29036661480363,4.28264141059176,4.27491931688368,4.26720052307448,4.25948522759008,4.25177363746306,4.24406596768135,4.23636244027523,4.22866328310948,4.22096872835203,4.21327901059727,4.2055943646332,4.19791502285609,4.19024121235501,4.18257315171005,4.17491104757222,4.16725509111746,4.15960545448968,4.15196228736636,4.14432571379098,4.13669582941845,4.12907269930988,4.12145635639194,4.11384680066463,4.10624399920126,4.09864788694076,4.09105836822819,4.08347531902008,4.07589858963896,4.06832800794091,4.06076338274999,4.05320450741518,4.0456511633564,4.03810312348459,4.03056015540346,4.02302202432503,4.0154884956549,4.00795933722513,4.00043432117095,3.99291322546209,3.9853958351106,3.97788194308391,3.97037135095611,3.9628638693321,3.95535931807883,3.94785752639614,3.94035833275727,3.93286158474574,3.92536713881234,3.9178748599723,3.91038462145987,3.90289630435451,3.89540979719026,3.88792499555758,3.88044180170513,3.87296012414711,3.86547987728043,3.85800098101496,3.85052336041884,3.84304694538062,3.83557167028869,3.82809747372868,3.81238690500126],"text":["year: 1952.000<br />meanGDP: 3.820242<br />continent: Europe","year: 1952.696<br />meanGDP: 3.827859<br />continent: Europe","year: 1953.392<br />meanGDP: 3.835475<br />continent: Europe","year: 1954.089<br />meanGDP: 3.843091<br />continent: Europe","year: 1954.785<br />meanGDP: 3.850708<br />continent: Europe","year: 1955.481<br />meanGDP: 3.858324<br />continent: Europe","year: 1956.177<br />meanGDP: 3.865940<br />continent: Europe","year: 1956.873<br />meanGDP: 3.873557<br />continent: Europe","year: 1957.570<br />meanGDP: 3.881173<br />continent: Europe","year: 1958.266<br />meanGDP: 3.888789<br />continent: Europe","year: 1958.962<br />meanGDP: 3.896406<br />continent: Europe","year: 1959.658<br />meanGDP: 3.904022<br />continent: Europe","year: 1960.354<br />meanGDP: 3.911638<br />continent: Europe","year: 1961.051<br />meanGDP: 3.919255<br />continent: Europe","year: 1961.747<br />meanGDP: 3.926871<br />continent: Europe","year: 1962.443<br />meanGDP: 3.934487<br />continent: Europe","year: 1963.139<br />meanGDP: 3.942104<br />continent: Europe","year: 1963.835<br />meanGDP: 3.949720<br />continent: Europe","year: 1964.532<br />meanGDP: 3.957336<br />continent: Europe","year: 1965.228<br />meanGDP: 3.964953<br />continent: Europe","year: 1965.924<br />meanGDP: 3.972569<br />continent: Europe","year: 1966.620<br />meanGDP: 3.980185<br />continent: Europe","year: 1967.316<br />meanGDP: 3.987802<br />continent: Europe","year: 1968.013<br />meanGDP: 3.995418<br />continent: Europe","year: 1968.709<br />meanGDP: 4.003034<br />continent: Europe","year: 1969.405<br />meanGDP: 4.010651<br />continent: Europe","year: 1970.101<br />meanGDP: 4.018267<br />continent: Europe","year: 1970.797<br />meanGDP: 4.025883<br />continent: Europe","year: 1971.494<br />meanGDP: 4.033500<br />continent: Europe","year: 1972.190<br />meanGDP: 4.041116<br />continent: Europe","year: 1972.886<br />meanGDP: 4.048732<br />continent: Europe","year: 1973.582<br />meanGDP: 4.056349<br />continent: Europe","year: 1974.278<br />meanGDP: 4.063965<br />continent: Europe","year: 1974.975<br />meanGDP: 4.071582<br />continent: Europe","year: 1975.671<br />meanGDP: 4.079198<br />continent: Europe","year: 1976.367<br />meanGDP: 4.086814<br />continent: Europe","year: 1977.063<br />meanGDP: 4.094431<br />continent: Europe","year: 1977.759<br />meanGDP: 4.102047<br />continent: Europe","year: 1978.456<br />meanGDP: 4.109663<br />continent: Europe","year: 1979.152<br />meanGDP: 4.117280<br />continent: Europe","year: 1979.848<br />meanGDP: 4.124896<br />continent: Europe","year: 1980.544<br />meanGDP: 4.132512<br />continent: Europe","year: 1981.241<br />meanGDP: 4.140129<br />continent: Europe","year: 1981.937<br />meanGDP: 4.147745<br />continent: Europe","year: 1982.633<br />meanGDP: 4.155361<br />continent: Europe","year: 1983.329<br />meanGDP: 4.162978<br />continent: Europe","year: 1984.025<br />meanGDP: 4.170594<br />continent: Europe","year: 1984.722<br />meanGDP: 4.178210<br />continent: Europe","year: 1985.418<br />meanGDP: 4.185827<br />continent: Europe","year: 1986.114<br />meanGDP: 4.193443<br />continent: Europe","year: 1986.810<br />meanGDP: 4.201059<br />continent: Europe","year: 1987.506<br />meanGDP: 4.208676<br />continent: Europe","year: 1988.203<br />meanGDP: 4.216292<br />continent: Europe","year: 1988.899<br />meanGDP: 4.223908<br />continent: Europe","year: 1989.595<br />meanGDP: 4.231525<br />continent: Europe","year: 1990.291<br />meanGDP: 4.239141<br />continent: Europe","year: 1990.987<br />meanGDP: 4.246757<br />continent: Europe","year: 1991.684<br />meanGDP: 4.254374<br />continent: Europe","year: 1992.380<br />meanGDP: 4.261990<br />continent: Europe","year: 1993.076<br />meanGDP: 4.269606<br />continent: Europe","year: 1993.772<br />meanGDP: 4.277223<br />continent: Europe","year: 1994.468<br />meanGDP: 4.284839<br />continent: Europe","year: 1995.165<br />meanGDP: 4.292455<br />continent: Europe","year: 1995.861<br />meanGDP: 4.300072<br />continent: Europe","year: 1996.557<br />meanGDP: 4.307688<br />continent: Europe","year: 1997.253<br />meanGDP: 4.315304<br />continent: Europe","year: 1997.949<br />meanGDP: 4.322921<br />continent: Europe","year: 1998.646<br />meanGDP: 4.330537<br />continent: Europe","year: 1999.342<br />meanGDP: 4.338154<br />continent: Europe","year: 2000.038<br />meanGDP: 4.345770<br />continent: Europe","year: 2000.734<br />meanGDP: 4.353386<br />continent: Europe","year: 2001.430<br />meanGDP: 4.361003<br />continent: Europe","year: 2002.127<br />meanGDP: 4.368619<br />continent: Europe","year: 2002.823<br />meanGDP: 4.376235<br />continent: Europe","year: 2003.519<br />meanGDP: 4.383852<br />continent: Europe","year: 2004.215<br />meanGDP: 4.391468<br />continent: Europe","year: 2004.911<br />meanGDP: 4.399084<br />continent: Europe","year: 2005.608<br />meanGDP: 4.406701<br />continent: Europe","year: 2006.304<br />meanGDP: 4.414317<br />continent: Europe","year: 2007.000<br />meanGDP: 4.421933<br />continent: Europe","year: 2007.000<br />meanGDP: 4.421933<br />continent: Europe","year: 2007.000<br />meanGDP: 4.421933<br />continent: Europe","year: 2006.304<br />meanGDP: 4.414317<br />continent: Europe","year: 2005.608<br />meanGDP: 4.406701<br />continent: Europe","year: 2004.911<br />meanGDP: 4.399084<br />continent: Europe","year: 2004.215<br />meanGDP: 4.391468<br />continent: Europe","year: 2003.519<br />meanGDP: 4.383852<br />continent: Europe","year: 2002.823<br />meanGDP: 4.376235<br />continent: Europe","year: 2002.127<br />meanGDP: 4.368619<br />continent: Europe","year: 2001.430<br />meanGDP: 4.361003<br />continent: Europe","year: 2000.734<br />meanGDP: 4.353386<br />continent: Europe","year: 2000.038<br />meanGDP: 4.345770<br />continent: Europe","year: 1999.342<br />meanGDP: 4.338154<br />continent: Europe","year: 1998.646<br />meanGDP: 4.330537<br />continent: Europe","year: 1997.949<br />meanGDP: 4.322921<br />continent: Europe","year: 1997.253<br />meanGDP: 4.315304<br />continent: Europe","year: 1996.557<br />meanGDP: 4.307688<br />continent: Europe","year: 1995.861<br />meanGDP: 4.300072<br />continent: Europe","year: 1995.165<br />meanGDP: 4.292455<br />continent: Europe","year: 1994.468<br />meanGDP: 4.284839<br />continent: Europe","year: 1993.772<br />meanGDP: 4.277223<br />continent: Europe","year: 1993.076<br />meanGDP: 4.269606<br />continent: Europe","year: 1992.380<br />meanGDP: 4.261990<br />continent: Europe","year: 1991.684<br />meanGDP: 4.254374<br />continent: Europe","year: 1990.987<br />meanGDP: 4.246757<br />continent: Europe","year: 1990.291<br />meanGDP: 4.239141<br />continent: Europe","year: 1989.595<br />meanGDP: 4.231525<br />continent: Europe","year: 1988.899<br />meanGDP: 4.223908<br />continent: Europe","year: 1988.203<br />meanGDP: 4.216292<br />continent: Europe","year: 1987.506<br />meanGDP: 4.208676<br />continent: Europe","year: 1986.810<br />meanGDP: 4.201059<br />continent: Europe","year: 1986.114<br />meanGDP: 4.193443<br />continent: Europe","year: 1985.418<br />meanGDP: 4.185827<br />continent: Europe","year: 1984.722<br />meanGDP: 4.178210<br />continent: Europe","year: 1984.025<br />meanGDP: 4.170594<br />continent: Europe","year: 1983.329<br />meanGDP: 4.162978<br />continent: Europe","year: 1982.633<br />meanGDP: 4.155361<br />continent: Europe","year: 1981.937<br />meanGDP: 4.147745<br />continent: Europe","year: 1981.241<br />meanGDP: 4.140129<br />continent: Europe","year: 1980.544<br />meanGDP: 4.132512<br />continent: Europe","year: 1979.848<br />meanGDP: 4.124896<br />continent: Europe","year: 1979.152<br />meanGDP: 4.117280<br />continent: Europe","year: 1978.456<br />meanGDP: 4.109663<br />continent: Europe","year: 1977.759<br />meanGDP: 4.102047<br />continent: Europe","year: 1977.063<br />meanGDP: 4.094431<br />continent: Europe","year: 1976.367<br />meanGDP: 4.086814<br />continent: Europe","year: 1975.671<br />meanGDP: 4.079198<br />continent: Europe","year: 1974.975<br />meanGDP: 4.071582<br />continent: Europe","year: 1974.278<br />meanGDP: 4.063965<br />continent: Europe","year: 1973.582<br />meanGDP: 4.056349<br />continent: Europe","year: 1972.886<br />meanGDP: 4.048732<br />continent: Europe","year: 1972.190<br />meanGDP: 4.041116<br />continent: Europe","year: 1971.494<br />meanGDP: 4.033500<br />continent: Europe","year: 1970.797<br />meanGDP: 4.025883<br />continent: Europe","year: 1970.101<br />meanGDP: 4.018267<br />continent: Europe","year: 1969.405<br />meanGDP: 4.010651<br />continent: Europe","year: 1968.709<br />meanGDP: 4.003034<br />continent: Europe","year: 1968.013<br />meanGDP: 3.995418<br />continent: Europe","year: 1967.316<br />meanGDP: 3.987802<br />continent: Europe","year: 1966.620<br />meanGDP: 3.980185<br />continent: Europe","year: 1965.924<br />meanGDP: 3.972569<br />continent: Europe","year: 1965.228<br />meanGDP: 3.964953<br />continent: Europe","year: 1964.532<br />meanGDP: 3.957336<br />continent: Europe","year: 1963.835<br />meanGDP: 3.949720<br />continent: Europe","year: 1963.139<br />meanGDP: 3.942104<br />continent: Europe","year: 1962.443<br />meanGDP: 3.934487<br />continent: Europe","year: 1961.747<br />meanGDP: 3.926871<br />continent: Europe","year: 1961.051<br />meanGDP: 3.919255<br />continent: Europe","year: 1960.354<br />meanGDP: 3.911638<br />continent: Europe","year: 1959.658<br />meanGDP: 3.904022<br />continent: Europe","year: 1958.962<br />meanGDP: 3.896406<br />continent: Europe","year: 1958.266<br />meanGDP: 3.888789<br />continent: Europe","year: 1957.570<br />meanGDP: 3.881173<br />continent: Europe","year: 1956.873<br />meanGDP: 3.873557<br />continent: Europe","year: 1956.177<br />meanGDP: 3.865940<br />continent: Europe","year: 1955.481<br />meanGDP: 3.858324<br />continent: Europe","year: 1954.785<br />meanGDP: 3.850708<br />continent: Europe","year: 1954.089<br />meanGDP: 3.843091<br />continent: Europe","year: 1953.392<br />meanGDP: 3.835475<br />continent: Europe","year: 1952.696<br />meanGDP: 3.827859<br />continent: Europe","year: 1952.000<br />meanGDP: 3.820242<br />continent: Europe","year: 1952.000<br />meanGDP: 3.820242<br />continent: Europe"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(153,153,153,0.4)","hoveron":"points","hoverinfo":"x+y","name":"Europe","legendgroup":"Europe","showlegend":false,"xaxis":"x2","yaxis":"y2","frame":null}],"layout":{"margin":{"t":57.0460772104608,"r":7.30593607305936,"b":29.4848408743456,"l":91.9883769198838},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"GDP per capita","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,0.489128071319852],"automargin":true,"type":"linear","autorange":false,"range":[1949.25,2009.75],"tickmode":"array","ticktext":["1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"],"tickvals":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"categoryorder":"array","categoryarray":["1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.2835201328352},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0.542894700428947,1],"automargin":true,"type":"linear","autorange":false,"range":[3.0312035755894,4.49638784942028],"tickmode":"array","ticktext":["$3,000.00","$10,000.00","$30,000.00"],"tickvals":[3.47712125471966,4,4.47712125471966],"categoryorder":"array","categoryarray":["$3,000.00","$10,000.00","$30,000.00"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.2835201328352},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"annotations":[{"text":"Mean GDP","x":-0.115143608294293,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"Africa","x":0.244564035659926,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":13.2835201328352},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Americas","x":0.755435964340074,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":13.2835201328352},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Asia","x":0.244564035659926,"y":0.457105299571053,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":13.2835201328352},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"Europe","x":0.755435964340074,"y":0.457105299571053,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":13.2835201328352},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.489128071319852,"y0":0.542894700428947,"y1":1},{"type":"rect","fillcolor":"rgba(230,230,250,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.489128071319852,"y0":0,"y1":24.9730178497302,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.510871928680148,"x1":1,"y0":0.542894700428947,"y1":1},{"type":"rect","fillcolor":"rgba(230,230,250,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.510871928680148,"x1":1,"y0":0,"y1":24.9730178497302,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.489128071319852,"y0":0,"y1":0.457105299571053},{"type":"rect","fillcolor":"rgba(230,230,250,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.489128071319852,"y0":0,"y1":24.9730178497302,"yanchor":0.457105299571053,"ysizemode":"pixel"},{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.510871928680148,"x1":1,"y0":0,"y1":0.457105299571053},{"type":"rect","fillcolor":"rgba(230,230,250,1)","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.510871928680148,"x1":1,"y0":0,"y1":24.9730178497302,"yanchor":0.457105299571053,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[1949.25,2009.75],"tickmode":"array","ticktext":["1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"],"tickvals":[1952,1957,1962,1967,1972,1977,1982,1987,1992,1997,2002,2007],"categoryorder":"array","categoryarray":["1952","1957","1962","1967","1972","1977","1982","1987","1992","1997","2002","2007"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.2835201328352},"tickangle":-45,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0.510871928680148,1],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y2","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis2":{"type":"linear","autorange":false,"range":[3.0312035755894,4.49638784942028],"tickmode":"array","ticktext":["$3,000.00","$10,000.00","$30,000.00"],"tickvals":[3.47712125471966,4,4.47712125471966],"categoryorder":"array","categoryarray":["$3,000.00","$10,000.00","$30,000.00"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.2835201328352},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"domain":[0,0.457105299571053],"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":1},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"468d1ea5ba5c":{"x":{},"y":{},"colour":{},"type":"scatter"},"468d126dfdbc":{"x":{},"y":{},"colour":{}}},"cur_data":"468d1ea5ba5c","visdat":{"468d1ea5ba5c":["function (y) ","x"],"468d126dfdbc":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

#### Part 4: Writing figures to file

``` r
#ggsave("hw05_factor_figure_mgmt_files/plot_gdpchange.png", p2)

#ggsave("hw05_factor_figure_mgmt_files/plot_gdpchange.svg", p2, units = "cm", width = 12, height = 8)
```
