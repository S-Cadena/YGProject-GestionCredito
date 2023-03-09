################################################################################
# Script que importa los CSV de Gestión de Crédito 
################################################################################
library(tidyverse)
library(lubridate)
library(plotly)
################################################################################
my_dataset <- NULL
my_path <- NULL
#my_path_parent <- 'D:/OneDrive - IBERTEL/ASEGURAMIENTO DE INGRESOS/07 CHI/04 ARE/Gestion_csv/'
#my_path_caledar <- 'D:/OneDrive - IBERTEL/ASEGURAMIENTO DE INGRESOS/0 SALDOS SIGMA VS SIGMA/PT CALENDARIO 2023.xlsx'
my_path_parent <- 'Gestion_csv/'
my_path_caledar <- 'Suplies/PT CALENDARIO 2023.xlsx'
################################################################################
my_calendar <- readxl::read_xlsx(my_path_caledar)
my_path_child <- list.files(my_path_parent)
my_path_child <- my_path_child[!my_path_child %>% str_detect('[.]')]
for (i in str_c(my_path_parent, my_path_child)) {
  x <- str_c(i, '/', list.files(i))
  my_path <- c(my_path, x)
}
my_week_lvl <- seq(
  as.Date('2023-01-02'),
  by = 'day',
  length.out = 7
) %>% format(
  '%A'
) %>% str_to_title(
)
################################################################################
for (i in my_path) {
  x <- read_delim(
    i,
    delim = ';',
    escape_double = FALSE,
    col_types = cols(.default = 'c'),
    locale = locale(encoding = "ISO-8859-1"),
    trim_ws = TRUE
  )
  my_dataset <- bind_rows(x, my_dataset)
}
my_dataset <- my_dataset %>% mutate(
  across(
    starts_with(c('credito', 'incremento')),
    ~.x %>% parse_number(locale = locale(grouping_mark = '.'))
  ),
  across(
    contains('fecha'),
    ~.x %>% ymd()
  )
) %>% mutate(
  FECHA = FECHA %>% ymd()
  #Semana = str_c('W', format(FECHA - 1, '%U')),
  #Dia = FECHA %>% format('%A') %>% str_to_title(),
  #Dia = Dia %>% factor(levels = my_week_lvl),
  #Ano = ifelse(FECHA == '2023-01-01', '2022', year(FECHA))
)
################################################################################
my_date_gap <- my_dataset$FECHA %>% unique() %>% sort()
my_report_day <- my_date_gap %>% tail(1)
my_date_max <- my_report_day
my_date_end <- my_report_day - 1
my_date_start <- my_date_end %m-% months(1)
my_date_min <- my_date_gap %>% head(1)
my_calendar <- my_calendar %>% mutate(
  FECHA = FECHA %>% ymd(),
  W_Year_Flag = c(2022, rep(2023, nrow(.) - 1)) %>% as.character()
) %>% filter(
#  FECHA <= my_report_day
) %>% arrange(
  FECHA
)
my_calendar_gap <- my_calendar %>% filter(
  FECHA >= my_date_start & FECHA <= my_date_end
)
################################################################################
my_num_cols <- my_dataset %>% select(where('is.numeric')) %>% names() %>% sort()
my_week_lst <- my_calendar$W %>% sort() %>% unique()
my_week_lst_gap <- my_calendar_gap$W %>% sort() %>% unique()
my_year_lst <- my_calendar$W_Year_Flag %>% sort() %>% unique()
my_mont_type_lst <- my_dataset$`TIPO INCREMENTO` %>% unique() %>% sort()
my_week_pal <- colorRampPalette(c('#00A3E0', '#1D428A'))(53)
################################################################################
my_dataset <- my_dataset %>% inner_join(
  my_calendar %>% transmute(
    FECHA,
    Dia = `DIA SEM`%>% str_to_title() %>% factor(levels = my_week_lvl),
    Semana = W,
    Ano = W_Year_Flag
  ),
  by = 'FECHA'
)
my_dataset <- my_dataset %>% mutate(
  flag1 = ifelse(
    Semana %in% my_week_lst_gap,
    'Mensual',
    'Historico'
  )
)
