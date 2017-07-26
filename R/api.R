#' @export
#' @import dplyr
testData <- function() {
  # SEPA data as output from the FinTS parser
  giro_dkb1 <- parse_fints("data-raw/DE62120300001002824108.csv") %>%
    mutate(Konto = "1002824108")

  giro_dkb2 <- parse_fints("data-raw/DE29120300001002969911.csv") %>%
    mutate(Konto = "1002969911")

  # credit card transactions obtained from dkbweb.py
  credit_dkb1 <- parse_credit_dkbweb("data-raw/1143.csv") %>%
    mutate(Konto = "1143")
  credit_dkb2 <- parse_credit_dkbweb("data-raw/4270.csv") %>%
    mutate(Konto = "4270")
  credit_dkb3 <- parse_credit_dkbweb("data-raw/5511.csv") %>%
    mutate(Konto = "5511")

  all <- tbl_df(bind_rows(giro_dkb1, giro_dkb2, credit_dkb1, credit_dkb2, credit_dkb3)) %>%
    mutate(Konto = as.factor(Konto))
}

#' @import dplyr
parse_fints <- function(filename) {
  readr::read_tsv(filename) %>%
    dplyr::mutate(Betrag = ifelse(soll_haben == "H", Betrag, -Betrag)) %>%
    dplyr::mutate(Belegdatum = as.Date(Datum, format="%Y-%d-%m")) %>%
    dplyr::select(Belegdatum, Beschreibung = Kontrahent, Betrag)
}

#' @import dplyr
parse_credit_dkbweb <- function(filename) {
  readr::read_csv2(filename, skip = 8, col_names = c("Abgerechnet", "Wertstellung", "Belegdatum", "Beschreibung", "Betrag", "OrigBetrag", "Kategorie")) %>%
    dplyr::mutate(Betrag = as.numeric(Betrag)) %>%
    dplyr::mutate(Belegdatum = as.Date(Belegdatum, format="%d.%m.%Y")) %>%
    dplyr::select(Belegdatum, Beschreibung, Betrag)
}
