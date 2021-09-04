#' New Ace Science Fiction Series 3 colour palettes
#'
#' `pal_sf` produces sequential or divergent colour palettes
#' based on the 12 original book covers released in the third
#' Ace SF series released between 1984 and 1990.
#'
#' @param book Reference identifier to one of the 12 books.
#' @param n Number of colors to return.
#' @param type Type of colour palette, either sequential or diverging.
#'
#' @return Vector of colour hex codes.
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#'  pal_sf(book = 'green eyes',
#'         n = 6,
#'         type = 'div')
#' }

pal_sf <- function(book = 'neuromancer',
                   n = 3,
                   type = 'div') {

  ## Create reference dataframe
  df_books <- books_sf()

  ## Check input rules
  if (!book %in% c(purrr::as_vector(df_books[,1]),
                   purrr::as_vector(df_books[,2]),
                   purrr::as_vector(df_books[,3]),
                   purrr::as_vector(df_books[,4]),
                   purrr::as_vector(df_books[,5]),
                   purrr::as_vector(df_books[,6]),
                   purrr::as_vector(df_books[,7]),
                   purrr::as_vector(df_books[,8]),
                   'iamrichard', 0451, '0451')) {
    print(books_sf())
    print('Please select a valid book using any of the reference information above.')
    stop()
  }
  if (!type %in% c('sequential', 'seq',
                   'diverging',  'div')) {
    stop('Please select a "sequential" or "diverging" palette type.')
  }
  if (n == 1) {
    print('Defaulting to n = 2.')
    n = 2
  }

  ## Start with divergent pattern
  ## 1: The Wild Shore # via palette creator ~ c('#F8005E', '#131311', '#FEB300')
  v_pal <- dplyr::case_when(book %in% df_books[1,] ~ c('#ee0459', '#0f100d', '#fcb100'),
                            ## 2: Green Eyes # via palette creator ~ c('#D0136F', '#222126', '#46A492')
                            book %in% df_books[2,] ~ c('#D0136F', '#1f0f47', '#1bf913'),
                            ## 3: Neuromancer # via palette creator ~ c('#F73E81', '#313035', '#3D94FE')
                            book %in% df_books[3,] ~ c('#fd34b2', '#1f0f47', '#51e6ff'),
                            ## 4: Palimpsests # via palette creator ~ c('#745172', '#1F241D', '#EFAF68') N.B. May substitute color 1 with #C32F5F (hotpink) or #583a86 (purple)
                            book %in% df_books[4,] ~ c('#a53075', '#0f100d', '#fcc15f'),
                            ## 5: Them Bones # via palette creator ~ c('#90BAA6', '#323A3C', '#DF790B')
                            book %in% df_books[5,] ~ c('#aad6d7', '#1a1f20', '#ff904e'),
                            ## 6: In the Drift # via palette creator ~ c('#F09B1E', '#292D2E', '#FE2446')
                            book %in% df_books[6,] ~ c('#d08743', '#121511', '#ea3b3c'),
                            ## 7: The Hercules Text # via palette creator ~ c('#9F0C26', '#1B261E', '#9F94A2') N.B. Different style. May substitute #CFD56B for middle(?)
                            book %in% df_books[7,] ~ c('#9e1427', '#15221c', '#9e95a2'),
                            ## 8: The Net # via palette creator ~ c('#455D99', '#051816', '#E34B00')
                            book %in% df_books[8,] ~ c('#2e63a0', '#021611', '#e85100'),
                            ## 9: Metrophage # via palette creator ~ c('#40125D', '#10110C', '#155652')
                            book %in% df_books[9,] ~ c('#55226f', '#121116', '#46748b'),
                            ## 10: The Tides of God # via palette creator ~ c('#970F59', '#17170D', '#E91700')
                            book %in% df_books[10,] ~ c('#9e0e5e', '#191709', '#f22007'),
                            ## 11: Black Snow Days # via palette creator ~  c('#1AA19B', '#3B3223', '#D36A54')
                            book %in% df_books[11,] ~ c('#098893', '#312e27', '#d3553c'),
                            ## 12: The Oxygen Barons # via palette creator ~ c('#E13A06', '#1F1B0F', '#1A6E70'),
                            book %in% df_books[12,] ~ c('#e43e07', '#252113', '#217b7b'),
                            ## S1: Metrophage (re-release) # via palette creator ~ c('#AE53A0', '#0C0C14', '#1F963A')
                            book %in% c('iamrichard') ~ c('#7cdb4a', '#3f3993', '#ad4a9a'),
                            ## S2: Deus Ex # N.B. May need to experiment with these....
                            book %in% c('0451', 0451) ~ c('#E7F8FD', '#656ECB', '#1F2C5C'),
                            ## SX: Original Neuromancer manual color ramp
                            TRUE ~ c('#51e6ff', '#1f0f47', '#fd34b2'))

  if (type %in% c('seq', 'sequential')) {
    v_pal <- v_pal[c(1,3)]
  }

  pal <- grDevices::colorRampPalette(v_pal)(n)

  rm(v_pal); rm(df_books)
  return(pal)
}

books_sf <- function() {

  df_books <- data.table::data.table(NUMBER = 1,
          BOOK = 'The Wild Shore',
          AUTHOR = 'Kim Stanley Robinson',
          DATE = '1984-03',
          BOOK_CODE1 = 'tws',
          BOOK_CODE2 = 'wild shore',
          AUTHOR_CODE1 = 'ksr',
          AUTHOR_CODE2 = 'robinson') %>%
    tibble::add_row(NUMBER = 2,  BOOK = 'Green Eyes',        AUTHOR = 'Lucius Shepard',                 DATE = '1984-05', BOOK_CODE1 = 'ge',   BOOK_CODE2 = 'green eyes',      AUTHOR_CODE1 = 'ls',  AUTHOR_CODE2 = 'shepard') %>%
    tibble::add_row(NUMBER = 3,  BOOK = 'Neuromancer',       AUTHOR = 'William Gibson',                 DATE = '1984-07', BOOK_CODE1 = 'n',    BOOK_CODE2 = 'neuromancer',     AUTHOR_CODE1 = 'wg',  AUTHOR_CODE2 = 'gibson')  %>%
    tibble::add_row(NUMBER = 4,  BOOK = 'Palimpsests',       AUTHOR = 'Carter Scholz & Glenn Harcourt', DATE = '1984-09', BOOK_CODE1 = 'p',    BOOK_CODE2 = 'palimpsests',     AUTHOR_CODE1 = 'cs',  AUTHOR_CODE2 = 'scholz') %>%
    tibble::add_row(NUMBER = 5,  BOOK = 'Them Bones',        AUTHOR = 'Howard Waldrop',                 DATE = '1984-11', BOOK_CODE1 = 'tb',   BOOK_CODE2 = 'them bones',      AUTHOR_CODE1 = 'hw',  AUTHOR_CODE2 = 'waldrop') %>%
    tibble::add_row(NUMBER = 6,  BOOK = 'In the Drift',      AUTHOR = 'Michael Swanwick',               DATE = '1985-02', BOOK_CODE1 = 'itd',  BOOK_CODE2 = 'in the drift',    AUTHOR_CODE1 = 'ms',  AUTHOR_CODE2 = 'swanwick') %>%
    tibble::add_row(NUMBER = 7,  BOOK = 'The Hercules Text', AUTHOR = 'Jack McDevitt',                  DATE = '1986-11', BOOK_CODE1 = 'tht',  BOOK_CODE2 = 'hercules text',   AUTHOR_CODE1 = 'jm',  AUTHOR_CODE2 = 'mcdevitt') %>%
    tibble::add_row(NUMBER = 8,  BOOK = 'The Net',           AUTHOR = 'Loren J. MacGregor',             DATE = '1987-06', BOOK_CODE1 = 'tn',   BOOK_CODE2 = 'net',             AUTHOR_CODE1 = 'ljm', AUTHOR_CODE2 = 'macgregor') %>%
    tibble::add_row(NUMBER = 9,  BOOK = 'Metrophage',        AUTHOR = 'Richard Kadrey',                 DATE = '1988-02', BOOK_CODE1 = 'm',    BOOK_CODE2 = 'metrophage',      AUTHOR_CODE1 = 'rk',  AUTHOR_CODE2 = 'kadrey')  %>%
    tibble::add_row(NUMBER = 10, BOOK = 'The Tides of God',  AUTHOR = 'Ted Reynolds',                   DATE = '1989-02', BOOK_CODE1 = 'ttog', BOOK_CODE2 = 'tides of god',    AUTHOR_CODE1 = 'tr',  AUTHOR_CODE2 = 'reynolds') %>%
    tibble::add_row(NUMBER = 11, BOOK = 'Black Snow Days',   AUTHOR = 'Claudia O\'Keefe',               DATE = '1990-04', BOOK_CODE1 = 'bsd',  BOOK_CODE2 = 'black snow days', AUTHOR_CODE1 = 'co',  AUTHOR_CODE2 = 'okeefe') %>%
    tibble::add_row(NUMBER = 12, BOOK = 'The Oxygen Barons', AUTHOR = 'Gregory Feeley',                 DATE = '1990-07', BOOK_CODE1 = 'tob',  BOOK_CODE2 = 'oxygen barons',   AUTHOR_CODE1 = 'gf',  AUTHOR_CODE2 = 'feeley')

  return(df_books)

}


###################################
### FUNCTION    : pal_sf
### AUTHOR      : toddellis.wa@gmail.com
### DATE        : 2021-09-04 : 2021-03-10
### DESCRIPTION : Creates color palettes based on the 12 Ace SF Specials edited by Terry Carr in the 1980s.
### NOTES       :
###################################
