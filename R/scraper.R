#' Scrape the Statinfo dissemination database.
#'
#' Scrape the Statinfo dissemination database of the Hungarian Central Statistics Office.
#'
#' @param code The code of the data set (given under the label 'Technical identifier').
#' @param column A list of vectors, where each vectors is a stratificiation variable for the column,
#' with the elements of the vector specifying the strata.
#' @param row A list of vectors, where each vectors is a stratificiation variable for the row, with
#' the elements of the vector specifying the strata.
#'
#' @return The raw file returned by the Statinfo database as a data frame.
#' @export
#'
#' @examples
#' KSHStatinfoScrape( "NT5C01", "[NTAC001]", list( c( "[2015]", "[2016]" ), c( "[N101_1]", "[N101_2]" ) ) )
KSHStatinfoScrape <- function( code, column, row ) {

  rowcolparser <- function( sel ) {
    n <- length( sel )
    if( n==1 ) {
      paste( sapply( sel, function( x ) paste( c( "{", paste( x, collapse = "," ), "}" ),
                                               collapse = "" ) ), collapse = "," )
    } else if( n==2 ) {
      paste0( "CrossJoin(", paste( sapply( sel, function( x ) paste( c( "{", paste( x, collapse = "," ), "}" ),
                                                                     collapse = "" ) ), collapse = ", " ), ")" )
    } else {
      paste0( paste( sapply( sel[ 1:(n-2) ], function( x ) paste( c( "CrossJoin({", paste( x, collapse = ", " ), "}" ),
                                                                  collapse = "" ) ), collapse = ", " ),
              ", CrossJoin(", paste( sapply( sel[ (n-1):n ], function( x ) paste( c( "{", paste( x, collapse = ", " ), "}" ),
                                                                                  collapse = "" ) ), collapse = ", " ), ")",
              paste0( rep( ")", n-2 ), collapse = "" ) )
    }
  }

  columnparsed <- rowcolparser( column )
  rowparsed <- rowcolparser( row )

  queryfile <- tempfile( fileext = ".ssq" )
  cat( "## Statinfo Saved Query ##\n", file = queryfile, append = FALSE )
  cat( "Cube: ", code, "\n", file = queryfile, sep = "", append = TRUE )
  cat( "MDX: SELECT", columnparsed, "ON COLUMNS,", rowparsed, "ON ROWS \n", file = queryfile, append = TRUE )
  cat( "Date: ", format( Sys.time(), "%Y.%m.%d. %H:%M"), "\n", file = queryfile, sep = "", append = TRUE )
  cat( "Language: hu\n", file = queryfile, append = TRUE )
  cat( "CustomItemOrder: 0;;0\n\n", file = queryfile, append = TRUE )

  session <- rvest::html_session( paste0( "http://statinfo.ksh.hu/Statinfo/QueryServlet?ha=", code ) )
  form <- rvest::html_form( session )
  form <- rvest::html_form( rvest::submit_form( session, form[[ 1 ]], submit = "tab.show.button" ) )

  res <- httr::POST( "http://statinfo.ksh.hu/Statinfo/haViewer.jsp", session$config,
                     handle = session$handle, encode = "form",
                     body = list( toolbar01.loadButton.x = 30, toolbar01.loadButton.y = 30 ) )
  form <- rvest::html_form( httr::content( res ) )

  form <- rvest::set_values( form[[ 3 ]], loadQueryName = httr::upload_file( queryfile ) )
  res <- rvest::submit_form( session, form, submit = "loadbutton" )
  if( length( rvest::html_nodes( res, "td[class='errorMessage nowrap']" ) )>0 ) {
    stop( paste0( "Error: '", trimws( rvest::html_text( rvest::html_nodes( res, "td[class='errorMessage nowrap']" ) ) ),
                  "' (perhaps some strata are not meaningful here?)." ) )
  }
  if( trimws( strsplit( rvest::html_text( rvest::html_nodes( res, "td[id='renderedCells']" ) ),
                        stringi::stri_unescape_unicode( "sz\\u00e1ma" ) )[[1]][2] )=="" ) {
    stop( "Error: no data returned (perhaps maximum limit of 15000 cells was exceeded?)." )
  }
  res <- rvest::jump_to( res, "http://statinfo.ksh.hu/Statinfo/Print?cube=01&type=0" )
  responsefile <- tempfile( fileext = ".xls" )
  writeBin( res$response$content, responsefile )

  resfile <- data.frame( readxl::read_xls( responsefile, sheet = 1, skip = 6 ) )

  unlink( queryfile )
  unlink( responsefile )

  resfile
}
