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

  session <- html_session( paste0( "http://statinfo.ksh.hu/Statinfo/QueryServlet?ha=", code ) )
  form <- html_form( session )
  form <- html_form( submit_form( session, form[[ 1 ]], submit = "tab.show.button" ) )

  request <- rvest:::submit_request( form[[ 2 ]], "toolbar01.loadButton" )
  url <- url_absolute( form[[ 2 ]]$url, session$url )
  res <- POST( url, session$config, handle = session$handle, encode = request$encode,
               body = list( toolbar01.loadButton.x = 30, toolbar01.loadButton.y = 30 ) )
  form <- html_form( read_html( res ) )

  form <- set_values( form[[ 3 ]], loadQueryName = upload_file( queryfile ) )
  res <- submit_form( session, form, submit = "loadbutton" )
  if( length( html_nodes( res, "td[class='errorMessage nowrap']" ) )>0 ) {
    stop( paste0( "Error: '", trimws( html_text( html_nodes( res, "td[class='errorMessage nowrap']" ) ) ),
                  "' (perhaps some strata are not meaningful here?)." ) )
  }
  if( trimws( strsplit( html_text( html_nodes( res, "td[id='renderedCells']" ) ), "száma:" )[[1]][2] )=="" ) {
    stop( "Error: no data returned (perhaps maximum limit of 15000 cells was exceeded?)." )
  }
  res <- jump_to( res, "http://statinfo.ksh.hu/Statinfo/Print?cube=01&type=0" )
  responsefile <- tempfile( fileext = ".xls" )
  writeBin( res$response$content, responsefile )

  resfile <- readWorksheetFromFile( responsefile, sheet = 1, startRow = 7 )

  unlink( queryfile )
  unlink( responsefile )

  resfile
}
