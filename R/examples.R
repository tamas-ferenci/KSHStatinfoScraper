#' Scrape the population pyramid of Hungary.
#'
#' Scrape the population pyramid of Hungary possibly stratified according to gender, age group
#' (main groups, five-year groups or one-year groups) and geographic area (NUTS2, NUTS3 or LAU1).
#'
#' Data available from 1990 to 2017, but LAU1 geographical resoulation is available only after
#' 2014, and yearly age groups are only available for NUTS2 and NUTS3 resolution.
#'
#' @param Type Type of population (possible values are "Jan1" and "MidYear").
#' @param Years Numeric vector containing the years (from 2015 to 2017, inclusive).
#' @param Gender Stratification according to gender (possible values are "Total" and "Both").
#' @param AgeGroup Stratification according to age (possible values are "Total", "Main" and
#' "FiveYear").
#' @param GeographicArea Stratification according to geographic area (possible values are "NUTS2",
#' "NUTS3" and "LAU1").
#'
#' @return Neatly formatted population pyramid.
#' @export
#'
#' @examples GetPopulationPyramidKSH( "MidYear", Years = 2015, AgeGroup = "Main" )
GetPopulationPyramidKSH <- function( Type = "Jan1", Years = 2013:2016, Gender = "Both",
                                     AgeGroup = "Main", GeographicArea = "NUTS2" ) {
  if ( !Type%in%c( "Jan1", "MidYear" ) )
    stop( "Type must be either 'Jan1' or 'MidYear'!" )
  if ( !Gender%in%c( "Total", "Both" ) )
    stop( "Gender must be either 'Total' or 'Both'!" )
  if( !AgeGroup%in%c( "Total", "Main", "FiveYear" ) )
    stop( "AgeGroup must be 'Total', 'Main', 'FiveYear' or 'OneYear'!" )
  if( !GeographicArea%in%c( "NUTS2", "NUTS3", "LAU1" ) )
    stop( "GeographicArea must be 'NUTS2', 'NUTS3' or 'LAU1'" )
  if( !is.numeric( Years ) )
    stop( "Years must be a numeric vector!")
  if( ( max( Years )>2017 )|( min( Years )<1990 ) )
    stop( "No data is available after 2017 or before 1990!" )
  if( ( min( Years )<2015 )&( GeographicArea=="LAU1" ) )
    stop( "LAU1 level data only available after 2014!" )
  if( ( AgeGroup=="OneYear" )&( GeographicArea=="LAU1" ) )
    stop( "Yearly age groups are only available for NUTS2 and NUTS3 resolution!" )

  PopPyramid <- do.call( rbind, lapply( Years, function( year )
    KSHStatinfoScrape( if( GeographicArea=="LAU1" ) "NT5C01" else "NT1C02",
                       list( if( Type=="Jan1" ) "[NTAC001]" else "[NTAC003]" ),
                       list( paste0( "[", year, "]" ),
                             KSHCodes[[ "Gender" ]][[ Gender ]],
                             KSHCodes[[ "AgeGroup" ]][[ AgeGroup ]],
                             KSHCodes[[ "GeographicArea" ]][[ GeographicArea ]] ) ) ) )

  PopPyramid <- tidyr::fill( PopPyramid, 1:3 )
  names( PopPyramid ) <- c( "YEAR", "SEX", "AGE", "GEO", "POPULATION" )
  PopPyramid$YEAR <- as.numeric( substring( PopPyramid$YEAR, 1, 4 ) )
  PopPyramid$SEX <- ifelse( PopPyramid$SEX==stringi::stri_unescape_unicode( "F\\u00e9rfi" ), "Male",
                            ifelse( PopPyramid$SEX==stringi::stri_unescape_unicode( "N\\u0151" ),
                                    "Female", "Total" ) )
  PopPyramid$AGE <- if( AgeGroup=="Total" ) "Total" else as.numeric( substring( PopPyramid$AGE, 1,
                                                                                2 ) )

  PopPyramid
}
