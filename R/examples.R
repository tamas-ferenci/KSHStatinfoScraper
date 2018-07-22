GetPopulationPyramidKSH <- function( Type = "Jan1", Years = 2015:2017, Gender = "Total",
                                     AgeGroup = "FiveYear", GeographicArea = "NUTS2",
                                     KSHCodesUsed = KSHCodes ) {
  if ( !Type%in%c( "Jan1", "MidYear" ) )
    stop( "Type must be either 'Jan1' or 'MidYear'!" )
  if ( !Gender%in%c( "Total", "Both" ) )
    stop( "Gender must be either 'Total' or 'Both'!" )
  if( !AgeGroup%in%c( "Total", "Main", "FiveYear" ) )
    stop( "AgeGroup must be 'Total', 'Main' or 'FiveYear'!" )
  if( !GeographicArea%in%c( "NUTS2", "NUTS3", "LAU1" ) )
    stop( "GeographicArea must be 'NUTS2', 'NUTS3' or 'LAU1'" )
  if( ( !is.numeric( Years ) )|( min( Years )<2015 )|( max( Years )>2017 ) )
    stop( "Years must be a numeric vector with elements between 2015 and 2017 (inclusive)!")

  PopPyramid <- do.call( rbind, lapply( Years, function( year )
    KSHStatinfoScrape( "NT5C01",
                       list( if( Type=="Jan1" ) "[NTAC001]" else "[NTCA003]" ),
                       list( paste0( "[", year, "]" ),
                             KSHCodesUsed[[ "Gender" ]][[ Gender ]],
                             KSHCodesUsed[[ "AgeGroup" ]][[ AgeGroup ]],
                             KSHCodesUsed[[ "GeographicArea" ]][[ GeographicArea ]] ) ) ) )

  PopPyramid <- tidyr::fill( PopPyramid, 1:3 )
  PopPyramid$`Időszak`` <- as.numeric( substring( PopPyramid$`Időszak``, 1, 4 ) )
  PopPyramid$Nem <- ifelse( PopPyramid$Nem=="Férfi", "Male", ifelse( PopPyramid$Nem=="Nő", "Female", "Total" ) )
  PopPyramid$Korcsoport <- if( AgeGroup=="Total" ) "Total" else as.numeric( substring( PopPyramid$Korcsoport, 1, 2 ) )
  names( PopPyramid ) <- c( "YEAR", "SEX", "AGE", "GEO", "POPULATION" )

  PopPyramid
}
