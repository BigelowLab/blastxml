#' Extract blast XML info 
#'
#' 
#' @export
#' @param xml_file character - the path and name of the XML file
#' @return list of
#' \itemize{
#' \item{filename}
#' \item{BlastOutput_program}
#' \item{BlastOutput_version}
#' \item{BlastOutput_db}
#' \item{N_iterations}
#' }
blastxml_info <- function(xml_file){
    
    X <- try(xml2::read_xml(xml_file))
    if (inherits(X, 'try-error')){
        stop("error reading XML")
    }

    list(
        'BlastOutput_program' = 
            xml2::xml_find_first(X,'BlastOutput_program') %>% 
                xml2::xml_text(),
        'BlastOutput_version' = 
            xml2::xml_find_first(X,'BlastOutput_version') %>% 
                xml2::xml_text(),
        'BlastOutput_db' = 
            xml2::xml_find_first(X,'BlastOutput_db') %>% 
                xml2::xml_text(),
         'N_Iterations' = 
             length(xml2::xml_find_all(X, 'BlastOutput_iterations/Iteration'))
    )
    
}


#' Extract the blast program name from the XML file
#'
#' @export
#' @param xml_file character - the path and name of the XML file
#' @return contents of  BlastOutput_program node in xml_file
blastxml_program <- function(xml_file){
    X <- try(xml2::read_xml(xml_file))
    if (inherits(X, 'try-error')){
        stop("error reading XML")
    }
    xml2::xml_find_first(X,'BlastOutput_program') %>% 
        xml2::xml_text()
}