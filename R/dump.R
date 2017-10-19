#' A list to identify the Blast fields to retrieve.
#'
#' blastn includes...
#'  Iteration_iter_num
#'  Iteration_query_id
#'  Iteration_query_def
#'  Iteration_query_len
#'  Hit_num
#'  Hit_id
#'  Hit_def
#'  Hit_accession
#'  Hit_len
#'  Hsp_num
#'  Hsp_bit_score
#'  Hsp_score
#'  Hsp_evalue
#'  Hsp_query_from
#'  Hsp_query_to
#'  Hsp_hit_from
#'  Hsp_hit_to
#'  Hsp_query_frame
#'  Hsp_identity
#'  Hsp_positive
#'  Hsp_gaps
#'  Hsp_align_len
#'  Hsp_qseq
#'  Hsp_hseq
#'  Hsp_midline
#'
#' @export
#' @param x character by default the fields are for 'blastn' but this 
#'  is set automatically when the file is opened.  Alternatively, this can be
#'  the xml filename. 
#' @param fields character of NULL  if not NULL fields are subset to include 
#'  just these.  The default can be found using \code{get_BXD_names()}
#' @return a named list of fields to return
#' \itemize{
#'  \item{app the name of the blast application by default 'blastn'}
#'  \item{blastn a list of character vectors fields in for Iteration, Hit and Hsp}
#' }
BXD <- function(x = 'blastn', fields = NULL){
    if (file.exists(x)){
        app = blastxml_program(x)
    } else {
        app = x
    }
    bxd <- list(
        app = app,
        blastn = list(
            Iteration = c(
                "Iteration_iter-num",
                "Iteration_query-ID",
                "Iteration_query-def",
                "Iteration_query-len"),
            Hit = c(
                "Hit_num",
                "Hit_id",
                "Hit_def",
                "Hit_accession",
                "Hit_len"),
            Hsp = c(
                "Hsp_num",
                "Hsp_bit-score",
                "Hsp_score",
                "Hsp_evalue",
                "Hsp_query-from",
                "Hsp_query-to",
                "Hsp_hit-from",
                "Hsp_hit-to",
                "Hsp_query-frame",
                "Hsp_identity",
                "Hsp_positive",
                "Hsp_gaps",
                "Hsp_align-len",
                "Hsp_qseq",
                "Hsp_hseq",
                "Hsp_midline")
            )
        )
    }
        
#' Get the fields for the application
#' 
#' @export
#' @param bxd a \code{BXD()} like list
#' @return a character vector
get_BXD_names <- function(bxd = BXD()) {
    unname(do.call(c, bxd[[bxd$app]]))
}

#' Set the fields to retrieve for the application
#' 
#' @export
#' @param x a character vector of names or NULL.  If NULL nothing 
#'  is done.
#' @param bxd a \code{BXD()} like list
#' @return the possibly updated BXD global
#' @examples
#'  \dontrun{
#'      xmlfile <- system.file("extdata", "blastn.xml.gz", package = "blastxml")
#'      bxd <- BXD(app = 'blastn')
#'      fields <- get_BXD_names()
#'      len <- length(fields)
#'      fields <- fields[-c((len-3):len)]
#'      bxd <- set_BXD_names(fields, bxd = bxd)
#'      x <- blastxml_dump(xmlfile, bxd = bxd)
#'  }  
set_BXD_names <- function(x, bxd = BXD()){
    
    if (missing(x) || is.null(x)) return(invisible(bxd))
    
    ss <- strsplit(x, '_', fixed = TRUE)
    s <- sapply(ss, '[[', 1)
    xx <- split(x, s)
    
    for (n in names(bxd[[bxd$app]])){
        if (n %in% names(xx)){
            bxd[[bxd$app]][[n]] <- xx[[n]]
        }
    }
    invisible(bxd)
}

#' Extract values from an Hsp node
#'
#' @export
#' @param x xml2::xml_node for Hsp
#' @param bxd list of known fields for the blast type, see \code{link{BXD}}
#' @return a character vector of Hsp value or NULL
hsp_extract <- function(x, bxd = BXD() ){
    
    nm = bxd[[bxd$app]][['Hsp']]
    if (length(nm) == 0) return(NULL)
     
    sapply(nm,
        function(n) x %>% xml2::xml_find_first(n) %>% xml2::xml_text())
}

#' Extract values from an Hit node
#'
#' @export
#' @param x xml2::xml_node for Hit
#' @param bxd list of known fields for the blast type, see \code{link{BXD}}
#' @return a character vector of Hit value or NULL
hit_extract <- function(x, bxd = BXD()){
    
    nm = bxd[[bxd$app]][['Hit']]
    if (length(nm) == 0) return(NULL)
    
    hit <- sapply(nm,
        function(n) x %>% xml2::xml_find_first(n) %>% xml2::xml_text())
        
    hsps_set <- xml2::xml_find_all(x, "Hit_hsps/Hsp")
    if (length(hsps_set) == 0) return(NULL)
    hsps <- do.call(rbind, lapply(hsps_set, hsp_extract, bxd = bxd))
    
    cbind(do.call(rbind,rep(list(hit), nrow(hsps))), hsps)
}

#' Extract values from an Iteration node
#'
#' @export
#' @param x xml2::xml_node for Iteration
#' @param bxd list of known fields for the blast type, see \code{link{BXD}}
#' @return a character vector of Iteration value or NULL
iter_extract <- function(x, bxd = BXD()){

    nm = bxd[[bxd$app]][['Iteration']]
    if (length(nm) == 0) return(NULL)
    
    iter <- sapply(nm,
        function(n) x %>% xml2::xml_find_first(n) %>% xml2::xml_text())
    
    hits_set <- xml2::xml_find_all(x,"Iteration_hits/Hit")
    
    if (length(hits_set) == 0) return(NULL)
    hits <- do.call(rbind, lapply(hits_set, hit_extract, bxd = bxd))
    
    cbind(do.call(rbind,rep(list(iter), nrow(hits))), hits)
}

#' Retrieve the path for an example blastn file
#'
#' @export
#' @param app character, by default 'blastn'
#' @return a filename or empty string
get_example_filename <- function(app = 'blastn'){
    system.file("extdata", paste0(app[1], ".xml.gz"),package = "blastxml")
}

#' Extract blast XML contents possibly writing to a text file
#'
#' 
#' @export
#' @param xml_file character - the path and name of the XML file
#' @param out_file character the path and name to output file, 
#'  or NULL to skip saving the file
#' @param sep charcater field separator if writing to file
#' @param bxd list of known fields for the blast type, see \code{link{BXD}}
#' @param fields character vector of fields to extract, defaults to all in bxd
#' @param form character, either matrix or tibble
#' @param ... further arguments for \code{\link{write.table}}
#' @return character matrix or tibble invisibly
blastxml_dump <- function(
    xml_file = get_example_filename(), 
    out_file = NULL,
    sep = "\t",
    bxd = NULL,
    fields = NULL, 
    form = c('matrix', 'tibble')[1],
    ...){
    
    if (is.null(bxd)) bxd = BXD(x = blastxml_program(xml_file))   

    if (!is.null(fields)) bxd <- set_BXD_names(fields, bxd = bxd)  
            
    X <- try(xml2::read_xml(xml_file))
    if (inherits(X, 'try-error')){
        stop("error reading XML")
    }

    Iters <- xml2::xml_find_all(X, 'BlastOutput_iterations/Iteration')
    if (length(Iters) == 0){
        stop("no BlastOutput_iterations children found")
    }
    
    x <- try(do.call(rbind, lapply(Iters, iter_extract, bxd = bxd)))
    
    if (!is.null(out_file) && !inherits(x, 'try-error'))
        write.table(x, file = out_file, ...)
    
    if((tolower(form) == 'tibble') && !inherits(x, 'try-error')){
        
        x <- x %>% 
            tibble::as_tibble() %>%
            lapply(type.convert, as.is = TRUE) %>% 
            tibble::as_tibble()  
    }
    
    
    invisible(x)        
}
