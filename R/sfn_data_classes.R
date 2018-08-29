#### sfn_data classes definitions ####

##### sfn_data #################################################################
# set old class for S$ to recognize tbl as data.frames
setOldClass(c("tbl_time", "tbl_df", "tbl", "data.frame"))


#' S4 class for sapfluxnet site data
#'
#' Main class for storing sapfluxnet project site data and metadata
#'
#' This class allows to store all the data and metadata for a sapfluxnet site
#' in one single object, to easily work with it. See
#' \code{vignette('sfn-data-classes', package = 'sapfluxnetr')} for more info.
#'
#' @slot sapf_data A data frame with the sapf data
#'
#' @slot env_data A data frame with the env data
#'
#' @slot sapf_flags A data frame with the same dimensions of \code{sapf_data}
#'   with the flag info for each tree/TIMESTAMP combination
#'
#' @slot env_flags A data frame with the same dimensions of \code{env_data} with
#'   the flag info for each env_var/TIMESTAMP combination
#'
#' @slot si_code A character vector of length one indicating
#'   the site code
#'
#' @slot timestamp A POSIXct vector of length \code{nrow(sapf_data)} with the
#'   timestamp
#'
#' @slot solar_timestamp A POSIXct vector of length \code{nrow(sapf_data)} with
#'   the solar timestamp
#'
#' @slot site_md A data frame containing the site metadata
#'
#' @slot stand_md A data frame containing the stand metadata
#'
#' @slot species_md A data frame containing the species metadata
#'
#' @slot plant_md A data frame containing the plant metadata
#'
#' @slot env_md A data frame containing the env metadata
#'
#' @import methods
#' @export sfn_data
#' @exportClass sfn_data

sfn_data <- setClass(
  'sfn_data',
  slots = list(
    sapf_data = "data.frame",
    env_data = "data.frame",
    sapf_flags = "data.frame",
    env_flags = "data.frame",
    si_code = "character",
    timestamp = "POSIXt",
    solar_timestamp = "POSIXt",
    site_md = "data.frame",
    stand_md = "data.frame",
    species_md = "data.frame",
    plant_md = "data.frame",
    env_md = "data.frame"
  )
)

##### sfn_data_multi ###########################################################
#' S4 class for sapfluxnet multi-site data
#'
#' Multi sfn data class, derived from list
#'
#' This class inherits from \code{list}, but modified to contain sfn_data objects
#' as elements. This will allow to work with several sites at the same time
#' obtaining results for all of them combined or individually as elements of
#' the resulting list (with \code{lapply} or \code{purrr::map})
#'
#' @export sfn_data_multi
#' @exportClass sfn_data_multi

sfn_data_multi <- setClass(
  'sfn_data_multi',
  contains = 'list'
)


##### R6 sfn_data ##############################################################
#' R6 clas for sapfluxnet site data
#' 
#' @export
sfn_data_R6 <- R6::R6Class(
  'sfn_data_R6',
  public = list(
    
    # fields
    sapf_data = NULL,
    sapf_flags = NULL,
    env_data = NULL,
    env_flags = NULL,
    si_code = NULL,
    timestamp = NULL,
    solar_timestamp = NULL,
    site_md = NULL,
    stand_md = NULL,
    species_md = NULL,
    plant_md = NULL,
    env_md = NULL,
    
    # methods
    
    ## initialize ####
    initialize = function(
      sapf_data, env_data, sapf_flags, env_flags, si_code, timestamp,
      solar_timestamp, site_md, stand_md, species_md, plant_md, env_md
    ) {
      
      # we store *_data and *_flags together as an array. This way the size of
      # the obeject is reduced. But this **converts the data to character** so,
      # at the time to retrieve (get methods) we must convert to numeric again
      self$sapf_data <- sapf_data %>% 
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      self$sapf_flags <- sapf_flags %>% 
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      self$env_data <- env_data %>% 
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      self$env_flags <- env_flags %>% 
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      self$si_code <- si_code
      self$timestamp <- timestamp
      self$solar_timestamp <- solar_timestamp
      self$site_md <- site_md %>% tibble::as_tibble()
      self$stand_md <- stand_md %>% tibble::as_tibble()
      self$species_md <- species_md %>% tibble::as_tibble()
      self$plant_md <- plant_md %>% tibble::as_tibble()
      self$env_md <- env_md %>% tibble::as_tibble()
    },
    
    ## format (show) ####
    format = function(...) {
      
      temp_names <- glue::glue_collapse(
        names(self$env_data), ', ', last = ' & '
      )
      
      c(
        # site code
        glue::glue('Data from {self$si_code} site'),
        # main contrb
        glue::glue(
          'kindly provided by {self$site_md[["si_contact_firstname"]]} ',
          '{self$site_md[["si_contact_lastname"]]} from ',
          '{self$site_md[["si_contact_institution"]]}'
        ),
        # additional contrb
        if (!is.na(self$site_md[['si_addcontr_firstname']])) {
          glue::glue("and {self$site_md[['si_addcontr_firstname']]} ",
                     "{self$site_md[['si_addcontr_lastname']]}")
        },
        # paper
        glue::glue("Site related literature: {self$site_md[['si_paper']]}"),
        # number of trees
        glue::glue(
          "Sap flow data: {dim(self$sapf_data)[1]} observations of ",
          "{dim(self$sapf_data)[2]} trees/plants"
        ),
        # species
        glue::glue("Species present: {self$species_md[['sp_name']]}"),
        # env data
        glue::glue(
          "Environmental data. {dim(self$env_data)[1]} observations of the ",
          "following variables: {temp_names}"
        ),
        # biome
        glue::glue("Biome: {self$site_md[['si_biome']]}")
        
        ##TODO the rest of the old show method
      )
      
      return(invisible(self))
    },
    
    ## get methods ####
    get_sapf_data = function(solar = FALSE) {
      # data
      .sapf <- self$sapf_data
      # timestamp, depending on solar
      if (solar) {
        TIMESTAMP <- self$solar_timestamp
      } else {
        TIMESTAMP <- self$timestamp
      }
      
      return(
        cbind(TIMESTAMP, .sapf) %>% tibble::as_tibble()
      )
    },
    
    get_env_data = function(solar = FALSE) {
      # data
      .env <- self$env_data
      # timestamp, depending on solar
      if (solar) {
        TIMESTAMP <- self$solar_timestamp
      } else {
        TIMESTAMP <- self$timestamp
      }
      
      return(
        cbind(TIMESTAMP, .env) %>% tibble::as_tibble()
      )
    },
    
    get_sapf_flags = function(solar = FALSE) {
      # data
      .sapf <- self$sapf_flags
      # timestamp, depending on solar
      if (solar) {
        TIMESTAMP <- self$solar_timestamp
      } else {
        TIMESTAMP <- self$timestamp
      }
      
      return(
        cbind(TIMESTAMP, .sapf) %>% tibble::as_tibble()
      )
    },
    
    get_env_flags = function(solar = FALSE) {
      # data
      .env <- self$env_flags
      # timestamp, depending on solar
      if (solar) {
        TIMESTAMP <- self$solar_timestamp
      } else {
        TIMESTAMP <- self$timestamp
      }
      
      return(
        cbind(TIMESTAMP, .env) %>% tibble::as_tibble()
      )
    },
    
    ## set methods ####
    set_sapf_data = function(data) {
      
      # remove timestamp if any
      tmp_data <- data %>%
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      
      # check dimensions
      if (dim(tmp_data) != dim(self$sapf_data)) {
        stop('new sapf_data and old sapf_data have different dimensions')
      } else {
        # if everything ok, assign it to the field
        self$sapf_data <- tmp_data
      }
    },
    
    set_sapf_flags = function(data) {
      
      # remove timestamp if any
      tmp_flags <- data %>%
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      
      # check dimensions
      if (dim(tmp_flags) != dim(self$sapf_flags)) {
        stop('new sapf_flags and old sapf_flags have different dimensions')
      } else {
        # if everything ok, assign it to the field
        self$sapf_flags<- tmp_flags
      }
    },
    
    set_env_data = function(data) {
      
      # remove timestamp if any
      tmp_data <- data %>%
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      
      # check dimensions
      if (dim(tmp_data) != dim(self$env_data)) {
        stop('new env_data and old env_data have different dimensions')
      } else {
        # if everything ok, assign it to the field
        self$env_data <- tmp_data
      }
    },
    
    set_env_flags = function(data) {
      
      # remove timestamp if any
      tmp_flags <- data %>%
        dplyr::select(-dplyr::one_of(c('TIMESTAMP', 'timestamp')))
      
      # check dimensions
      if (dim(tmp_flags) != dim(self$env_flags)) {
        stop('new env_flags and old env_flags have different dimensions')
      } else {
        # if everything ok, assign it to the field
        self$env_flags<- tmp_flags
      }
    }
    
    
  )
)

