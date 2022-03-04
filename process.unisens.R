#' Process and extract ECG data from the XMLfile exported from Movisens 
#' SensorManager.
#' 
#' @param file The path of the XML file exported from SensorManager.
#' @return A data frame with the recording information.
#' @examples 
#' process.unisens('unisens.xml')

process.unisens <- function(file) {
  if (endsWith(file, '.xml')) {
    unisens <- xml2::read_xml(file)
  } else {
    stop('Invalid file type. Input file must be of .xml format.')
  }
  
  info <- data.frame(matrix())
  
  # Get the recording start time
  options(digits.secs = 3)
  timestampStart <- paste(
    unlist(
      strsplit(
        xml2::xml_attr(unisens, 'timestampStart'), 
        'T')), 
    collapse = ' ')
  info <- dplyr::transmute(
    info, 
    start = as.POSIXct(timestampStart, format = '%Y-%m-%d %H:%M:%OS'))
  
  # Get XML nodes
  children <- xml2::xml_children(unisens)
  signals <- children[xml2::xml_name(children) == 'signalEntry']
  attributes <- xml2::xml_children(
    children[xml2::xml_name(children) == 'customAttributes'])
  
  # Get ECG recording info
  for (s in signals) {
    if (xml_attr(s, 'comment') == 'ecg') {
      info$fs <- as.numeric(xml_attr(s, 'sampleRate'))
    }
  }
  
  # Get general recording info
  for (a in attributes) {
    if (xml_attr(a, 'key') == 'sensorLocation') {
      info$loc <- xml_attr(a, 'value')
    }
    if (xml_attr(a, 'key') == 'personId') {
      info$id <- xml_attr(a, 'value')
    }
    if (xml_attr(a, 'key') == 'gender') {
      info$gender <- xml_attr(a, 'value')
    }
    if (xml_attr(a, 'key') == 'age') {
      info$age <- xml_attr(a, 'value')
    }
    if (xml_attr(a, 'key') == 'weight') {
      info$weight <- xml_attr(a, 'value')
    }
    if (xml_attr(a, 'key') == 'height') {
      info$height <- xml_attr(a, 'value')
    }
  }
  
  # Check for missing fields
  if (!'start' %in% colnames(info)) {
    info$start <- NA
  }
  if (!'fs' %in% colnames(info)) {
    info$fs <- NA
  }
  if (!'id' %in% colnames(info)) {
    info$id <- NA
  }
  if (!'loc' %in% colnames(info)) {
    info$loc <- NA
  }
  if (!'gender' %in% colnames(info)) {
    info$gender <- NA
  }
  if (!'age' %in% colnames(info)) {
    info$age <- NA
  }
  if (!'height' %in% colnames(info)) {
    info$height <- NA
  }
  if (!'weight' %in% colnames(info)) {
    info$weight <- NA
  }
  
  # Rearrange columns of data frame
  vars <- c('start', 'fs', 'id', 'loc', 'gender', 'age', 'height', 'weight')
  info <- info[, vars]
  
  return(info)
}
