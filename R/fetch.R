#' Given a URL, fectch and download to a specified path the data as TIFF.
#' 
#' Subsetting by bounding box is optional
#' 
#' @export
#' @param x url-database or tidync object with database ("db") attribute
#' @param bb NULL or object from which [sf::st_bbox] can be extracted.
#' @param path chr, the root data path 
#' @return a file database
fetch_obpg = function(x = obpg_url(),
                      bb = NULL,
                      root = "."){
  if (!inherits(x, "tidync")) x = open_obpg(x)
  if (!is.null(bb)) x = crop_obpg(x, bb)
  s = as_stars(x)
  db = attr(x, "db")
  ofile = compose_filename(db, path = root)
  ok = make_path(dirname(ofile))
  s = stars::write_stars(s, ofile)
  return(db)
}