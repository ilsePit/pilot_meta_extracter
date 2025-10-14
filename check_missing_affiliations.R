library(rcrossref)

dois <- c('10.1177/1745691620950684', '10.1037/pspi0000504')

for(doi in dois) {
  cat('\nDOI:', doi, '\n')
  work <- cr_works(doi = doi)$data

  if(!is.null(work$author) && length(work$author) > 0) {
    authors <- work$author[[1]]

    if('affiliation.name' %in% names(authors)) {
      cat('Affiliations:\n')
      for(i in 1:min(3, nrow(authors))) {
        cat('  Author', i, ':', authors$affiliation.name[i], '\n')
      }
    } else {
      cat('  No affiliation data available\n')
    }
  } else {
    cat('  No author data available\n')
  }
}
