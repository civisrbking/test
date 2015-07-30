loadData <- function(username,password) {
  library(DBI)
  library(RPostgreSQL)
  
  # Get connection
  redshift <- dbConnect(PostgreSQL(), 
                        user = username,
                        password = password,
                        host = "redshift-general.tgtg", # general civis cluster
                        dbname = "dev")
  
  query <- paste0("SELECT tsa.state_code
                ,CASE 
                  WHEN base.vote_g2012 = 1 AND base.vote_g2014 = 1 
                    THEN 'Habitual'
                  WHEN base.vote_g2012 = 1 AND base.vote_g2014 = 0 
                    THEN 'Sporadic'
                  WHEN base.vote_g2012 = 0 AND base.vote_g2014 = 0 
                    THEN 'Nonvoter'
                  WHEN base.vote_g2012 = 0 AND base.vote_g2014 = 1 
                    THEN 'Other'
                  ELSE 'Does.this.exist' -- In case I missed one
                  END AS voter_type
                ,ROUND(tsa.turnout_2016,2) AS turnout_2016_bins
                ,COUNT(*)
                FROM dnc.turnout_scores_2016_adjusted tsa
                  JOIN sampling.dnc_sampling_basefile base
                  ON tsa.personid = base.personid
                GROUP BY tsa.state_code, voter_type, turnout_2016_bins
                ORDER BY tsa.state_code, voter_type, turnout_2016_bins")
  
  dat <- dbGetQuery(redshift, query) 
  dbDisconnect(redshift)
  return(dat)
}