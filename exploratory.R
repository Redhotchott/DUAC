#Here is how to send queries to the database, then return the query to the R workspace as a dataframe object: 
conectar <- function(){
  library(RPostgreSQL)
	drv <- dbDriver("PostgreSQL")
	u <- "ceberlco"
	d <- "csci403"
	h <- "flowers.mines.edu"
	p <- .rs.askForPassword("DBpass?")
	con <- dbConnect(drv,user=u,dbname=d,host=h,password=p)
	return(con)
} 

connection <- conectar()
rs <- dbSendQuery(connection,statement=paste("YOUR SQL QUERY GOES HERE, WITHOUT SEMICOLON AT END"))
data <- fetch(rs, n=-1)
#n=-1 above refers to the option to return ALL of the tuples of the SQL query result
