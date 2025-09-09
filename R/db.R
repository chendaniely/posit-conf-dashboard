db_connect <- function(env_file = NULL) {
	if (is.null(env_file)) {
		env_file <- here::here(".env")
	}

	if (file.exists(env_file)) {
		dotenv::load_dot_env(env_file)
	}

	con <- DBI::dbConnect(
		RPostgres::Postgres(),
		host = Sys.getenv("DB_HOST"),
		port = Sys.getenv("DB_PORT"),
		dbname = Sys.getenv("DB_DATABASE"),
		user = Sys.getenv("DB_USER"),
		password = Sys.getenv("DB_PASSWORD")
	)
}

get_last_response_id <- function(con = NULL) {
	if (is.null(con)) {
		con <- db_connect()
		on.exit(DBI::dbDisconnect(con))
	}

	DBI::dbGetQuery(con, "SELECT MAX(id) as id FROM responses")$id
}

get_n_responses <- function(con = NULL) {
	if (is.null(con)) {
		con <- db_connect()
		on.exit(DBI::dbDisconnect(con))
	}

	DBI::dbGetQuery(con, "SELECT COUNT(*) as n FROM responses")$n
}

get_responses <- function(con = NULL) {
	if (!is.null(getOption("local_data"))) {
		local_path <- getOption("local_data")
		if (grepl("rds$", local_path)) {
			return(readr::read_rds(local_path))
		} else {
			return(readr::read_csv(local_path))
		}
	}

	if (is.null(con)) {
		con <- db_connect()
		on.exit(DBI::dbDisconnect(con))
	}

	DBI::dbGetQuery(con, "SELECT * FROM responses")
}
