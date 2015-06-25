
all: db01urlspidering.db db02pageanalysis.db
	@rm -f testHXT2
	@ghc -o testHXT2 src/testHXT2.hs src/UrlCaching.hs src/WikiSpidering.hs src/MJHUri.hs src/Phase01WikipediaSpidering.hs
	@./testHXT2
	@echo "Done"

db01urlspidering.db:
	sqlite3 $@ "CREATE TABLE raw_downloads (id INTEGER PRIMARY KEY, url text, contents text);"
	sqlite3 $@ "CREATE TABLE spidering_completed (id INTEGER PRIMARY KEY, url text);"
	sqlite3 $@ "CREATE TABLE spidering_scheduled (id INTEGER PRIMARY KEY, url text);"
	sqlite3 $@ "CREATE TABLE valid_timeline_urls (id INTEGER PRIMARY KEY, url text);"
	
db02pageanalysis.db:
	sqlite3 $@ "CREATE TABLE t02_clean_urls (id INTEGER PRIMARY KEY, url text, contents text);"


	
