.PHONY: manifest
manifest:
	Rscript -e "rsconnect::writeManifest()"
