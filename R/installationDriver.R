#' installationDriver
#' 
#' @import vprint
#' 
#' @description
#' determine missing dependencies and install them, packs do not need
#'  to be installed or loaded
#' 
#' @param dir character string containing the name of the directory holding packs
#' @param packs list of package names
#' @param reportPacks Boolean if FALSE do not return packages in parameter packs even if missing  
#' @param verbose integer passed to vprint()
#' 
#' @examples
#' \dontrun{
#' # you need to specify dir, packs that are on your own computer !!
#' # purposely remove a required library to make install go through its paces !!
#' remove.packages("hash")
#' dir1<-"~/personal/hearts/hearts_card_game_bayesian_inference"
#' dir2<-"packages/inference_packages/inference_packages/"
#' dir<-sprintf("%s/%s",dir1,dir2)
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' missing<-installationDriver(dir,packs,reportPacks=FALSE,verbose=c(-1,4))
#' }
#' 
#' @details
#' the return value 'missing' is a Boolean vector whose components are the names of missing dependencies \cr
#' a value of FALSE means that the missing dependency is no longer missing because it was successfully installed
#' 
#' @return returns a Boolean vector confirming installation of missing dependencies, also has side effect of installing some or all missing dependencies
#'  
#' @export
installationDriver<-
	function(dir,packs,reportPacks=FALSE,verbose=4) {
		rns<-retrieveNamespace2(dir,packs)
		urns<-unique(unlist(rns))

		# Filter out standard base R packages (if desired)
		base_pkgs <- c("base", "compiler", "datasets", "grDevices", "graphics", "grid", "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
		imported_packages <- setdiff(urns, base_pkgs)

		# Retrieve recursive dependencies
		# Use which = c("Depends", "Imports", "LinkingTo") to grab all required package types
		
		# Define your preferred CRAN mirror URL
		mirror_url <- "https://cloud.r-project.org"
		
		# Retrieve the package database from that specific mirror
		db <- utils::available.packages(repos = mirror_url)
		
		recursive_deps <- tools::package_dependencies(
			packages = imported_packages, 
			recursive = TRUE,
			which = c("Depends", "Imports", "LinkingTo"),
			db<-db
		)

		# View the full list of recursive dependencies
		all_recursive_deps <- unique(unlist(recursive_deps))

		all_deps<-unique(union(imported_packages,all_recursive_deps))

		# Check which dependencies are missing from your installed packages
	
		missing_deps<-setdiff(all_deps, rownames(utils::installed.packages(.libPaths())))
		if(!reportPacks)
		  missing_deps<-setdiff(missing_deps,packs)
		
		vprint(-1,verbose,"rns:",style="red")
		vprint0(-1,verbose,rns)
		vprint(-1,verbose,"urns:",style="red")
		vprint0(-1,verbose,urns)
		vprint(-1,verbose,"imported_packages:",style="red")
		vprint0(-1,verbose,imported_packages)
		vprint(-1,verbose,"recursive_deps:",style="red")
		vprint0(-1,verbose,recursive_deps)
		vprint(-1,verbose,"all_deps:",style="red")
		vprint0(-1,verbose,all_deps)
		vprint(-1,verbose,"missing_deps:",style="red")
		if(length(missing_deps)>0)
		  vprint0(-1,verbose,missing_deps)
		
		# Print the result
		recheck<-TRUE
		if (length(missing_deps) == 0) {
		  vprint(4,verbose,"All required packages are installed!")
		    return(character(0))
		} else {
		  vprint(4,verbose,"Missing dependencies found: ")
		  vprint(4,verbose,missing_deps)

			# Install the missing ones
  		v<-vector("logical",length(missing_deps))
  		names(v)<-missing_deps
			vprint(4,verbose,"Installing missing dependencies . . . ")
  		for(missing_dep in missing_deps) {
  		  installReturnStr<-utils::capture.output(utils::install.packages(missing_dep,verbose=TRUE),type="message")
  		  vprint(4,verbose,"return code")
  		  vprint(4,verbose,installReturnStr)
  		  v[missing_dep]<-identical(installReturnStr, character(0))
  		}
		}
		return(v)
	}

#' retrieveNamespace2
#' 
#' @description
#' retrieve a list of the imported packages in a NAMESPACE FILE, packs do not need
#'  to be installed or loaded
#' 
#' @param dir character string containing the name of the directory holding packs
#' @param packs list of package names
#' 
#' @examples
#' \dontrun{
#' # you need to specify dir, packs that are on your own computer !!
#' dir1<-"~/personal/hearts/hearts_card_game_bayesian_inference"
#' dir2<-"packages/inference_packages/inference_packages/"
#' dir<-sprintf("%s/%s",dir1,dir2)
#' packs<-c("cardUtils","clickableImageMap","editDriver",
#' "heartsCIM","iterationDriver","logos","playOneTrick",
#' "playWholeHandDriverPassParams","probTab","relaxDriver")
#' rns<-retrieveNamespace2(dir,packs)
#' }
#' 
#' @return returns a list containing the imported package names
#'  
#' @export
retrieveNamespace2<-
  function(dir,packs) {
    l<-list()
    for(pack in packs) {
      f<-sprintf("%s/%s/%s",dir,pack,"NAMESPACE")
      if(!file.exists(f))
        stop("retrieveNamespace missing: ",f)
      NAMESPACE<-readLines(f)
      g<-grep("^import\\(",NAMESPACE)
      x<-NAMESPACE[g]
      ss<-substr(x,8,nchar(x)-1)
      l[[pack]]<-ss
    }
  return(l)
  }