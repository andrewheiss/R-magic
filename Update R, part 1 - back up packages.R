# Create list of installed packages
tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
save(installedpkgs, file="installed_old.rda")

#---------------------------------------
# Delete old version of R
# Install new version of R
# (Or,  if you're on OS X, just run the install package. It'll uninstall and install magically)
#---------------------------------------
