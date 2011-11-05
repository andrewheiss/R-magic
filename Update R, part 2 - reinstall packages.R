#---------------------------------------
# Delete old version of R
# Install new version of R
# (Or,  if you're on OS X, just run the install package. It'll uninstall and install magically)
#---------------------------------------

# Reinstall previously installed packages
load("installed_old.rda")
tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()