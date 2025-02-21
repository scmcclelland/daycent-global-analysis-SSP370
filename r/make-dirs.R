# filename:     make-dirs.R
# created:      21 February 2025
# last updated: 21 February 2025
# description:  Execute this script to create the directory structure required for the 
#               R project. Once created, downloaded data from Zenodo repo can be added
#               and unzipped
#-----------------------------------------------------------------------------------------
library(rstudioapi)
library(stringr)
m_dir = dirname(getActiveDocumentContext()$path)
m_dir = str_split(m_dir, '/r')
m_dir = m_dir[[1]][1]
setwd(m_dir) # this directory should contain the sub-directory /r and it is the main working dir

# make data directory
dir.create('data')
# the tar zip files can be added directly. Once unzipped, they will already correspond to the 
# required sub-directory

# make figure directories
dir.create('figures')
dir.create('figures/main')
dir.create('figures/ext')
dir.create('figures/si')