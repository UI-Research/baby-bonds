
# This file defines common variables

username = tolower(Sys.getenv('USERNAME'))

nlsy_data_dir = '~/../Box/Baby Bonds Microsimulation Modeling-Rockefeller/Data/NLSY'

# If the Box path is different on your computer, define it here
if(username == 'storresrod') {
  nlsy_data_dir = '~/../Box/Baby Bonds Microsimulation Modeling-Rockefeller/Data/NLSY'
}