
# This file defines common variables

username = tolower(Sys.getenv('USERNAME'))
if( username == "") {
  username = tolower(Sys.getenv('USER'))
}

nlsy_data_dir = '~/../Box/Baby Bonds Microsimulation Modeling-Rockefeller/Data/NLSY'

# If the Box path is different on your computer, define it here

# Windows
if(username == 'dcosic') {
  nlsy_data_dir = '~/../Box/Baby Bonds Microsimulation Modeling-Rockefeller/Data/NLSY'
} else if(username == 'damircosic') {
  nlsy_data_dir = '~/Library/CloudStorage/Box-Box/Baby Bonds Microsimulation Modeling-Rockefeller/Data/NLSY'
}