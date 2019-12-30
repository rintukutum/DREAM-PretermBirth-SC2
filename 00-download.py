# Load required libraries
import synapseclient
import synapseutils
# login to Synapse
syn = synapseclient.login(
  email='rintukutum@gmail.com', # your synapse email id
  password='gp41@Synapse' # your password
) 
files = synapseutils.syncFromSynapse(
  syn, entity = 'syn18507661', path = './data/'
  )
  
files = synapseutils.syncFromSynapse(
  syn, entity = 'syn20632048', path = './data/'
  )
