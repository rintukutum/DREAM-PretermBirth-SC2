# Load required libraries
import synapseclient
import synapseutils
# login to Synapse
syn = synapseclient.login(
  email='', # your synapse email id
  password='' # your password
) 
files = synapseutils.syncFromSynapse(
  syn, entity = 'syn18507661', path = './data/'
)
  
files = synapseutils.syncFromSynapse(
  syn, entity = 'syn20632048', path = './data/'
)
