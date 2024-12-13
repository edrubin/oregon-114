# Notes ----------------------------------------------------------------------------------
#   Goal:   Define groups of states (e.g., permit-to-purchase, point-of-contact, etc.)
#   Note:   Used by other scripts; does not produce any output.

# Define sets of states ------------------------------------------------------------------
  # States to always drop due to coincident changes in laws/data
  drop_states = c('Alabama', 'North Carolina')
  # Permit-to-purchase states
  p2p_states = c(
    'California', 'Connecticut', 'Hawaii', 'Illinois', 'Massachusetts',
    'New Jersey', 'Minnesota', 'Maryland', 'Michigan', 'Nebraska',
    'New York', 'North Carolina', 'Rhode Island', #'Washington, D.C.',
    'Missouri'
  )
  # Point-of-contact states
  poc_states = c(
    'California', 'Colorado', 'Connecticut', 'Florida', #'Hawaii',
    'Illinois', 'Nevada', 'New Jersey', 'New York', 'Pennsylvania',
    'Tennessee', 'Utah', 'Virginia', 'Washington',
    'Maryland', 'Wisconsin', 'Nebraska', 'New Hampshire'
  )
  # States that border Oregon
  border_states = c('California', 'Washington', 'Idaho', 'Nevada')
  # Brady-exempt states (permits replace background checks)
  # https://www.atf.gov/rules-and-regulations/permanent-brady-permit-chart
  brady_states = c(
    'Arizona', 'Arkansas', 'Georgia', 'Idaho', 'Kansas', 'Kentucky', 'Louisiana',
    'Michigan', 'Montana', 'Nebraska', 'Nevada', 'North Carolina', 'North Dakota',
    'South Carolina', 'South Dakota', 'Texas', 'Utah', 'West Virginia', 'Wyoming',
    'Iowa', 'Mississippi', 'Ohio'
  )
