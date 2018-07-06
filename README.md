## companieshr
#### An API wrapper for Companies House API

The package is under development. Currently only access to the https://api.companieshouse.gov.uk/company API end-point.

### Authentication 

To generate one please go to https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/apikey_authorisation.html and follow the instructions.

### Example:

response_list = companieshr::companies_profile_collect(companies_df = df_file_of_the_day,
                                                                       auth_api_key = companies_house_api_key_arg,
                                                                       time_to_rest = 3,
                                                                       join = FALSE)

### Arguments

* companies_df - It takes a data.frame with at least one column called "CompanyNumber" (it accepts a vector of CompanyNumbers as well). The package includes a Companies House extract for testing (companieshr::companies).
* time_to_rest - Allows the user to control the time (in seconds) between retries when 429 response codes (Too Many Requests) are returned.
* join - This argument (bollean) allows the user to decide if it wants the response from the API to be joined with the companies_df parsed or not.
                                       
### Output

Returns the output of the companies hourse "/company" end point call as a data.frame together with a log of missed responses (result is a list with this two objects). 
