## companieshr
#### A R Package API wrapper for Companies House API

Currently the package offers easy access to 4 Companies House API end-points returning data-frames as a result. The end-points are:
* 'company_profile' (https://developer.companieshouse.gov.uk/api/docs/company/company_number/readCompanyProfile.html)
* 'persons_significant_control' (https://developer.companieshouse.gov.uk/api/docs/company/company_number/persons-with-significant-control/listPersonsWithSignificantControl.html)
* 'officers' (https://developer.companieshouse.gov.uk/api/docs/company/company_number/officers/officerList.html)
* 'charges' (https://developer.companieshouse.gov.uk/api/docs/company/company_number/charges/getChargeList.html)

More end-points will be added soon.

### Authentication 

To generate one please go to https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/apikey_authorisation.html and follow the instructions.

### Example:

companies_profile = companieshr::companies_house_collect(companies = head(companieshr::companies, 10),
                                                         api_end_point = "company_profile", 
                                                         auth_api_key = "your_key",
                                                         time_to_rest = 3, 
                                                         join = FALSE, 
                                                         verbose = TRUE
                                                        )

### Arguments

* companies - It takes a data.frame with at least one column called "CompanyNumber" (it accepts a vector of CompanyNumbers as well). The package includes a Companies House extract for testing (companieshr::companies).
* api_end_point - A character value describing which end-point to call. Can be 'company_profile', 'persons_significant_control', 'officers' or 'charges'. Defaults to 'company_profile'.
* auth_api_key - Your key accessed from https://developer.companieshouse.gov.uk/api/docs/index/gettingStarted/apikey_authorisation.html 
* time_to_rest - Allows the user to control the time (in seconds) between retries when 429 response codes (Too Many Requests) are returned.
* join - This argument (bollean) allows the user to decide if it wants the response from the API to be joined with the 'companies' (if it is a data-frame) parsed or not.
* verbose - To return print messages about each inidividual record collected or not.
                                       
### Output

A list with two values is returned:

* **results_df** A data-frame with the results returned from the selected Companies House API's end-point.
* **errorLogs**  In case some CompanyNumbers failed to return values either because of errors or because there was no results to be returned, these can be explored via errorLogs.


