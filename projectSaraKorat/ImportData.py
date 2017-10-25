########################################################
## Importing data from web
########################################################

# import libraries
import requests


# The URL of front page
numbeo_url = "https://www.numbeo.com/cost-of-living/"



# COUNTRIES

def download_front_page(numbeo_url):
    '''Downloads front page and saves it to a file as a string. '''
    r = requests.get(numbeo_url)
    open('countries.txt', 'wb').write(r.content)
    return None


def countries(page_text):
    '''Opens front page file, gets all countries and returns the list of countries.'''
    # TODO
    return


# INDEXES

## List of all indexes

def download_pages(list_of_urls):
    '''Downloads pages and saves them to a file as strings.'''
    # TODO
    return


def indexes(list_of_pages):
    ''''''
