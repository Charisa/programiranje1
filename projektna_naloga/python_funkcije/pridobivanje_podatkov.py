########################################################################
# Importing libraries
########################################################################

import pandas as pd
from bs4 import BeautifulSoup
import os.path
import numpy as np
import re
import requests
import fileinput


########################################################################
# Getting data for bitcoin, ether and litecoin
# from web (data from the 28. 4. 2013 to 31. 12. 2017).
########################################################################


# the URL of bitcoin data 
bitcoin_url = "https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130428&end=20171231"
# the URL of ETH data
eth_url = "https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20130428&end=20171231"
# the URL of Litecoin data
litecoin_url = "https://coinmarketcap.com/currencies/litecoin/historical-data/?start=20130428&end=20171231"

# folder in which we save data
folder = "podatki"

# the filenames for the CSV files for the extracted data
bitcoin = "bitcoin.csv"
eth = "ether.csv"
litecoin = "litecoin.csv"

# list of cryptocurrencies and their data urls we will use to analyze
crypto_list = ["bitcoin", "ether", "litecoin"]
url_list = [bitcoin_url, eth_url, litecoin_url]


def download_data_to_csv(crypto_list, url_list):
    '''This function creates a directory in which we save our data.
    Then downloads data of given cryptocurrency and save it to the CSV files.
    If data is already downloaded, then pass.'''
    if os.path.isdir("../" + folder):
        pass
    else:
        os.mkdir("../" + folder)
        
    for crypto in crypto_list:
        if os.path.isfile("../" + folder + "/" + crypto + ".csv"):
            pass
        else:
            data, = pd.read_html(url_list[crypto_list.index(crypto)], header = 0, parse_dates = ["Date"])
            data.to_csv("../" + folder + "/" + crypto + ".csv", index = False)
    return

download_data_to_csv(crypto_list, url_list)


########################################################################
# Getting data for cryptocurrencies 
########################################################################


# the URL of page with a list of all cryptocurrencies
list_url = "https://coinmarketcap.com/all/views/all/"
columns = ["#","Name", "Symbol", "MarketCap", "Price", "CirculatingSupply", "Volume24h", "%1h", "%24h", "%7d"]
columns_number = ['Market Cap', 'Price', "Circulating Supply", "Volume (24h)", "% 1h", "% 24h", "% 7d"]


def all_cryptos(list_url, col, col_num):
    '''This function gets a list of all cryptocurrencies from the web (not from string file).'''
    if os.path.isdir("../" + folder):
        pass
    else:
        os.mkdir("../" + folder)
    if os.path.isfile("../" + folder + "/vse_kriptovalute.csv"):
        return
    else:
        pd.set_option('float_format', '{:f}'.format)    
        pd.options.display.float_format = "{:.2f}".format
        data, = pd.read_html(list_url, header = 0)
        data = data.replace(["\*", "\$", "\%", ","], "", regex = True)
        data = data.replace(["\?", "Low Vol"], np.NaN, regex = True)
        data[col_num] = data[col_num].apply(pd.to_numeric)
        data.to_csv("../" + folder + "/vse_kriptovalute.csv", index = False, header = col)
    return
    
shranjene_kriptovalute = all_cryptos(list_url, columns, columns_number)



def save_names(minimum, replace):
    '''This function gets a list of cryptocurrencies that satisfied one condition:
    volume on 31st of December in 2017 was more than 15.000.'''
    # getting right data
    data_crypto = pd.read_csv('../podatki/vse_kriptovalute.csv', index_col = ['#'])
    crypto_sort = data_crypto[data_crypto['Volume24h'] >= minimum]
    crypto_sort = crypto_sort.sort_values(by = ['Volume24h'], ascending = False)
    # saving names of cryptocurrencies in crypto_sort in a list
    cryptos = []
    for row in crypto_sort['Name'].iteritems():
        index, data = row
        cryptos.append(data)
    # replacing spaces with "-"
    new = []
    for crypto in cryptos:
        name = re.sub('\S+\s\s', '', crypto)
        if replace == "yes":
            if " " in name:
                new.append((name.replace(" ", "-")))
            else:
                new.append(name)
        else:
            new.append(name)
    return new
  

list_cryptos = save_names(15000, "no")

##### Two functions for saving html as a string (for finding right urls of cryptocurrencies).

columns = ["Date", "Open", "High", "Low", "Close", "Volume", "MarketCap", "Name"]


def download_to_string(url):
    '''This function downloads an url and saves it as a string.'''
    try: 
        # some code here that may raise an exception
        r = requests.get(url)
    except:
        return 
    return r.text

string_file = download_to_string(list_url)
# replacing "[" and "]" with ""
string_file = string_file.replace("[", "")
string_file = string_file.replace("]", "")
string_file = string_file.replace("(", "")
string_file = string_file.replace(")", "")
string_file = string_file.replace("'", "&#39;")


filename = "string_all_data.txt"

def save_string_to_file(text, filename):
    '''Write "text" to the file "filename".'''
    if os.path.isfile(filename):
        return 
    else:
        with open(filename, 'w', encoding = 'utf-8') as file_out:
            file_out.write(text)
    return 

string = save_string_to_file(string_file, filename)



def all_data(col, list_cryptos, text):
    '''For cryptocurrencies in a given list function gets
    all the data from the web for year 2017 and returns a csv file.'''
    # creating empty dataframe with columns
    print(len(list_cryptos))
    dataframe = pd.DataFrame()
    count = 0
    # getting data of cryptocurrencies in the list from web
    for crypto in list_cryptos:
        print(crypto)
        count += 1
        print(count)
        if "  Money" in crypto:
                crypto = "Money"
        u = (r'<a class="currency-name-container" href=".*?">\.?\s*?' + '{}' + r'\s?.?</a>').format(crypto)
        print(u)
        if "HempCoin" in crypto:
            pass
        else:
            if "[" in u:
                u = u.replace("[", "")
            if "]" in u:
                u = u.replace("]", "")
            if "(" in u:
                u = u.replace("(", "")
            if ")" in u:
                u = u.replace(")", "")
            if "'" in u:
                u = u.replace("'", "&#39;")
            ref = re.findall(u, text)
            print(ref)
            c = (re.findall(r'".*?"' , ref[0]))[1]
            c = c.replace("\"", "")
            url = "https://coinmarketcap.com" + c + "historical-data/?start=20170101&end=20171231"
            #print(url)
            try:
                data, = pd.read_html(url, header = 0)
                data["Name"] =  crypto
                dataframe = dataframe.append(data)
            except:
                print(url)
    dataframe.to_csv("../" + folder + "/kriptovalute_max_volume.csv", index = False, header = col)
    return 
    
# data = all_data(columns, list_cryptos, string_file)


