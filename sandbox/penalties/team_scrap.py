import re
import requests
from bs4 import BeautifulSoup as bs

def get_player_page(player_id: int):

    headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.76 Safari/537.36', "Upgrade-Insecure-Requests": "1","DNT": "1","Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8","Accept-Language": "en-US,en;q=0.5","Accept-Encoding": "gzip, deflate"}
    url = f"https://www.transfermarkt.com/a/elfmetertore/spieler/{player_id}"
    request = requests.get(url, headers=headers)
    return request

def get_player_data(player_id: int):

    request = get_player_page(player_id)
    soup = bs(request.content, 'html5lib') # If this line causes an error, run 'pip install html5lib' or install html5lib

    #name_element = soup.find('span', itemprop='birthDate', class_='data-header__content')
    #if name_element:
    #    birth_date = name_element.get_text(strip=True)
    #    print(birth_date)
    #else:
    #    print('Name element not found.')

    name_element = soup.find('h1', class_='data-header__headline-wrapper')
    if name_element is None:
        raise KeyError("Name not found")
    
    player_name =name_element.get_text(strip=True)

    pk_data = [0,0]
    pk_elements = soup.find_all('h2', class_='content-box-headline')
    for index, pk_element in enumerate(pk_elements):
        pk_element_text = pk_element.get_text(strip=True)
        pattern = r"Total penalties .+ - (\d+)"  # Matches one or more digits
        match = re.search(pattern, pk_element_text)
        if match:
            pk_data[index] = int(match.group(1))
        else:
            print("pk element text match no found")

    player_name = re.sub(r'^#?\d+', '', player_name)

    return (player_name, pk_data[0], pk_data[1])




