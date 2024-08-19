import re
import requests
from bs4 import BeautifulSoup as bs
import time
import logging
from threading import Thread

logpath = "db.csv"
logger = logging.getLogger('log')
logger.setLevel(logging.INFO)
ch = logging.FileHandler(logpath)
ch.setFormatter(logging.Formatter('%(message)s'))
logger.addHandler(ch)
first_id = 1
last_id = 100000


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

    return (player_name, pk_data[0], pk_data[1])


def get_players_data(first_id: int, last_id: int, players_last_digit_id: int):
    first_id = 1
    last_id = 100000
    
    for k in range((first_id - first_id % 10 + players_last_digit_id),last_id, 10):
        try:
            print(f"fetching player {k}")
    
            player_name, pk1, pk2 = get_player_data(k)
            logger.info(f"{player_name},{pk1},{pk2}")

            time.sleep(0.001)
        except KeyError:
            time.sleep(0.001)


threads = []
for t_id in range(0, 10):
    t = Thread(target=get_players_data, args=(first_id, last_id, t_id,))
    threads.append(t)
    t.start()

# wait for the threads to complete
for t in threads:
    t.join()

