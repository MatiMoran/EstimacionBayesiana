import requests
from bs4 import BeautifulSoup as bs

def get_player_page(player_id: int):

    print(f"Fetching player with id: {player_id}")

    headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.76 Safari/537.36', "Upgrade-Insecure-Requests": "1","DNT": "1","Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8","Accept-Language": "en-US,en;q=0.5","Accept-Encoding": "gzip, deflate"}
    url = f"https://www.transfermarkt.com/a/elfmetertore/spieler/{player_id}"
    request = requests.get(url, headers=headers)
    return request

request = get_player_page(1)
soup = bs(request.content, 'html5lib') # If this line causes an error, run 'pip install html5lib' or install html5lib
print(soup.prettify())

