import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import logging
import time
from threading import Thread
from team_scrap import *

logpath = "players_penalties.csv"
logger = logging.getLogger('log')
logger.setLevel(logging.INFO)
formatter = logging.Formatter('%(message)s')

file_handler = logging.FileHandler(logpath)
file_handler.setFormatter(formatter)
logger.addHandler(file_handler)

console_handler = logging.StreamHandler()
console_handler.setFormatter(formatter)
logger.addHandler(console_handler)

player_df = pd.read_csv('../kaggle/players.csv')
player_df = player_df[player_df['last_season'] >= 2023]
player_df = player_df[player_df['current_club_domestic_competition_id'].isin(['GB1', 'ES1', 'IT1', 'L1'])] # only premier, la liga serie a and bundesliga
player_df = player_df[player_df['position'].isin(['Midfield', 'Attack'])]
player_df

def get_players_data(players_ids: list[int]):
    for player_id in players_ids:
        try:
            player_name, pk1, pk2 = get_player_data(player_id)
            logger.info(f"{player_name},{pk1},{pk2}")
            time.sleep(0.001)
        except KeyError:
            time.sleep(0.001)

def main():
    players_ids = player_df["player_id"].to_list()
    sublists = [players_ids[i:i + len(players_ids) // 10] for i in range(0, len(players_ids), len(players_ids) // 10)]

    threads = []
    for sublist in sublists:
        t = Thread(target=get_players_data, args=(sublist,))
        threads.append(t)
        t.start()

    # wait for the threads to complete
    for t in threads:
        t.join()

if __name__ == '__main__':
    main()