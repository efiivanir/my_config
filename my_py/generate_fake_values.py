import sys
from faker import Faker
import random
from datetime import datetime
from dateutil.relativedelta import relativedelta
import re
import glob, os, os.path

filelist = glob.glob(os.path.join('.', "*.csv"))
for f in filelist:
    os.remove(f)

iso_format = "{Year}-{Month}-{Day} {Hour}:{Minute}"
month_range = [
    "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"
]
day_range = [str(i).zfill(2) for i in range(1, 28)]
hour_range = [str(i).zfill(2) for i in range(1,24)]
min_range = [str(i).zfill(2) for i in range(1, 60)]

bet_range = range(100,10000,100)
dealer_relations_type = ['parent','grand','brother','child','nephew','uncle']

game_dict = {}
games_list = []
dealers_list = []
game_dict['Poker'] = ['Card game', 2, 12]
game_dict['Remi'] = ['Card game', 2, 20]
game_dict['Black Jeck'] = ['Machine game', 1, 1]
game_dict['Bridge'] = ['Card Game', 4, 4]

sys.path.insert(0, '/root/ivanir_py/Names')
sys.path.insert(0, '/root/ivanir_py')

from Names import *
from R_Ids import *

num_of_dealers = 10
num_of_players = 100
num_of_lines = num_of_dealers + num_of_players
dealers_ids = list(range(1000,1000 + num_of_dealers))
games_length_range = list(range(1,5))
num_of_games = 20

mysql_db_root_dir = '/root/technion_db/final_exam'
dealer_f = mysql_db_root_dir + "/" + 'Dealer.csv'
player_f = mysql_db_root_dir + "/" + 'Player.csv'
game_type_f = mysql_db_root_dir + "/" + 'GameType.csv'
game_f = mysql_db_root_dir + "/" + 'Game.csv'
player_in_game_f = mysql_db_root_dir + "/" + 'PlayerInGame.csv'
player_dealer_relation_f = mysql_db_root_dir + "/" + 'PlayerDealerRelation.csv'

s = Names()

first_names = s.random_first_names(num_of_lines)
last_names = s.random_last_names(num_of_lines)

r = R_Ids()
ids = r.random_ids(num_of_lines)

##############################################
# Create Dealer
##############################################
with open(dealer_f, "w") as f:
    iso_format = "{Year}-{Month}-{Day}"
    work_id = 1000
    for i in range(num_of_dealers):
        year_range = [str(i) for i in range(1930, 1992)]
        argz = {
            "Year": random.choice(year_range),
            "Month": random.choice(month_range),
            "Day": random.choice(day_range),
        }

        d = iso_format.format(**argz)
        year_range = [str(i) for i in range(2016, 2018)]
        argz = {
            "Year": random.choice(year_range),
            "Month": random.choice(month_range),
            "Day": random.choice(day_range),
        }
        
        w = iso_format.format(**argz)

        line = str(work_id) + "," + last_names[i] + " " + first_names[i] + "," + d + "," + w + "," + ids[i] + "\n"
        f.write(line)
        dealers_list.append(work_id)
        work_id += 1
f.close
##############################################
# Game type
##############################################
with open(game_type_f, "w") as f:
    for x, y in game_dict.items():
        line = x + "," + ",".join(map(str, y)) + "\n"
        f.write(line)
f.close

##############################################
# Game 
##############################################
with open(game_f, "w") as f:
    iso_format = "{Year}-{Month}-{Day} {Hour}:{Minute}"
    start_range = [str(i) for i in range(2018,2019)]
    for i in range(num_of_games):
        dealer_id = random.choice(dealers_ids)
        game = random.choice(list(game_dict.keys()))

        argz = {
            "Year": random.choice(start_range),
            "Month": random.choice(month_range),
            "Day": random.choice(day_range),
            "Hour": random.choice(hour_range),
            "Minute": random.choice(min_range),
        }
        start_time = iso_format.format(**argz)
        tmp = datetime.strptime(start_time, "%Y-%m-%d %H:%M")
        game_length = random.choice(games_length_range);
        end_time = tmp + relativedelta(hours=game_length)
        end_time = str(end_time)
        end_time = re.sub('\:00$', '', end_time)
        line = game + "," + start_time + "," + end_time + "," + str(dealer_id) + "\n";
        games_list.append([game,start_time])
        f.write(line) 
f.close


##############################################
# Create Player
##############################################
current_players = {}
with open(player_f, "w") as f:
    iso_format = "{Year}-{Month}-{Day}"
    for i in range(num_of_dealers - 1, num_of_lines - 1):
        fake = Faker()
        city = fake.city()
        prefered_game = random.choice(list(game_dict.keys()))
        year_range = [str(i) for i in range(1930, 1993)]
        argz = {
            "Year": random.choice(year_range),
            "Month": random.choice(month_range),
            "Day": random.choice(day_range),
        }

        d = iso_format.format(**argz)
        line = ids[i] + "," + first_names[i] + "," + last_names[i] + "," + city + "," + d + "," + prefered_game + "\n"
        f.write(line)
        current_players[ids[i]] = 1
f.close

##############################################
# Create PlayerInGame
##############################################
with open(player_in_game_f, "w") as f:
    for i in games_list:
        game_type = i[0]
        game_start_date = i[1]
        game_min_gamers = game_dict[game_type][1]
        game_max_gamers = game_dict[game_type][2]
        if game_min_gamers == game_max_gamers:
            num_of_gamers = game_min_gamers
        else:
            num_of_gamers = random.sample(range(game_min_gamers,game_max_gamers),1)[0]
        game_players = random.sample(list(current_players.keys()),num_of_gamers)
        for j in game_players:
            bet_amount = random.sample(bet_range,1)[0]
            loose_option = bet_amount * -1
            win_option = random.sample(range(0,bet_amount,100),1)[0]
            win_loos = random.sample([loose_option,win_option],1)[0]
            line = ",".join(map(str,[j,game_type,game_start_date,bet_amount,win_loos])) + "\n"                         
            f.write(line)
f.close
##############################################
# Create PlayerDealerRelation
##############################################
with open(player_dealer_relation_f, "w") as f:
    for i in dealers_list:
        connection = random.sample(list(current_players.keys()),1)[0]
        connection_type = random.sample(dealer_relations_type,1)[0]
        line = ",".join(map(str,[connection,i,connection_type])) + "\n"  
        f.write(line)
f.close
