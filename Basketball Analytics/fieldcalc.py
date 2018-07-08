#Calculates what player is on the field at the time each event occurs

#list of games
with open("Jakob-lineup.csv","r") as lineup:
    foo = lineup.readlines()[1:]
games = {game:{} for game in list(set([line.split(",")[0] for line in foo]))}
with open("Jakob-pbp.csv","r") as pbp:
    bar = pbp.readlines()[1:]

cur_game = None
cur_period = 0
ct = 0
for play in bar:
    ct+=1
    play2 = play.split(",")
    my_game = play2[0]
    if (cur_game != my_game):
        if cur_game:
            for key in list(games[cur_game].keys()):
                if len(games[cur_game][key][1])%2:
                    games[cur_game][key][1] += [28800]
        cur_game = my_game
    my_period = int(play2[3])
    if my_period != cur_period or ct==len(bar):
        for key in list(games[my_game].keys()):
            if(len(games[my_game][key][1])%2):
                games[my_game][key][1] += [7200*cur_period]
        lineup_filtered = list(map(lambda y:y.split(",")[2], list(filter(lambda x:x.split(",")[0]==my_game and x.split(",")[1]==str(my_period), foo))))
        for player in lineup_filtered:
            if games[my_game].get(player,False):
                games[my_game][player][1] += [7200*(my_period-1)]
            else:
                games[my_game][player] = [play2[10],[7200*(my_period-1)], 0] #play2[10] may not be accurate. who knows???
    elif play2[2] == "8":
        p1 = play2[11]
        p2 = play2[12]
        tim = 7200-int(play2[5])+7200*(my_period-1)
        if games[my_game].get(p1, False):
            games[my_game][p1][1] += [tim]
        else:
            games[my_game][p1] = [play2[10], [tim], 0]
        if games[my_game].get(p2, False):
            games[my_game][p2][1] += [tim]
        else:
            games[my_game][p2] = [play2[10], [tim], 0]
    cur_period = my_period
#essentially each pair of numbers corresponds to an interval of time when the player is on the court during a game
#2nd pass
#adds +/- rating to each player on a per-game basis
for play in bar:
    play2 = play.split(",")
    my_game = play2[0]
    tim = 7200-int(play2[5])+(int(play2[3])-1)*7200
    on_court = []
    for key in list(games[my_game].keys()):
        for i in range(len(games[my_game][key][1])//2):
            t1 = games[my_game][key][1][i*2]
            t2 = games[my_game][key][1][i*2+1]
            if t1 < tim <= t2:
                on_court += [key]
    for player in on_court:
        if games[my_game][player][0] == play2[10]:
            games[my_game][player][2] += int(play2[7])
        else:
            games[my_game][player][2] -= int(play2[7])
print(games)
