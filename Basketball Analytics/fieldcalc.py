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
                games[my_game][player] = [None,[7200*(my_period-1)]] #none is placeholder for team!
    elif play2[2] == "8":
        p1 = play2[11]
        p2 = play2[12]
        tim = 7200-int(play2[5])+7200*(my_period-1)
        if games[my_game].get(p1, False):
            games[my_game][p1][1] += [tim]
        else:
            games[my_game][p1] = [None, [tim]]
        if games[my_game].get(p2, False):
            games[my_game][p2][1] += [tim]
        else:
            games[my_game][p2] = [None, [tim]]
        #if(("377f46" in p1 or "377f46" in p2) and "fac11e" in play2[0]):
            #print(p1, games[my_game][p1])
         #   print(p2, games[my_game][p2])
    cur_period = my_period
#essentially each pair of numbers corresponds to an interval of time when the player is on the court during a game
#2nd pass
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
    print(play2[5], on_court)
    
