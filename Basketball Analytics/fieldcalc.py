#list of games
with open("Jakob-lineup.csv","r") as lineup:
    foo = lineup.readlines()[1:]
#Game ids are keys, values are dicts that map players to the times they are on the court    
games = {game:{} for game in list(set([line.split(",")[0] for line in foo]))}

#list of plays
with open("Jakob-pbp.csv","r") as pbp:
    bar = pbp.readlines()[1:]

cur_game = None
cur_period = 0
ct = 0 #nuber of plays
for play in bar:
    ct+=1
    play2 = play.split(",")
    my_game = play2[0]
    #anyone who is still on court at the end of the game has their period ended at 2880.0 sec (end of game)
    if (cur_game != my_game):
        if cur_game:
            for key in list(games[cur_game].keys()):
                if len(games[cur_game][key][1])%2:
                    games[cur_game][key][1] += [28800]
        cur_game = my_game
    my_period = int(play2[3])
    #edge case for when we enter a new period
    if my_period != cur_period or ct==len(bar):
        #if the period ends, then so does a player's time on the court
        for key in list(games[my_game].keys()):
            if(len(games[my_game][key][1])%2):
                games[my_game][key][1] += [7200*cur_period]
        #adds court entry times for the starting lineup for a period
        lineup_filtered = list(map(lambda y:y.split(",")[2], list(filter(lambda x:x.split(",")[0]==my_game and x.split(",")[1]==str(my_period), foo))))
        for player in lineup_filtered:
            #each player dict maps to a list of three things: their team (str), a list of times their status changes (list(int)) and +/- rating (int)
            if games[my_game].get(player,False):
                games[my_game][player][1] += [7200*(my_period-1)]
            else:
                OHGODWHY =  list(filter(lambda x:my_game in x, foo))
                temp = list(map(lambda y:player in y, OHGODWHY))
                crapIdx = foo.index(OHGODWHY[0])+temp.index(True)
                team = foo[crapIdx].split(",")[3][:-1] if True in temp else None
                games[my_game][player] = [team,[7200*(my_period-1)], 0] #play2[10] may not be accurate. who knows???
    #handles routine transfers
    elif play2[2] == "8":
        p1 = play2[11]
        p2 = play2[12]
        #time since beginning of the game
        tim = 7200-int(play2[5])+7200*(my_period-1)
        if games[my_game].get(p1, False):
            games[my_game][p1][1] += [tim]
        else:
            OHGODWHY = list(filter(lambda x:my_game in x, foo))
            temp = list(map(lambda y:p1 in y, OHGODWHY))
            crapIdx = foo.index(OHGODWHY[0])+temp.index(True)
            team = foo[crapIdx].split(",")[3][:-1] if True in temp else None
            print(team, "______________FUCK YOU")
            games[my_game][p1] = [team, [tim], 0]
        if games[my_game].get(p2, False):
            games[my_game][p2][1] += [tim]
        else:
            OHGODWHY = list(filter(lambda x:my_game in x, foo))
            temp = list(map(lambda y:p2 in y, OHGODWHY))
            if True in temp:
                crapIdx = foo.index(OHGODWHY[0])+temp.index(True)
                team = foo[crapIdx].split(",")[3][:-1]
            else:
                team = play2[10]
            games[my_game][p2] = [team, [tim], 0]
    cur_period = my_period
#at this point we have a list of times where each player enters or leaves the court during each game    
#now for the 2nd pass
for play in list(filter(lambda x:x.split(",")[2] in ["1","3"], bar)):
    play2 = play.split(",")
    my_game = play2[0]
    tim = 7200-int(play2[5])+(int(play2[3])-1)*7200
    on_court = [] #who is on court during the play
    for key in list(games[my_game].keys()):
        for i in range(len(games[my_game][key][1])//2):
            t1 = games[my_game][key][1][i*2]
            t2 = games[my_game][key][1][i*2+1]
            if t1 <= tim <= t2: 
                on_court += [key]
    for player in on_court:
        if games[my_game][player][0] == play2[10]:
            games[my_game][player][2] += int(play2[7])
        #    print(player, "_______________", my_game)
        else:
       #     print(player, "...............", my_game)
            games[my_game][player][2] -= int(play2[7])
with open("fake-garbage-data.csv","a+") as stupidcrap:
    stupidcrap.write("Game_ID,Player_ID,Player_Plus/Minus\n")
    for game in list(games.keys()):
        for player in list(games[game].keys()):
            stupidcrap.write(game+","+player+","+str(games[game][player][2])+"\n")
