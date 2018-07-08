#with open("NBA Hackathon - Play by Play Data Sample (50 Games).txt","r") as pbp:
#    foo = pbp.read().split()
#with open("Jakob-pbp.csv","a+") as jakob:
#    for i in range(len(foo)//14):
#        jakob.write(",".join(foo[i*14:(i+1)*14])+"\n")

with open("NBA Hackathon - Game Lineup Data Sample (50 Games).txt","r") as lineup:
    foo = lineup.read().split()

with open("Jakob-lineup.csv","a+") as jakob:
    for i in range(len(foo)//5):
        jakob.write(",".join(foo[i*5:(i+1)*5-1])+"\n")
        
