const fs = require("fs");

// prepare solution file
fs.writeFileSync(`solution.csv`, `Game_ID, Player_ID, Player_Plus/Minus\n`, (err) =>
{
    if (err)
        console.log(err);
});

let game_ids = JSON.parse(fs.readFileSync("jsonfiles/games.json", "utf8"));

// for each game
game_ids.forEach(game_id =>
{
    // make game lineup to track game data
    // players: [ {id: ..., plus_minus: ...}, {}, ... ]
    let players = [];

    // get period lineups and game plays
    let periods = JSON.parse(fs.readFileSync(`jsonfiles/game${game_id}.json`, "utf8"));
    let plays = JSON.parse(fs.readFileSync(`jsonfiles/plays${game_id}.json`, "utf8"));

    // add players from period lineups
    periods.forEach(period =>
    {
        period.team1.forEach(player =>
        {
            players.push({id: player, plus_minus: 0})
        });

        period.team2.forEach(player =>
        {
            players.push({id: player, plus_minus: 0})
        });
    });

    // process plays
    plays.forEach(play =>
    {

        let period = periods.find((current, index) =>
        {
            return current.period == play.period;
        })
     
        // substitution
        if(play.event_type == 8)
        {
            if(play.team == period.team1_id)
                team = period.team1;
            else
                team = period.team2;

            // add one
            team.push(play.playerIn);
            
            // remove other
            team = team.splice(team.findIndex((current, index) =>
            {
                return current == play.playerOut;
            }), 1);

            // if playerIn not in line up already
            if(players.find((current, index) =>
            {
                return current.id == play.playerIn;
            }) == undefined)
            {
                players.push(
                    {
                        id: play.playerIn,
                        plus_minus: 0   
                    }
                )
            }  
        }
        
        // scoring
        else if(play.event_type == 3 || play.event_type == 1)
        { 
            let score = parseInt(play.score, 10);
            if(play.team == period.team1_id)
                scores(period.team1, period.team2, score, players);
            else
                scores(period.team2, period.team1, score, players);     
        }
    });

    // append each player to solution file
    players.forEach(player =>
    {
        fs.appendFileSync(`solution.csv`, 
                    `${game_id}, ${player.id}, ${player.plus_minus}\n`, (err) =>
        {
            if (err)
                console.log(err);
        });
    });

});

// allocates score to plus and minus teams on players array
function scores(plus, minus, score, players)
{
    plus.forEach(element =>
    {
        let player = players.find((current, index) =>
        {
            return current.id == element;
        });
        
        player.plus_minus += score;
    });

    minus.forEach(element =>
    {
        let player = players.find((current, index) =>
        {
            return current.id == element;
        });

        player.plus_minus -= score;
    });
}