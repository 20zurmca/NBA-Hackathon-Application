const fs = require("fs");

let lineups = fs.readFileSync("lineups.in", "utf8");
let lineup_entries = lineups.split(/[(|)]/g);

let games = [] // array for unique game-period objects

lineup_entries.forEach(element => {
    
    data = element.split(/[(,)]/g);
    let game_id = data[0];
    let period = data[1];
    let player = data[2];
    let team = data[3];

    // make new game if it DNE
    let game = games.find((current, index) =>
    {
        return current.game_id == game_id
    });

    if(game == undefined)
    {
        let newgame =  { 
                            game_id: game_id,
                            period: period,
                            team1_id: "", 
                            team2_id: "",
                            team1: [],
                            team2: [],
                        };

        games.push(newgame);
        game = newgame;
    }

    // make new game-period if it DNE
    let anothergame = games.find((current, index) =>
    {
        return current.game_id == game_id && current.period == period; 
    });

    if(anothergame == undefined)
    {
        anothergame = games.find((current, index) =>
        {
            return current.game_id == game_id; 
        });
        
        let newgame =  { 
                            game_id: game_id,
                            period: period,
                            team1_id: anothergame.team1_id, 
                            team2_id: anothergame.team2_id,
                            team1: [],
                            team2: [],
                        };

        games.push(newgame);
        game = newgame;
    }
    else
        game = anothergame

    // assign game teams
    if(game.team1_id == "" && game.team2_id == "")
        game.team1_id = team;
    else if(game.team1_id != team && game.team2_id == "" )
        game.team2_id = team;

    // assign player to team
    if(team == game.team1_id)
        game.team1.push(player);
    else if(team == game.team2_id)
        game.team2.push(player);

});

// prepare solution data structure
/*
[
    {
        game_id: ...
        players: 
                [
                    {
                        id: ...,
                        plus_minus: ...
                    }
                ]
    }
]
*/
let solution = [];

games.forEach(element => {

    // add new game
    let game = solution.find((current, index) =>
    {
        return current.game_id == element.game_id;
    });

    if(game == undefined)
    {
        let newgame =   { 
                            game_id: element.game_id,
                            players: []
                        };

        solution.push(newgame);
        game = newgame;
    }

    // add player
    element.team1.forEach(element => {
        
        let players = game.players.find((current, index) =>
        {
            return current.player_id == element;
        });

        if(players == undefined)
            game.players.push(
                                {
                                    id: element,
                                    plus_minus: 0
                                }
                            );
    });

    element.team2.forEach(element => {
        
        let players = game.players.find((current, index) =>
        {
            return current.player_id == element;
        });

        if(players == undefined)
            game.players.push(
                                {
                                    id: element,
                                    plus_minus: 0
                                }
                            );
    });

});

// read in plays
// note that clock stops for free throws and substitutions inbetween

let plays_entries = fs.readFileSync("relevant_plays.in", "utf8").split(/[(?:|)]/g);

let plays = [] // array for unique game-period objects

plays_entries.forEach(element => {
    
    data = element.split(/[(?:,)]/g);
    let game_id = data[0];
    let period = data[4];
    let event_num = data[1];
    let event_type = data[2];
    let score = data[9];
    let team = data[12];
    let playerOut = data[13];
    let playerIn = data[14];
    let rank = data[16];

    plays.push(
        {
            game_id: game_id,
            period: period,
            event_num: event_num,
            event_type: event_type,
            score: score,
            team: team,
            playerOut: playerOut,
            playerIn: playerIn,
            rank: rank
        }
    )
});

// sort plays
plays.sort((a, b) =>
{
    return a.rank - b.rank;
});

// technique to ensure serializability and synchrony
let i = 0;
while(i < plays.length)
{
    process(plays[i]);
}

setTimeout(resolve, 10);

function process(play)
{

    if(play == undefined)
        return;

    // find game in question
    let game = solution.find((current, index) =>
    {
        return current.game_id == play.game_id && current.period == play.period;
    });

    if(game == undefined)
        return;

    console.log(game.game_id);

    // substitution
    if(play.event_type == 8)
    {
        if(play.team == game.team1_id)
            team = game.team1;
        else
            team = game.team2;

        team.push(play.playerIn);

        // not in line up already
        if(game.players.find((current, index) =>
        {
            return current.id == play.playerIn;
        }) == undefined)
        {
            game.players.push(
                {
                    id: playerIn,
                    plus_minus: 0   
                }
            )
        }

        team.splice(findIndex((current, index) =>
        {
            return current == playerOut;
        }), 1);
    }

    // scoring
    if(play.event_type == 3 || play.event_type == 1)
    {
        if(play.team == game.team1_id)
        {
            // team 1 scored
            game.team1.forEach(element =>
            {
                let player = game.players.find((current, index) =>
                {
                    return current.id == element;
                });

                if(player == undefined)
                    return;
                
                player.plus_minus += score;
            });

            game.team2.forEach(element =>
            {
                let player = game.players.find((current, index) =>
                {
                    return current.id == element;
                });

                if(player == undefined)
                    return;
                
                player.plus_minus -= score;
            });
        }
        else
        {
            // team 2 scored
            game.team2.forEach(element =>
                {
                    let player = game.players.find((current, index) =>
                    {
                        return current.id == element;
                    });
    
                    if(player == undefined)
                        return;
                    
                    player.plus_minus += score;
                });
    
                game.team1.forEach(element =>
                {
                    let player = game.players.find((current, index) =>
                    {
                        return current.id == element;
                    });
    
                    if(player == undefined)
                        return;
                    
                    player.plus_minus -= score;
                });
        }
    }
}


function resolve()
{

    fs.writeFile(`solution.csv`, `Game_ID, Player_ID, Player_Plus/Minus`, (err) =>
        {
            if (err)
                console.log(err);
        });

    solution.forEach( element =>
    {
        let game_id = element.game_id;

        element.players.forEach( element =>
        {
            fs.writeFile(`solution.csv`, `${game_id}, ${element.id}, ${element.plus_minus},`, (err) =>
            {
                if (err)
                    console.log(err);
            });
        });

    });
}