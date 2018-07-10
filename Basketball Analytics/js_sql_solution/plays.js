const fs = require("fs");

// read in plays

let plays_entries = fs.readFileSync("relevant_plays.in", "utf8").split(/[(|)]/g);
let game_ids = JSON.parse(fs.readFileSync("jsonfiles/games.json", "utf8"));

let plays = [] // array for unique game-period objects

plays_entries.forEach(element => {

    data = element.split(/[(,)]/g);
    if(game_ids.includes(data[0].toString()))
    {
        let game_id = data[0].toString();
        let period = data[4];
        let event_num = data[1];
        let event_type = data[2];
        let score = data[9];
        let team = data[12].toString();
        let playerOut = data[13].toString();
        let playerIn = data[14].toString();
        let rank = data[16];

        plays.push(
            {
                game_id: game_id,
                period: period,
                event_num: event_num,
                event_type: event_type,
                score: parseInt(score, 10),
                team: team,
                playerOut: playerOut,
                playerIn: playerIn,
                rank: rank
            }
        )
    }

});

// separate json file for plays for each game
game_ids.forEach(element =>
    {
        let gameplays = plays.filter(current => 
        {
            return current.game_id == element;
        });

        // sort plays
        gameplays.sort((a, b) =>
        {
            return a.rank - b.rank;
        });
    
        fs.writeFile(`jsonfiles/plays${element}.json`, JSON.stringify(gameplays), (err) =>
        {
            if (err)
                console.log(err);
        });
    });