import fs from "fs"
import BingoGame from './BingoGame'

function play() {
    const data = fs.readFileSync("input.txt", 'utf8')
    const game = BingoGame.create(data)
    let winner
    let lastWinner
    while(!lastWinner) {
        game.takeNumber()
        winner = game.winner
        lastWinner = game.lastWinner
    }
    console.log("First winner score: ", winner)
    console.log("Last winner score: ", lastWinner)
    return game.winner
}


play()