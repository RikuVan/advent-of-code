import BingoBoard from './BingoBoard'

class BingoGame {
    boards: BingoBoard[]
    numbers: string[]
    winner?: number
    allWinners = new Set()
    lastWinner?: number

    constructor(boards: BingoBoard[], numbers: string[]) {
        this.boards = boards
        this.numbers = numbers
    }

    static create(data: string) {
        const [numbers, ...boardLines] = data.split("\n")
        const boards = chunk(5, boardLines.filter(line => line.trim().length)).map(BingoBoard.create)
        return new BingoGame(boards, numbers.split(","))
    }

    takeNumber() {
        if (this.numbers.length < 1) throw Error("out of numbers")
        const [n, ...t] = this.numbers
        this.numbers = t
        for (const [i, board] of this.boards.entries()) {
            const bingo = board.markBoard(n)
            if (bingo) {
                if (!this.winner) {
                    this.winner = board.score() * parseInt(n)
                    console.log("First winner", board.printBoard())
                }
                this.allWinners.add(i)
                if (this.allWinners.size === this.boards.length) {
                    this.lastWinner = board.score() * parseInt(n)
                    console.log("Last winner: ", board.printBoard())
                    break
                }
            }
        }
    }
}

export function chunk<T extends {}>(size: number, array: T[]): T[][] {
    if (!array.length) {
        return [];
    }
    if (size > array.length) {
        return [array];
    }
    const head = array.slice(0, size);
    const tail = array.slice(size);

    return [head, ...chunk(size, tail)];
}

export default BingoGame