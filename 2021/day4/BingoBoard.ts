type Board = string[][]

class BingoBoard {
    board: Board
    size: number
    row: number[] = [0, 0, 0, 0, 0]
    col: number[] = [0, 0, 0, 0, 0]
    diag: number[] = [0, 0, 0, 0, 0]
    antiDiag: number[] = [0, 0, 0, 0, 0]

    constructor(board) {
        this.board = board
        this.size = board.length
    }

    static create(board: string[]) {
        return new BingoBoard(board.map(line => line.split(" ").filter(v => v !== "")))
    }

    markBoard(val: string) {
        const [r, c] = this.findPosition(val)
        if (r === -1) return false
        this.board[r][c] = "X"
        return this.check([r, c])
    }


    findPosition(val: string) {
        for (let i = 0; i <= this.board.length - 1; i++) {
            const index = this.board[i].indexOf(val)
            if (index > -1) {
                return [i, index]
            }
        }
        return [-1, -1]
    }

    check([r, c]: [number, number]) {
        this.row[r]++;
        this.col[c]++;
        if(r==c) this.diag[r]++;
        if(r+c==this.size) this.antiDiag[r]++;
        if(this.row[r]==this.size || this.col[c]==this.size || this.diag[r]==this.size || this.antiDiag[r]==this.size) return true;
        else return false;
    }

    score() {
        let score = 0
        for (const r of this.board) {
            score += r.filter(v => v !== 'X').reduce((acc, v) => acc + parseInt(v), 0)
        }
        return score
    }

    printBoard() {
        this.board.forEach(row => {
            const values = row.map(v => v.length === 1 ? ` ${v}` : v)
            console.log(values.join(" "))
        })
        console.log('\n')
    }
}

export default BingoBoard
