import fs from 'fs'

const lineRegex = /^(\d+),(\d+) -> (\d+),(\d+)$/

type Point = {
  x: number
  y: number
}
type Instruction = {
  from: Point
  to: Point
}
type Grid = number[][]

function day5() {
  const entries = parseInstructions()
  const result = part1(entries)
  console.log('Part 1 result ' + result)
  const result2 = part2(entries)
  console.log('Part 2 result ' + result2)
}

function part1(entries: Instruction[]) {
  const horAndVertLines = filterHorAndVertLines(entries)
  const grid = createGrid(getGridDimensions(horAndVertLines))
  const gridWithLines = horAndVertLines.reduce(placeLineOnGrid, grid)
  prettyPrintGrid(gridWithLines)
  return sumOverlappingLines(gridWithLines)
}

function part2(entries: Instruction[]) {
  const { diagonals, nonDiagonals } = filterDiagonals(entries)
  const grid = createGrid(getGridDimensions(entries))
  const gridWithLines = entries.reduce(placeLineOnGrid, grid)
  const gridWithDiagonals = diagonals.reduce(placeDiagonalOnGrid, gridWithLines)
  prettyPrintGrid(gridWithDiagonals)
  return sumOverlappingLines(gridWithDiagonals)
}

function parseInstructions(): Instruction[] {
  const data = fs.readFileSync('input.txt', 'utf8')
  const lines = data.split('\n')
  return lines.filter(Boolean).map((line) => {
    const match = lineRegex.exec(line)
    if (!match) throw Error('failed to parse line')
    return {
      from: {
        x: parseInt(match[1], 10),
        y: parseInt(match[2], 10),
      },
      to: {
        x: parseInt(match[3], 10),
        y: parseInt(match[4], 10),
      },
    }
  })
}

function placeDiagonalOnGrid(grid: Grid, { from, to }: Instruction): Grid {
  const count = Math.abs(from.x - to.x) + 1
  const xDirection = from.x < to.x ? 1 : -1
  const yDirection = from.y < to.y ? 1 : -1
  for (let i = 0; i < count; i++) {
    const x = from.x + xDirection * i
    const y = from.y + yDirection * i
    grid[y][x] += 1
  }
  return grid
}

function placeLineOnGrid(g: Grid, { from, to }: Instruction): Grid {
  if (from.x === to.x) {
    const maxY = Math.max(from.y, to.y)
    const minY = Math.min(from.y, to.y)
    for (let i = minY; i <= maxY; i++) {
      g[i][from.x] += 1
    }
  } else if (from.y === to.y) {
    const maxX = Math.max(from.x, to.x)
    const minX = Math.min(from.x, to.x)
    for (let k = minX; k <= maxX; k++) {
      g[from.y][k] += 1
    }
  }
  return g
}

function sumOverlappingLines(grid: Grid): number {
  return grid.reduce((acc, row) => acc + row.reduce((acc, n) => (n >= 2 ? acc + 1 : acc), 0), 0)
}

function filterHorAndVertLines(entries: Instruction[]) {
  return entries.filter((e) => e.from.x === e.to.x || e.from.y === e.to.y)
}

function prettyPrintGrid(grid: number[][]) {
  grid.forEach((row) => {
    console.log(row.map((n) => (n === 0 ? '.' : n)).join(''))
  })
}

function createGrid({ x, y }: { x: number; y: number }): number[][] {
  return Array.from({ length: y + 1 }, () => Array.from({ length: x + 1 }, () => 0))
}

function getGridDimensions(data: Instruction[]) {
  const x = Math.max(...data.flatMap((i) => [i.from.x, i.to.x]))
  const y = Math.max(...data.flatMap((i) => [i.from.y, i.to.y]))
  return { x, y }
}

function filterDiagonals(entries: Instruction[]) {
  const diags = entries.filter((i) => i.from.x !== i.to.x && i.from.y !== i.to.y)
  const diagonals = []
  const nonDiagonals = filterHorAndVertLines(entries)
  // just in case
  for (const d of diags) {
    const xDiff = Math.abs(d.from.x - d.to.x)
    const yDiff = Math.abs(d.from.y - d.to.y)
    if (xDiff !== yDiff) {
      console.log(
        'Not a 45 diagonal line: ' + d.from.x + ',' + d.from.y + ' -> ' + d.to.x + ',' + d.to.y
      )
    } else {
      diagonals.push(d)
    }
  }
  return { diagonals, nonDiagonals }
}

day5()
