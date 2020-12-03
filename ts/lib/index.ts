import * as fs from 'fs'
import * as readline from 'readline'

export type Lines = readline.Interface
export type Parser<T> = (lines: Lines) => Promise<T>

export function lines(path: string) {
  return readline.createInterface({
    input: fs.createReadStream(path),
    crlfDelay: Infinity
  })
}

export function parseLines<T>(
  parser: (str: string) => T
): Parser<T[]> {
  return async function parse(lines: Lines) {
    const acc = []

    for await (const line of lines) {
      acc.push(parser(line))
    }

    return acc
  }
}

export const numbers: Parser<number[]> = parseLines(parseInt)
