import { Parser, parseLines } from './lib'

export type Policy = {
  min: number,
  max: number,
  letter: string
}

const pattern = /(?<min>\d+)-(?<max>\d+) (?<letter>[a-z]): (?<password>.*)/

export const parse: Parser<[Policy, string][]> = parseLines(str => {
  const match = str.match(pattern)
  if (!match || !match.groups) { throw (`Could not parse: ${str}`) }

  const groups = match.groups
  return [
    {
      min: parseInt(groups.min),
      max: parseInt(groups.max),
      letter: groups.letter
    },
    groups.password
  ]
})

export function solve(entries: [Policy, string][]) {
  console.log(
    entries.filter(([policy, password]) => valid(policy, password)).length
  ) // 600
  console.log(
    entries.filter(([policy, password]) => revalid(policy, password)).length
  ) // 245
}

export function valid({ min, max, letter }: Policy, password: string) {
  const count = password.split('').reduce((acc, c) => c == letter ? acc + 1 : acc, 0)
  return count >= min && count <= max
}

export function revalid({ min, max, letter }: Policy, password: string) {
  return (password[min - 1] === letter) !== (password[max - 1] === letter)
}
