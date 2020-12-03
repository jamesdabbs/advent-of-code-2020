import { findPair, findTriple } from './01'

const numbers = [
  1721,
  979,
  366,
  299,
  675,
  1456
]

test('findPair', () => {
  expect(findPair(numbers)).toEqual([1721, 299])
})

test('findTriple', () => {
  expect(findTriple(numbers)).toEqual([979, 366, 675])
})