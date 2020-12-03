import { numbers } from './lib'

const target = 2020

export const parse = numbers

export async function solve(numbers: number[]) {
  const pair = findPair(numbers)
  console.log(pair && product(pair)) // 73371

  const triple = findTriple(numbers)
  console.log(triple && product(triple)) // 127642310
}

export function findPair(numbers: number[]) {
  for (let i = 0; i < numbers.length; i++) {
    for (let j = i + 1; j < numbers.length; j++) {
      if (numbers[i] + numbers[j] == target) {
        return [numbers[i], numbers[j]]
      }
    }
  }
}

export function findTriple(numbers: number[]) {
  for (let i = 0; i < numbers.length; i++) {
    for (let j = i + 1; j < numbers.length; j++) {
      for (let k = j + 1; k < numbers.length; k++) {
        if (numbers[i] + numbers[j] + numbers[k] == target) {
          return [numbers[i], numbers[j], numbers[k]]
        }
      }
    }
  }
}

function product(values: number[]) {
  return values.reduce((acc, n) => acc * n, 1)
}