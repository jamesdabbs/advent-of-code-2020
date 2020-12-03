import { Policy, revalid, valid } from './02'

const input: [Policy, string][] = [
  [{ min: 1, max: 3, letter: 'a' }, 'abcde'],
  [{ min: 1, max: 3, letter: 'b' }, 'cdefg'],
  [{ min: 2, max: 9, letter: 'c' }, 'ccccccccc'],
]

function matches(strategy, entries = input) {
  return entries
    .filter(([policy, password]) => strategy(policy, password))
    .map(([policy, _]) => policy.letter)
}

test('valid', () => {
  expect(matches(valid)).toEqual(['a', 'c'])
})

test('revalid', () => {
  expect(matches(revalid)).toEqual(['a'])
})