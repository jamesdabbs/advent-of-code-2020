import { lines } from './lib'

const number = (process.env.N || process.argv[2] || '1').padStart(2, '0')

const solution = require(`./${number}`)

solution.
  parse(lines(`../inputs/${number}`)).
  then(solution.solve)
