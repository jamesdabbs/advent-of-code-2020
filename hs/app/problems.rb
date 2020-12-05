#!/usr/bin/env ruby

filename, input, output, * = ARGV

examples = Dir.
  foreach(File.expand_path('../../inputs', __dir__)).
  reject { |f| f.start_with?('.') || f.end_with?('example') }

imports = examples.map do |n|
  "import qualified P#{n}"
end.join("\n")

solutions = examples.map do |n|
  "  #{n.to_i} -> P#{n}.solve input"
end.join("\n")

initial = File.read(input)
code = initial.sub('{{IMPORTS}}', imports).sub('{{SOLUTIONS}}', solutions)

File.write(output, code)
