#!/usr/bin/env ruby

# reads a rule, return a regex and a split diff to apply when matched
# e.g.:
#  .aan5     => "^aan", "0005"
#  4chn      => "chn", "4000"
def parse_rule(line)
  regex = line.gsub /[0-9]/, ''
  regex[0] = '^' if regex[0] == '.'
  regex[-1] = '$' if regex[-1] == '.'

  filter = line.gsub /([0-9])[a-z]/, '\1'
  filter = filter.gsub /[a-z]/, '0'
  filter = filter.gsub /[^0-9]/, ''
  filter = filter.split('').map(&:to_i)

  [Regexp.new(regex), filter]
end

# split a line using the rules
def apply_rules(line, rules)
  score = Array.new(line.length + 1, 0)

  rules.each do |rule|
    regex, filter = rule
    line.scan regex do |_|
      n = Regexp.last_match.begin(0) - 1
      score[n,filter.length].each_with_index do |item, index|
        score[n + index] = [item, filter[index]].max
      end
    end
  end

  score
end

def print_word(word, breaks)
  word.chars.each_with_index do |c, i|
    print c
    print '-' if breaks[i] % 2 == 1
  end
  print "\n"
end

# create dictionary of rules
rules = []
while (line = $stdin.gets) != "----------\n"
  rules << parse_rule(line.strip!)
end

# split words
while line = $stdin.gets
  breaks = apply_rules(line.strip!, rules)
  print_word(line, breaks)
end
