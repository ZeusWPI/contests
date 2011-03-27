#!/usr/bin/ruby

sizes = gets.split
n = sizes[0].to_i
k = sizes[1].to_i
dict = {}

n.times do
  line = gets.split
  dict[line[0]] = line[1]
end

k.times do
  key = gets.chomp
  translation = dict[key]
  if translation then puts translation else puts "???" end
end
