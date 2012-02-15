$stdin.readline
n = $stdin.readlines.map(&:to_i)
puts n.minmax.join ' '
