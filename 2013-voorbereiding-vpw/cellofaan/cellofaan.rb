def solve(lines)
  max = lines.map do |line|
    color, *parsed = parse(line)
    [parsed[0] + parsed[2], parsed[1] + parsed[3]].max
  end.max
  matrix = Array.new(max) {Array.new(max, 0)}
  lines.each do |line|
    color, x1, y1, width, height = parse(line)
    (x1...(x1+width)).each do |x|
      (y1...(y1+height)).each do |y|
        if matrix[x][y] == 0
          matrix[x][y] += color
        elsif matrix[x][y] == 1
          matrix[x][y] += color if color == 2
        elsif matrix[x][y] == 2
          matrix[x][y] += color if color == 1
        end
      end
    end
  end

  matrix.flatten.find_all {|i| i == 3 }.size

end

def parse(line)
  color = line[0] == "R" ? 1 : 2
  x1, y1, width, height = line[1..-1].split.map(&:to_i)
  return color, x1, y1, width, height
end


n = gets.chomp.to_i
n.times do
  i = gets.chomp.to_i
  lines = []
  i.times { lines << gets }
  puts solve(lines)
end

