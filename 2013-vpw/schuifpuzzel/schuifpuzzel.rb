def go(grid, solution, correct_spot, empty_spot, previous_spot, count, min)
  if grid == solution || count > 14
    return count
  end

  bound = Math.sqrt((correct_spot[0] - empty_spot[0]) ** 2 + (correct_spot[1] - empty_spot[1]) ** 2)
  if (count + bound >= min)
    return 15
  end

  left, right, top, down = 15, 15, 15, 15

  # Check directions
  if empty_spot[0] > 0
    next_spot = [empty_spot[0] - 1, empty_spot[1]]
    if next_spot != previous_spot
      switch(grid, empty_spot, next_spot)
      top = go(grid, solution, correct_spot, next_spot, empty_spot, count + 1, 
        [left, right, top, down].min)
      switch(grid, next_spot, empty_spot)
    end
  end
  if empty_spot[0] < grid.length - 1
    next_spot = [empty_spot[0] + 1, empty_spot[1]]
    if next_spot != previous_spot
      switch(grid, empty_spot, next_spot)
      down = go(grid, solution, correct_spot, next_spot, empty_spot, count + 1,

  [left, right, top, down].min)
      switch(grid, next_spot, empty_spot)
    end
  end
  if empty_spot[1] > 0
    next_spot = [empty_spot[0], empty_spot[1] - 1]
    if next_spot != previous_spot
      switch(grid, empty_spot, next_spot)
      left = go(grid, solution, correct_spot, next_spot, empty_spot, count + 1, 

  [left, right, top, down].min)
      switch(grid, next_spot, empty_spot)
    end
  end
  if empty_spot[1] < grid[0].length - 1
    next_spot = [empty_spot[0], empty_spot[1] + 1]
    if next_spot != previous_spot
      switch(grid, empty_spot, next_spot)
      right = go(grid, solution, correct_spot, next_spot, empty_spot, count + 1, 
        
  [left, right, top, down].min)
      switch(grid, next_spot, empty_spot)
    end
  end

  [left, right, top, down].min
end

def switch(grid, empty_spot, next_spot)
  a = grid[next_spot[0]][next_spot[1]]
  grid[next_spot[0]][next_spot[1]] = grid[empty_spot[0]][empty_spot[1]]
  grid[empty_spot[0]][empty_spot[1]] = a
end


def parse_grid(input, rows, columns)
  input.chomp("\n").split('').each_slice(columns).to_a
end

n = gets.chomp.to_i
n.times do
  rows, columns = gets.split(' ').map(&:to_i)
  grid = parse_grid(gets, rows, columns)
  solution = parse_grid(gets, rows, columns)
  empty_spot = [grid.find_index { |row| row.index(' ') != nil }]
  empty_spot << grid[empty_spot[0]].index(' ')

  correct_spot = [solution.find_index { |row| row.index(' ') != nil }]
  correct_spot << solution[correct_spot[0]].index(' ')

  puts go(grid, solution, correct_spot, empty_spot, empty_spot, 0, 15)
end
