class Pizza
  attr_accessor :pizza, :pieces

  def initialize(array, pieces)
    self.pieces = pieces
    self.pizza = array
  end

  def neighbours(i)
    [left_neighbour(i), right_neighbour(i)]
  end

  def left_neighbour(i)
    i = (i - 1 + self.pizza.length) % self.pizza.length
    picked = self.pizza[i]
    while picked == 0
      i = (i - 1 + self.pizza.length) % self.pizza.length
      picked = self.pizza[i]
    end
    [picked, i]
  end

  def right_neighbour(i)
    i = (i + 1) % self.pizza.length
    picked = self.pizza[i]
    while picked == 0
      i = (i + 1) % self.pizza.length
      picked = self.pizza[i]
    end
    [picked, i]
  end

  def max_gain(i)
    l, r = neighbours(i)
    lv, lx = l
    rv, rx = r
    lgain = self.pizza[i] - lv
    rgain = self.pizza[i] - rv
    if lgain > rgain
      return [lgain, lv, lx]
    else
      return [rgain, rv, rx]
    end
  end

  def find_max_gain
    max,index = 0,0
    self.pieces.times do |i|
      gain, v, j = gain(i)
      if gain >= max && self.pizza[i] > self.pizza[index]
        max = gain
        index = i
      end
    end
    [self.pizza[index], index]
  end

  def gain(i)
    l, r = neighbours(i)
    lv, lx = l
    rv, rx = r
    lgain = self.pizza[i] - lv
    rgain = self.pizza[i] - rv
    if lgain > rgain
      return [rgain, lv, lx]
    else
      return [lgain, rv, rx]
    end
  end
  def solve(i)
    picked = self.pizza[i]
    eaten = picked
    self.pizza[i] = 0
    self.pieces -= 1

    alice = false
    while self.pieces > 0
      l, r = neighbours(i)
      lv, lx = l
      rv, rx = r
      lgain, a, b = gain(lx)
      rgain, a,b = gain(rx)

      if rgain > lgain
        picked, i = rv, rx
      else
        picked, i = lv, lx
      end
      self.pizza[i] = 0
      self.pieces -= 1

      if alice
        alice = false
        eaten += picked
      else
        alice = true
      end
    end
    eaten
  end

  def real_solve
    temp = self.pizza.dup
    temp2 = self.pieces
    max = 0
    temp2.times do |i|
      res = solve(i)
      if res > max 
        max = res
      end
      self.pizza = temp.dup
      self.pieces = temp2
    end
    max 
  end
end

i = gets.chomp.to_i
i.times do
  n, *pizza = gets.chomp.split.map(&:to_i)
  p Pizza.new(pizza, n).real_solve()
end
