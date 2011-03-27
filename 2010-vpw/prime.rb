def setup
  require "prime"
  @powers = all_powers_of_2
end

def all_powers_of_2
	arr,i = [], 0
	pos = 2 ** i
  while pos < 1_000_000
    arr << pos
    i+=1
  end
	arr
end

def all_powers_between(m,n)
  @powers.reject {|i| i < m || i > n}
end

def smallest_distance(i,m,n)
  all_powers_between(m,n).map {|pow| (pow - i).abs}.min
end

setup

p @powers

#count = gets.to_i

# count.times do 
#   distance = -1
#   prime = -1
#   m, n = gets.split.map(&:to_i)
#   m +=1 if m % 2 == 0
#   m.step(n,2) do |p|
#     if Prime.prime?(p)
#       pos_dis = smallest_distance(p,m,n)
#       if (pos_dis < distance || distance == -1)
#         distance = pos_dis
#         prime = p
#       end
#     end
#   end
#   puts p
# end
