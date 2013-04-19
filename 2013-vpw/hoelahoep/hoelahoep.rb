def solve(boxes)
  first_box = boxes.shift
  rings = (first_box + first_box.map(&:reverse)).map { |a| [a] }.uniq
  boxes.each do |box|
    box = box + box.map(&:reverse)
    new_rings = []
    box.each do |elem|
      matches = rings.find { |r| r.last[1] == elem[0] }
      p matches
      if matches
        p matches.map { |m| m + [elem] }
        new_rings.concat(matches.map { |m| m + [elem] })
      end
    end
    p new_rings
  end
  0

#  max = 0
#  first_box = boxes.shift
#
#  first_box.each do |element|
#    box = first_box.dup
#    box.delete(element)
#
#    a = complete([element], boxes.dup, box, 1)
#    b = complete([element.reverse], boxes.dup, box, 1)
#
#    if a > max
#      max = a
#    end
#    if b > max
#      max = b
#    end
#  end
#
#  max
end

def complete(ring, boxes, first_box, count)
  max = 0
  first_element = ring.first
  last_element = ring.last

  if boxes.length > 0
    # Try to find a connection in the next box
    next_box = boxes.shift
    p [ring.length, boxes.length]
    next_box.each do |el|
      a = 0
      b = 0
      if el[0] == last_element[1]
        ring2 = ring.dup
        ring2 << el
        a = complete(ring2, boxes.dup, first_box, count + 1)
      end
      if el[1] == last_element[1]
        ring2 = ring.dup
        ring2 << el.reverse
        b = complete(ring2, boxes.dup, first_box, count + 1)
      end

      if a > max
        max = a
      end
      if b > max
        max = b
      end
    end
  end

  # Return to the first box?
  first_box.each do |el|
    a = 0
    b = 0

    if el[0] == last_element[1] && el[1] == last_element[0]
      a = count + 1
    end
    if el[1] == last_element[1] && el[0] == last_element[1]
      b = count + 1
    end

    if a > max
      max = a
    end
    if b > max
      max = b
    end
  end

  max
end

n = gets.chomp.to_i
n.times do
  d = gets.chomp.to_i
  a = gets.chomp.to_i
  boxes = []
  d.times do
    box = []
    a.times do
      box << gets.chomp.split(' ').map(&:to_i)
    end
    boxes << box
  end
  puts solve(boxes)
  exit
end

