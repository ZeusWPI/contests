
class Picture
  attr_accessor :height, :width, :data

  def initialize(height, width, data)
    @height = height
    @width  = width
    @data   = data
  end
  
  def rotate_clockwise()
    height = @width
    width  = @height
    data   = 0.upto(height - 1).collect do |y|
      0.upto(width - 1).collect do |x|
        @data[width - x - 1][y]
      end
    end
    Picture.new(height, width, data)
  end

  def search_rotating(sub)
    0.upto(3) do |r|
      coords = search(sub)
      if coords then
        return [coords[0], coords[1], r]
      end
      sub = sub.rotate_clockwise
    end
  end

  def search(sub)
    0.upto(@height - sub.height).each do |y|
      0.upto(@width - sub.width).each do |x|
        if matches(sub, y, x) then
          return [y, x]
        end
      end
    end

    return nil
  end

  def to_s
    str = ''
    0.upto(@height - 1).each do |y|
      0.upto(@width - 1).each do |x|
        str += @data[y][x]
      end
      str += "\n"
    end
    str
  end

  private

  def matches(sub, offset_y, offset_x)
    0.upto(sub.height - 1).each do |y|
      0.upto(sub.width - 1).each do |x|
        unless @data[offset_y + y][offset_x + x] == sub.data[y][x]
          return false
        end
      end
    end

    return true
  end
end

def read_picture()
  dimensions = gets.split  
  height     = dimensions[0].to_i
  width      = dimensions[1].to_i
  data       = 1.upto(height).collect { gets.strip }
  Picture.new(height, width, data)
end

cases = gets.to_i
cases.times do
  picture = read_picture
  subs    = gets.to_i
  subs.times do
    sub   = read_picture
    descr = picture.search_rotating(sub)
    puts "#{descr[0] + 1} #{descr[1] + 1} #{descr[2] * 90}"
  end
end
