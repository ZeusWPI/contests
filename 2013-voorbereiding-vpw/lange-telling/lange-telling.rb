require 'date'

class MayanDate < Struct.new(:kin, :uinal, :tun, :katun, :baktun)

  def self.base_date
    return new(5, 7, 16, 17, 12)
  end

  def to_s
    [self.kin, self.uinal, self.tun, self.katun, self.baktun].reverse.join('.')
  end

  def +(other_date)
    (other_date - Date.parse('1970-01-01')).to_i.times { self.increment_kin }
    self
  end

  def increment_kin
    self.kin += 1
    if self.kin == 20
      self.kin = 0
      self.uinal += 1
      if self.uinal == 18
        self.uinal = 0
        self.tun += 1
        if self.tun == 20
          self.tun = 0
          self.katun += 1
          if self.katun == 20
            self.katun = 0
            self.baktun += 1
          end
        end
      end
    end
  end

end
n = gets.chomp.to_i
n.times do
  puts MayanDate.base_date + Date.parse(gets)
end
