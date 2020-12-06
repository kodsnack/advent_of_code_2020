input = File.read('input')

def to_passport(entry)
  passport = {}
  entry.lines.each do |line|
    passport.merge! line.strip.split.map { |f| f.split ':' }.to_h
  end
  passport
end

passports = input.strip.split("\n\n").map(&method(:to_passport))

REQUIRED_KEYS = %w[byr iyr eyr hgt hcl ecl pid]

def required_present?(passport)
  missing_keys = REQUIRED_KEYS - passport.keys
  missing_keys.empty?
end

puts passports.count(&method(:required_present?))

def valid?(passport)
  return false unless required_present? passport
  
  # byr (Birth Year) - four digits; at least 1920 and at most 2002.
  byr = passport['byr'].to_i
  return false unless (1920..2002).include? byr
  
  # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  iyr = passport['iyr'].to_i
  return false unless (2010..2020).include? iyr
  
  # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  eyr = passport['eyr'].to_i
  return false unless (2020..2030).include? eyr
  
  # hgt (Height) - a number followed by either cm or in:
  #     If cm, the number must be at least 150 and at most 193.
  #     If in, the number must be at least 59 and at most 76.
  hgt = passport['hgt']
  case
  when hgt.end_with?('cm') then return false unless (150..193).include? hgt.to_i
  when hgt.end_with?('in') then return false unless (59..76).include? hgt.to_i
  else return false
  end
  
  # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  hcl = passport['hcl']
  return false unless hcl.match? /^#[0-9a-f]{6}$/
  
  # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  ecl = passport['ecl']
  return false unless %w[amb blu brn gry grn hzl oth].include? ecl
  
  # pid (Passport ID) - a nine-digit number, including leading zeroes.
  pid = passport['pid']
  return false unless pid.match? /^[0-9]{9}$/

  # cid (Country ID) - ignored, missing or not.
  true
end

puts passports.count(&method(:valid?))

# input = <<~EOS
# ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
# byr:1937 iyr:2017 cid:147 hgt:183cm
#
# iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
# hcl:#cfa07d byr:1929
#
# hcl:#ae17e1 iyr:2013
# eyr:2024
# ecl:brn pid:760753108 byr:1931
# hgt:179cm
#
# hcl:#cfa07d eyr:2025 pid:166559648
# iyr:2011 ecl:brn hgt:59in
# EOS

# input = <<~EOS
# eyr:1972 cid:100
# hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
#
# iyr:2019
# hcl:#602927 eyr:1967 hgt:170cm
# ecl:grn pid:012533040 byr:1946
#
# hcl:dab227 iyr:2012
# ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
#
# hgt:59cm ecl:zzz
# eyr:2038 hcl:74454a iyr:2023
# pid:3556412378 byr:2007
# EOS

# input = <<~EOS
# pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
# hcl:#623a2f
#
# eyr:2029 ecl:blu cid:129 byr:1989
# iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
#
# hcl:#888785
# hgt:164cm byr:2001 iyr:2015 cid:88
# pid:545766238 ecl:hzl
# eyr:2022
#
# iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719
# EOS
