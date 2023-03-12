require "csv"

lines = IO.readlines('raw_data.csv').map do |line|
  line.gsub!('"','') # remove quotes
  line_split = line.split(',')
  next if line_split[0].split('-')[0].to_i < 1993 # only continue if the year is greater than 1993

  line_split[0] = line_split[0] + '-01' # add day to the date
  line = line_split.join(',')
end

lines = lines.compact # remove nulls results

File.open('processed_data.csv', 'w') do |file|
  file.puts lines
end
