if ARGV.size == 0
  puts 'Usage:'
  puts 'ruby get_uniq_moves.rb <pgn file name>'
  exit 1
end

result = []
File.open(ARGV[0]).each do |line|
  if line =~ /^1\./
    moves = line.split
    result << moves.reject { |m| m =~ /^\d+\.$/ }.map { |i| i.gsub(/\d/, '1').gsub(/[a-h]/, 'a') }
  end
end

puts result.flatten.uniq.sort
