if ARGV.size == 0
  puts 'Usage:'
  puts 'ruby get_uniq_moves.rb <pgn file name>'
  exit 1
end

result = []
suffixes = []
File.open(ARGV[0]).each do |line|
  if line =~ /^1\./
    moves = line.split.reject { |m| m =~ /^\d+\./ }.map { |i| i.gsub(/\d/, '1').gsub(/[a-h]/, 'a') }
    result << moves
    suffixes << moves.map { |m| m.gsub(/[[:alnum:]]/, '') }
  end
end

puts result.flatten.uniq.sort
puts suffixes.flatten.uniq.sort
