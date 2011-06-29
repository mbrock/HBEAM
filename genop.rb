puts "module Opcodes where"
puts

max_opcode = 0

STDIN.each_line do |line|
  case line
  when /^#/: next
  when /^BEAM_FORMAT_NUMBER=(.*)/
    fail unless $1 == '0'
  when /^(\d+): -?(.*?)\/(\d+)$/
    op = $1.to_i
    max_opcode = op if op > max_opcode
    puts "opcodeInfo #{$1.to_i} = (\"#$2\", #$3)"
  end
end

puts 'opcodeInfo n = error $ "no such opcode " ++ show n'

puts
puts "maxOpcode = #{max_opcode}"
