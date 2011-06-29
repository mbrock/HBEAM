module Opcodes where

opcodeInfo 1 = ("label", 1)
opcodeInfo 2 = ("func_info", 3)
opcodeInfo 3 = ("int_code_end", 0)
opcodeInfo 4 = ("call", 2)
opcodeInfo 5 = ("call_last", 3)
opcodeInfo 6 = ("call_only", 2)
opcodeInfo 7 = ("call_ext", 2)
opcodeInfo 8 = ("call_ext_last", 3)
opcodeInfo 9 = ("bif0", 2)
opcodeInfo 10 = ("bif1", 4)
opcodeInfo 11 = ("bif2", 5)
opcodeInfo 12 = ("allocate", 2)
opcodeInfo 13 = ("allocate_heap", 3)
opcodeInfo 14 = ("allocate_zero", 2)
opcodeInfo 15 = ("allocate_heap_zero", 3)
opcodeInfo 16 = ("test_heap", 2)
opcodeInfo 17 = ("init", 1)
opcodeInfo 18 = ("deallocate", 1)
opcodeInfo 19 = ("return", 0)
opcodeInfo 20 = ("send", 0)
opcodeInfo 21 = ("remove_message", 0)
opcodeInfo 22 = ("timeout", 0)
opcodeInfo 23 = ("loop_rec", 2)
opcodeInfo 24 = ("loop_rec_end", 1)
opcodeInfo 25 = ("wait", 1)
opcodeInfo 26 = ("wait_timeout", 2)
opcodeInfo 27 = ("m_plus", 4)
opcodeInfo 28 = ("m_minus", 4)
opcodeInfo 29 = ("m_times", 4)
opcodeInfo 30 = ("m_div", 4)
opcodeInfo 31 = ("int_div", 4)
opcodeInfo 32 = ("int_rem", 4)
opcodeInfo 33 = ("int_band", 4)
opcodeInfo 34 = ("int_bor", 4)
opcodeInfo 35 = ("int_bxor", 4)
opcodeInfo 36 = ("int_bsl", 4)
opcodeInfo 37 = ("int_bsr", 4)
opcodeInfo 38 = ("int_bnot", 3)
opcodeInfo 39 = ("is_lt", 3)
opcodeInfo 40 = ("is_ge", 3)
opcodeInfo 41 = ("is_eq", 3)
opcodeInfo 42 = ("is_ne", 3)
opcodeInfo 43 = ("is_eq_exact", 3)
opcodeInfo 44 = ("is_ne_exact", 3)
opcodeInfo 45 = ("is_integer", 2)
opcodeInfo 46 = ("is_float", 2)
opcodeInfo 47 = ("is_number", 2)
opcodeInfo 48 = ("is_atom", 2)
opcodeInfo 49 = ("is_pid", 2)
opcodeInfo 50 = ("is_reference", 2)
opcodeInfo 51 = ("is_port", 2)
opcodeInfo 52 = ("is_nil", 2)
opcodeInfo 53 = ("is_binary", 2)
opcodeInfo 54 = ("is_constant", 2)
opcodeInfo 55 = ("is_list", 2)
opcodeInfo 56 = ("is_nonempty_list", 2)
opcodeInfo 57 = ("is_tuple", 2)
opcodeInfo 58 = ("test_arity", 3)
opcodeInfo 59 = ("select_val", 3)
opcodeInfo 60 = ("select_tuple_arity", 3)
opcodeInfo 61 = ("jump", 1)
opcodeInfo 62 = ("catch", 2)
opcodeInfo 63 = ("catch_end", 1)
opcodeInfo 64 = ("move", 2)
opcodeInfo 65 = ("get_list", 3)
opcodeInfo 66 = ("get_tuple_element", 3)
opcodeInfo 67 = ("set_tuple_element", 3)
opcodeInfo 68 = ("put_string", 3)
opcodeInfo 69 = ("put_list", 3)
opcodeInfo 70 = ("put_tuple", 2)
opcodeInfo 71 = ("put", 1)
opcodeInfo 72 = ("badmatch", 1)
opcodeInfo 73 = ("if_end", 0)
opcodeInfo 74 = ("case_end", 1)
opcodeInfo 75 = ("call_fun", 1)
opcodeInfo 76 = ("make_fun", 3)
opcodeInfo 77 = ("is_function", 2)
opcodeInfo 78 = ("call_ext_only", 2)
opcodeInfo 79 = ("bs_start_match", 2)
opcodeInfo 80 = ("bs_get_integer", 5)
opcodeInfo 81 = ("bs_get_float", 5)
opcodeInfo 82 = ("bs_get_binary", 5)
opcodeInfo 83 = ("bs_skip_bits", 4)
opcodeInfo 84 = ("bs_test_tail", 2)
opcodeInfo 85 = ("bs_save", 1)
opcodeInfo 86 = ("bs_restore", 1)
opcodeInfo 87 = ("bs_init", 2)
opcodeInfo 88 = ("bs_final", 2)
opcodeInfo 89 = ("bs_put_integer", 5)
opcodeInfo 90 = ("bs_put_binary", 5)
opcodeInfo 91 = ("bs_put_float", 5)
opcodeInfo 92 = ("bs_put_string", 2)
opcodeInfo 93 = ("bs_need_buf", 1)
opcodeInfo 94 = ("fclearerror", 0)
opcodeInfo 95 = ("fcheckerror", 1)
opcodeInfo 96 = ("fmove", 2)
opcodeInfo 97 = ("fconv", 2)
opcodeInfo 98 = ("fadd", 4)
opcodeInfo 99 = ("fsub", 4)
opcodeInfo 100 = ("fmul", 4)
opcodeInfo 101 = ("fdiv", 4)
opcodeInfo 102 = ("fnegate", 3)
opcodeInfo 103 = ("make_fun2", 1)
opcodeInfo 104 = ("try", 2)
opcodeInfo 105 = ("try_end", 1)
opcodeInfo 106 = ("try_case", 1)
opcodeInfo 107 = ("try_case_end", 1)
opcodeInfo 108 = ("raise", 2)
opcodeInfo 109 = ("bs_init2", 6)
opcodeInfo 110 = ("bs_bits_to_bytes", 3)
opcodeInfo 111 = ("bs_add", 5)
opcodeInfo 112 = ("apply", 1)
opcodeInfo 113 = ("apply_last", 2)
opcodeInfo 114 = ("is_boolean", 2)
opcodeInfo 115 = ("is_function2", 3)
opcodeInfo 116 = ("bs_start_match2", 5)
opcodeInfo 117 = ("bs_get_integer2", 7)
opcodeInfo 118 = ("bs_get_float2", 7)
opcodeInfo 119 = ("bs_get_binary2", 7)
opcodeInfo 120 = ("bs_skip_bits2", 5)
opcodeInfo 121 = ("bs_test_tail2", 3)
opcodeInfo 122 = ("bs_save2", 2)
opcodeInfo 123 = ("bs_restore2", 2)
opcodeInfo 124 = ("gc_bif1", 5)
opcodeInfo 125 = ("gc_bif2", 6)
opcodeInfo 126 = ("bs_final2", 2)
opcodeInfo 127 = ("bs_bits_to_bytes2", 2)
opcodeInfo 128 = ("put_literal", 2)
opcodeInfo 129 = ("is_bitstr", 2)
opcodeInfo 130 = ("bs_context_to_binary", 1)
opcodeInfo 131 = ("bs_test_unit", 3)
opcodeInfo 132 = ("bs_match_string", 4)
opcodeInfo 133 = ("bs_init_writable", 0)
opcodeInfo 134 = ("bs_append", 8)
opcodeInfo 135 = ("bs_private_append", 6)
opcodeInfo 136 = ("trim", 2)
opcodeInfo 137 = ("bs_init_bits", 6)
opcodeInfo 138 = ("bs_get_utf8", 5)
opcodeInfo 139 = ("bs_skip_utf8", 4)
opcodeInfo 140 = ("bs_get_utf16", 5)
opcodeInfo 141 = ("bs_skip_utf16", 4)
opcodeInfo 142 = ("bs_get_utf32", 5)
opcodeInfo 143 = ("bs_skip_utf32", 4)
opcodeInfo 144 = ("bs_utf8_size", 3)
opcodeInfo 145 = ("bs_put_utf8", 3)
opcodeInfo 146 = ("bs_utf16_size", 3)
opcodeInfo 147 = ("bs_put_utf16", 3)
opcodeInfo 148 = ("bs_put_utf32", 3)
opcodeInfo 149 = ("on_load", 0)
opcodeInfo 150 = ("recv_mark", 1)
opcodeInfo 151 = ("recv_set", 1)
opcodeInfo 152 = ("gc_bif3", 7)
opcodeInfo n = error $ "no such opcode " ++ show n

maxOpcode = 152