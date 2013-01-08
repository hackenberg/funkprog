extract "we10iopK11P iU010" == "1011010"
extract "0w0e010iopK101P iU01B1" == "10101011"
extract "aerfweife" == "0"
extract "a0erf00we0ife" == "0"
extract "a0erf00w1e0ife" == "10"

nbIncr "110" == "111"
nbIncr "10" == "11"
nbIncr "1101" == "10"
nbIncr "11111" == "11100"
nbIncr "11" == "0"

nbDecr "1" == "0"
nbDecr "1101" == "1100"
nbDecr "1110" == "1001"
nbDecr "11111" == "11110"
nbDecr "110" == "1"

nbAbs "0" == "0"
nbAbs "11" == "1"
nbAbs "1111" == "101"
nbAbs "111" == "111"
nbAbs "1110" == "11010"

nbPlus "0" "1110" == "1110"
nbPlus "10101" "0" == "10101"
nbPlus "110" "111" == "101"
nbPlus "10" "1100" == "1110"
nbPlus "1100" "101" == "1"

nbTimes "0" "1110" == "0"
nbTimes "10101" "0" == "0"
nbTimes "10" "1100" == "11000"
nbTimes "110" "111" == "11010"
nbTimes "110" "1101" == "1110"