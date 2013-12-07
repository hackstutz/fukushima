# Measuring and analysing the causal impact of the Fukushima Daiichi nuclear disaster

## Data

We use the International Social Survey Program (ISSP) data 2010 which added an environmental attitudes survey as a survey focus. The survey was conducted between 2010 and 2011 whereby the exact period varies by country. Data needs to be downloaded from ZACAT <http://zacat.gesis.org/webview/>.


```r
library(foreign)
setwd("C:/Users/Hackstutz/Dropbox/Fukushima/")
issp <- read.dta("issp_data/ZA5500_v2-0-0.dta")
```

```
## Warning: duplicated levels in factors are deprecated Warning: duplicated
## levels in factors are deprecated Warning: duplicated levels in factors are
## deprecated
```


For a few countries we have information about the month the interview took place but not the exact day. We suggest to impute these information using random uniform distributions.


```r
# random.org
set.seed(950075)

issp$randomDY <- NULL
howmany31 <- length(which(issp$DATEMO %in% c("January", "March", "May", "July", 
    "August", "October", "December")))
howmany30 <- length(which(issp$DATEMO %in% c("April", "June", "September", "November")))
howmany28 <- length(which(issp$DATEMO %in% c("February")))
# Plus cases missing DATEMO Sums to N (45199)

# Create random days that can be used to fill Missings
issp$randomDY[issp$DATEMO %in% c("January", "March", "May", "July", "August", 
    "October", "December")] <- floor(runif(howmany31, 1, 32))
issp$randomDY[issp$DATEMO %in% c("April", "June", "September", "November")] <- floor(runif(howmany30, 
    1, 31))
issp$randomDY[issp$DATEMO %in% c("February")] <- floor(runif(howmany28, 1, 29))

# Fill where the value of DATEDY is missing:
issp$DATEDY[is.na(issp$DATEDY)] <- issp$randomDY[is.na(issp$DATEDY)]

# Create a nice data variable
levels(issp$DATEYR) <- c("2009", "2010", "2011", "No answer")
issp$datestring <- paste(issp$DATEYR, issp$DATEMO, issp$DATEDY, sep = "-")
# Change the locale to apply strptime with english names for months
Sys.setlocale("LC_TIME", "us")
```

```
## [1] "English_United States.1252"
```

```r
issp$date <- strptime(issp$datestring, format = "%Y-%B-%d")
issp$V4[is.na(issp$date)]  #6471 Missings
```

```
##    [1] DK-Denmark                            
##    [2] DK-Denmark                            
##    [3] DK-Denmark                            
##    [4] DK-Denmark                            
##    [5] DK-Denmark                            
##    [6] DK-Denmark                            
##    [7] DK-Denmark                            
##    [8] DK-Denmark                            
##    [9] DK-Denmark                            
##   [10] DK-Denmark                            
##   [11] DK-Denmark                            
##   [12] DK-Denmark                            
##   [13] DK-Denmark                            
##   [14] DK-Denmark                            
##   [15] DK-Denmark                            
##   [16] DK-Denmark                            
##   [17] DK-Denmark                            
##   [18] DK-Denmark                            
##   [19] DK-Denmark                            
##   [20] DK-Denmark                            
##   [21] DK-Denmark                            
##   [22] DK-Denmark                            
##   [23] DK-Denmark                            
##   [24] DK-Denmark                            
##   [25] DK-Denmark                            
##   [26] DK-Denmark                            
##   [27] DK-Denmark                            
##   [28] DK-Denmark                            
##   [29] DK-Denmark                            
##   [30] DK-Denmark                            
##   [31] DK-Denmark                            
##   [32] DK-Denmark                            
##   [33] DK-Denmark                            
##   [34] DK-Denmark                            
##   [35] DK-Denmark                            
##   [36] DK-Denmark                            
##   [37] DK-Denmark                            
##   [38] DK-Denmark                            
##   [39] DK-Denmark                            
##   [40] DK-Denmark                            
##   [41] DK-Denmark                            
##   [42] DK-Denmark                            
##   [43] FR-France                             
##   [44] FR-France                             
##   [45] FR-France                             
##   [46] FR-France                             
##   [47] FR-France                             
##   [48] FR-France                             
##   [49] FR-France                             
##   [50] FR-France                             
##   [51] FR-France                             
##   [52] FR-France                             
##   [53] FR-France                             
##   [54] FR-France                             
##   [55] FR-France                             
##   [56] FR-France                             
##   [57] FR-France                             
##   [58] FR-France                             
##   [59] FR-France                             
##   [60] FR-France                             
##   [61] FR-France                             
##   [62] FR-France                             
##   [63] FR-France                             
##   [64] FR-France                             
##   [65] FR-France                             
##   [66] FR-France                             
##   [67] FR-France                             
##   [68] FR-France                             
##   [69] FR-France                             
##   [70] FR-France                             
##   [71] FR-France                             
##   [72] FR-France                             
##   [73] FR-France                             
##   [74] FR-France                             
##   [75] FR-France                             
##   [76] FR-France                             
##   [77] FR-France                             
##   [78] FR-France                             
##   [79] FR-France                             
##   [80] FR-France                             
##   [81] FR-France                             
##   [82] FR-France                             
##   [83] FR-France                             
##   [84] FR-France                             
##   [85] FR-France                             
##   [86] FR-France                             
##   [87] FR-France                             
##   [88] FR-France                             
##   [89] FR-France                             
##   [90] FR-France                             
##   [91] FR-France                             
##   [92] FR-France                             
##   [93] FR-France                             
##   [94] FR-France                             
##   [95] FR-France                             
##   [96] FR-France                             
##   [97] FR-France                             
##   [98] FR-France                             
##   [99] FR-France                             
##  [100] FR-France                             
##  [101] FR-France                             
##  [102] FR-France                             
##  [103] FR-France                             
##  [104] FR-France                             
##  [105] FR-France                             
##  [106] FR-France                             
##  [107] FR-France                             
##  [108] FR-France                             
##  [109] FR-France                             
##  [110] FR-France                             
##  [111] FR-France                             
##  [112] FR-France                             
##  [113] FR-France                             
##  [114] FR-France                             
##  [115] FR-France                             
##  [116] FR-France                             
##  [117] FR-France                             
##  [118] FR-France                             
##  [119] FR-France                             
##  [120] FR-France                             
##  [121] FR-France                             
##  [122] FR-France                             
##  [123] FR-France                             
##  [124] FR-France                             
##  [125] FR-France                             
##  [126] FR-France                             
##  [127] FR-France                             
##  [128] FR-France                             
##  [129] FR-France                             
##  [130] FR-France                             
##  [131] FR-France                             
##  [132] FR-France                             
##  [133] FR-France                             
##  [134] FR-France                             
##  [135] FR-France                             
##  [136] FR-France                             
##  [137] FR-France                             
##  [138] FR-France                             
##  [139] FR-France                             
##  [140] FR-France                             
##  [141] FR-France                             
##  [142] FR-France                             
##  [143] FR-France                             
##  [144] FR-France                             
##  [145] FR-France                             
##  [146] FR-France                             
##  [147] FR-France                             
##  [148] FR-France                             
##  [149] FR-France                             
##  [150] FR-France                             
##  [151] FR-France                             
##  [152] FR-France                             
##  [153] FR-France                             
##  [154] FR-France                             
##  [155] FR-France                             
##  [156] FR-France                             
##  [157] FR-France                             
##  [158] FR-France                             
##  [159] FR-France                             
##  [160] FR-France                             
##  [161] FR-France                             
##  [162] FR-France                             
##  [163] FR-France                             
##  [164] FR-France                             
##  [165] FR-France                             
##  [166] FR-France                             
##  [167] FR-France                             
##  [168] FR-France                             
##  [169] FR-France                             
##  [170] FR-France                             
##  [171] FR-France                             
##  [172] FR-France                             
##  [173] FR-France                             
##  [174] FR-France                             
##  [175] FR-France                             
##  [176] FR-France                             
##  [177] FR-France                             
##  [178] FR-France                             
##  [179] FR-France                             
##  [180] FR-France                             
##  [181] FR-France                             
##  [182] FR-France                             
##  [183] FR-France                             
##  [184] FR-France                             
##  [185] FR-France                             
##  [186] FR-France                             
##  [187] FR-France                             
##  [188] FR-France                             
##  [189] FR-France                             
##  [190] FR-France                             
##  [191] FR-France                             
##  [192] FR-France                             
##  [193] FR-France                             
##  [194] FR-France                             
##  [195] FR-France                             
##  [196] FR-France                             
##  [197] FR-France                             
##  [198] FR-France                             
##  [199] FR-France                             
##  [200] FR-France                             
##  [201] FR-France                             
##  [202] FR-France                             
##  [203] FR-France                             
##  [204] FR-France                             
##  [205] FR-France                             
##  [206] FR-France                             
##  [207] FR-France                             
##  [208] FR-France                             
##  [209] FR-France                             
##  [210] FR-France                             
##  [211] FR-France                             
##  [212] FR-France                             
##  [213] FR-France                             
##  [214] FR-France                             
##  [215] FR-France                             
##  [216] FR-France                             
##  [217] FR-France                             
##  [218] FR-France                             
##  [219] FR-France                             
##  [220] FR-France                             
##  [221] FR-France                             
##  [222] FR-France                             
##  [223] FR-France                             
##  [224] FR-France                             
##  [225] FR-France                             
##  [226] FR-France                             
##  [227] FR-France                             
##  [228] FR-France                             
##  [229] FR-France                             
##  [230] FR-France                             
##  [231] FR-France                             
##  [232] FR-France                             
##  [233] FR-France                             
##  [234] FR-France                             
##  [235] FR-France                             
##  [236] FR-France                             
##  [237] FR-France                             
##  [238] FR-France                             
##  [239] FR-France                             
##  [240] FR-France                             
##  [241] FR-France                             
##  [242] FR-France                             
##  [243] FR-France                             
##  [244] FR-France                             
##  [245] FR-France                             
##  [246] FR-France                             
##  [247] FR-France                             
##  [248] FR-France                             
##  [249] FR-France                             
##  [250] FR-France                             
##  [251] FR-France                             
##  [252] FR-France                             
##  [253] FR-France                             
##  [254] FR-France                             
##  [255] FR-France                             
##  [256] FR-France                             
##  [257] FR-France                             
##  [258] FR-France                             
##  [259] FR-France                             
##  [260] FR-France                             
##  [261] FR-France                             
##  [262] FR-France                             
##  [263] FR-France                             
##  [264] FR-France                             
##  [265] FR-France                             
##  [266] FR-France                             
##  [267] FR-France                             
##  [268] FR-France                             
##  [269] FR-France                             
##  [270] FR-France                             
##  [271] FR-France                             
##  [272] FR-France                             
##  [273] FR-France                             
##  [274] FR-France                             
##  [275] FR-France                             
##  [276] FR-France                             
##  [277] FR-France                             
##  [278] FR-France                             
##  [279] FR-France                             
##  [280] FR-France                             
##  [281] FR-France                             
##  [282] FR-France                             
##  [283] FR-France                             
##  [284] FR-France                             
##  [285] FR-France                             
##  [286] FR-France                             
##  [287] FR-France                             
##  [288] FR-France                             
##  [289] FR-France                             
##  [290] FR-France                             
##  [291] FR-France                             
##  [292] FR-France                             
##  [293] FR-France                             
##  [294] FR-France                             
##  [295] FR-France                             
##  [296] FR-France                             
##  [297] FR-France                             
##  [298] FR-France                             
##  [299] FR-France                             
##  [300] FR-France                             
##  [301] FR-France                             
##  [302] FR-France                             
##  [303] FR-France                             
##  [304] FR-France                             
##  [305] FR-France                             
##  [306] FR-France                             
##  [307] FR-France                             
##  [308] FR-France                             
##  [309] FR-France                             
##  [310] FR-France                             
##  [311] FR-France                             
##  [312] FR-France                             
##  [313] FR-France                             
##  [314] FR-France                             
##  [315] FR-France                             
##  [316] FR-France                             
##  [317] FR-France                             
##  [318] FR-France                             
##  [319] FR-France                             
##  [320] FR-France                             
##  [321] FR-France                             
##  [322] FR-France                             
##  [323] FR-France                             
##  [324] FR-France                             
##  [325] FR-France                             
##  [326] FR-France                             
##  [327] FR-France                             
##  [328] FR-France                             
##  [329] FR-France                             
##  [330] FR-France                             
##  [331] FR-France                             
##  [332] FR-France                             
##  [333] FR-France                             
##  [334] FR-France                             
##  [335] FR-France                             
##  [336] FR-France                             
##  [337] FR-France                             
##  [338] FR-France                             
##  [339] FR-France                             
##  [340] FR-France                             
##  [341] FR-France                             
##  [342] FR-France                             
##  [343] FR-France                             
##  [344] FR-France                             
##  [345] FR-France                             
##  [346] FR-France                             
##  [347] FR-France                             
##  [348] FR-France                             
##  [349] FR-France                             
##  [350] FR-France                             
##  [351] FR-France                             
##  [352] FR-France                             
##  [353] FR-France                             
##  [354] FR-France                             
##  [355] FR-France                             
##  [356] FR-France                             
##  [357] FR-France                             
##  [358] FR-France                             
##  [359] FR-France                             
##  [360] FR-France                             
##  [361] FR-France                             
##  [362] FR-France                             
##  [363] FR-France                             
##  [364] FR-France                             
##  [365] FR-France                             
##  [366] FR-France                             
##  [367] FR-France                             
##  [368] FR-France                             
##  [369] FR-France                             
##  [370] FR-France                             
##  [371] FR-France                             
##  [372] FR-France                             
##  [373] FR-France                             
##  [374] FR-France                             
##  [375] FR-France                             
##  [376] FR-France                             
##  [377] FR-France                             
##  [378] FR-France                             
##  [379] FR-France                             
##  [380] FR-France                             
##  [381] FR-France                             
##  [382] FR-France                             
##  [383] FR-France                             
##  [384] FR-France                             
##  [385] FR-France                             
##  [386] FR-France                             
##  [387] FR-France                             
##  [388] FR-France                             
##  [389] FR-France                             
##  [390] FR-France                             
##  [391] FR-France                             
##  [392] FR-France                             
##  [393] FR-France                             
##  [394] FR-France                             
##  [395] FR-France                             
##  [396] FR-France                             
##  [397] FR-France                             
##  [398] FR-France                             
##  [399] FR-France                             
##  [400] FR-France                             
##  [401] FR-France                             
##  [402] FR-France                             
##  [403] FR-France                             
##  [404] FR-France                             
##  [405] FR-France                             
##  [406] FR-France                             
##  [407] FR-France                             
##  [408] FR-France                             
##  [409] FR-France                             
##  [410] FR-France                             
##  [411] FR-France                             
##  [412] FR-France                             
##  [413] FR-France                             
##  [414] FR-France                             
##  [415] FR-France                             
##  [416] FR-France                             
##  [417] FR-France                             
##  [418] FR-France                             
##  [419] FR-France                             
##  [420] FR-France                             
##  [421] FR-France                             
##  [422] FR-France                             
##  [423] FR-France                             
##  [424] FR-France                             
##  [425] FR-France                             
##  [426] FR-France                             
##  [427] FR-France                             
##  [428] FR-France                             
##  [429] FR-France                             
##  [430] FR-France                             
##  [431] FR-France                             
##  [432] FR-France                             
##  [433] FR-France                             
##  [434] FR-France                             
##  [435] FR-France                             
##  [436] FR-France                             
##  [437] FR-France                             
##  [438] FR-France                             
##  [439] FR-France                             
##  [440] FR-France                             
##  [441] FR-France                             
##  [442] FR-France                             
##  [443] FR-France                             
##  [444] FR-France                             
##  [445] FR-France                             
##  [446] FR-France                             
##  [447] FR-France                             
##  [448] FR-France                             
##  [449] FR-France                             
##  [450] FR-France                             
##  [451] FR-France                             
##  [452] FR-France                             
##  [453] FR-France                             
##  [454] FR-France                             
##  [455] FR-France                             
##  [456] FR-France                             
##  [457] FR-France                             
##  [458] FR-France                             
##  [459] FR-France                             
##  [460] FR-France                             
##  [461] FR-France                             
##  [462] FR-France                             
##  [463] FR-France                             
##  [464] FR-France                             
##  [465] FR-France                             
##  [466] FR-France                             
##  [467] FR-France                             
##  [468] FR-France                             
##  [469] FR-France                             
##  [470] FR-France                             
##  [471] FR-France                             
##  [472] FR-France                             
##  [473] FR-France                             
##  [474] FR-France                             
##  [475] FR-France                             
##  [476] FR-France                             
##  [477] FR-France                             
##  [478] FR-France                             
##  [479] FR-France                             
##  [480] FR-France                             
##  [481] FR-France                             
##  [482] FR-France                             
##  [483] FR-France                             
##  [484] FR-France                             
##  [485] FR-France                             
##  [486] FR-France                             
##  [487] FR-France                             
##  [488] FR-France                             
##  [489] FR-France                             
##  [490] FR-France                             
##  [491] FR-France                             
##  [492] FR-France                             
##  [493] FR-France                             
##  [494] FR-France                             
##  [495] FR-France                             
##  [496] FR-France                             
##  [497] FR-France                             
##  [498] FR-France                             
##  [499] FR-France                             
##  [500] FR-France                             
##  [501] FR-France                             
##  [502] FR-France                             
##  [503] FR-France                             
##  [504] FR-France                             
##  [505] FR-France                             
##  [506] FR-France                             
##  [507] FR-France                             
##  [508] FR-France                             
##  [509] FR-France                             
##  [510] FR-France                             
##  [511] FR-France                             
##  [512] FR-France                             
##  [513] FR-France                             
##  [514] FR-France                             
##  [515] FR-France                             
##  [516] FR-France                             
##  [517] FR-France                             
##  [518] FR-France                             
##  [519] FR-France                             
##  [520] FR-France                             
##  [521] FR-France                             
##  [522] FR-France                             
##  [523] FR-France                             
##  [524] FR-France                             
##  [525] FR-France                             
##  [526] FR-France                             
##  [527] FR-France                             
##  [528] FR-France                             
##  [529] FR-France                             
##  [530] FR-France                             
##  [531] FR-France                             
##  [532] FR-France                             
##  [533] FR-France                             
##  [534] FR-France                             
##  [535] FR-France                             
##  [536] FR-France                             
##  [537] FR-France                             
##  [538] FR-France                             
##  [539] FR-France                             
##  [540] FR-France                             
##  [541] FR-France                             
##  [542] FR-France                             
##  [543] FR-France                             
##  [544] FR-France                             
##  [545] FR-France                             
##  [546] FR-France                             
##  [547] FR-France                             
##  [548] FR-France                             
##  [549] FR-France                             
##  [550] FR-France                             
##  [551] FR-France                             
##  [552] FR-France                             
##  [553] FR-France                             
##  [554] FR-France                             
##  [555] FR-France                             
##  [556] FR-France                             
##  [557] FR-France                             
##  [558] FR-France                             
##  [559] FR-France                             
##  [560] FR-France                             
##  [561] FR-France                             
##  [562] FR-France                             
##  [563] FR-France                             
##  [564] FR-France                             
##  [565] FR-France                             
##  [566] FR-France                             
##  [567] FR-France                             
##  [568] FR-France                             
##  [569] FR-France                             
##  [570] FR-France                             
##  [571] FR-France                             
##  [572] FR-France                             
##  [573] FR-France                             
##  [574] FR-France                             
##  [575] FR-France                             
##  [576] FR-France                             
##  [577] FR-France                             
##  [578] FR-France                             
##  [579] FR-France                             
##  [580] FR-France                             
##  [581] FR-France                             
##  [582] FR-France                             
##  [583] FR-France                             
##  [584] FR-France                             
##  [585] FR-France                             
##  [586] FR-France                             
##  [587] FR-France                             
##  [588] FR-France                             
##  [589] FR-France                             
##  [590] FR-France                             
##  [591] FR-France                             
##  [592] FR-France                             
##  [593] FR-France                             
##  [594] FR-France                             
##  [595] FR-France                             
##  [596] FR-France                             
##  [597] FR-France                             
##  [598] FR-France                             
##  [599] FR-France                             
##  [600] FR-France                             
##  [601] FR-France                             
##  [602] FR-France                             
##  [603] FR-France                             
##  [604] FR-France                             
##  [605] FR-France                             
##  [606] FR-France                             
##  [607] FR-France                             
##  [608] FR-France                             
##  [609] FR-France                             
##  [610] FR-France                             
##  [611] FR-France                             
##  [612] FR-France                             
##  [613] FR-France                             
##  [614] FR-France                             
##  [615] FR-France                             
##  [616] FR-France                             
##  [617] FR-France                             
##  [618] FR-France                             
##  [619] FR-France                             
##  [620] FR-France                             
##  [621] FR-France                             
##  [622] FR-France                             
##  [623] FR-France                             
##  [624] FR-France                             
##  [625] FR-France                             
##  [626] FR-France                             
##  [627] FR-France                             
##  [628] FR-France                             
##  [629] FR-France                             
##  [630] FR-France                             
##  [631] FR-France                             
##  [632] FR-France                             
##  [633] FR-France                             
##  [634] FR-France                             
##  [635] FR-France                             
##  [636] FR-France                             
##  [637] FR-France                             
##  [638] FR-France                             
##  [639] FR-France                             
##  [640] FR-France                             
##  [641] FR-France                             
##  [642] FR-France                             
##  [643] FR-France                             
##  [644] FR-France                             
##  [645] FR-France                             
##  [646] FR-France                             
##  [647] FR-France                             
##  [648] FR-France                             
##  [649] FR-France                             
##  [650] FR-France                             
##  [651] FR-France                             
##  [652] FR-France                             
##  [653] FR-France                             
##  [654] FR-France                             
##  [655] FR-France                             
##  [656] FR-France                             
##  [657] FR-France                             
##  [658] FR-France                             
##  [659] FR-France                             
##  [660] FR-France                             
##  [661] FR-France                             
##  [662] FR-France                             
##  [663] FR-France                             
##  [664] FR-France                             
##  [665] FR-France                             
##  [666] FR-France                             
##  [667] FR-France                             
##  [668] FR-France                             
##  [669] FR-France                             
##  [670] FR-France                             
##  [671] FR-France                             
##  [672] FR-France                             
##  [673] FR-France                             
##  [674] FR-France                             
##  [675] FR-France                             
##  [676] FR-France                             
##  [677] FR-France                             
##  [678] FR-France                             
##  [679] FR-France                             
##  [680] FR-France                             
##  [681] FR-France                             
##  [682] FR-France                             
##  [683] FR-France                             
##  [684] FR-France                             
##  [685] FR-France                             
##  [686] FR-France                             
##  [687] FR-France                             
##  [688] FR-France                             
##  [689] FR-France                             
##  [690] FR-France                             
##  [691] FR-France                             
##  [692] FR-France                             
##  [693] FR-France                             
##  [694] FR-France                             
##  [695] FR-France                             
##  [696] FR-France                             
##  [697] FR-France                             
##  [698] FR-France                             
##  [699] FR-France                             
##  [700] FR-France                             
##  [701] FR-France                             
##  [702] FR-France                             
##  [703] FR-France                             
##  [704] FR-France                             
##  [705] FR-France                             
##  [706] FR-France                             
##  [707] FR-France                             
##  [708] FR-France                             
##  [709] FR-France                             
##  [710] FR-France                             
##  [711] FR-France                             
##  [712] FR-France                             
##  [713] FR-France                             
##  [714] FR-France                             
##  [715] FR-France                             
##  [716] FR-France                             
##  [717] FR-France                             
##  [718] FR-France                             
##  [719] FR-France                             
##  [720] FR-France                             
##  [721] FR-France                             
##  [722] FR-France                             
##  [723] FR-France                             
##  [724] FR-France                             
##  [725] FR-France                             
##  [726] FR-France                             
##  [727] FR-France                             
##  [728] FR-France                             
##  [729] FR-France                             
##  [730] FR-France                             
##  [731] FR-France                             
##  [732] FR-France                             
##  [733] FR-France                             
##  [734] FR-France                             
##  [735] FR-France                             
##  [736] FR-France                             
##  [737] FR-France                             
##  [738] FR-France                             
##  [739] FR-France                             
##  [740] FR-France                             
##  [741] FR-France                             
##  [742] FR-France                             
##  [743] FR-France                             
##  [744] FR-France                             
##  [745] FR-France                             
##  [746] FR-France                             
##  [747] FR-France                             
##  [748] FR-France                             
##  [749] FR-France                             
##  [750] FR-France                             
##  [751] FR-France                             
##  [752] FR-France                             
##  [753] FR-France                             
##  [754] FR-France                             
##  [755] FR-France                             
##  [756] FR-France                             
##  [757] FR-France                             
##  [758] FR-France                             
##  [759] FR-France                             
##  [760] FR-France                             
##  [761] FR-France                             
##  [762] FR-France                             
##  [763] FR-France                             
##  [764] FR-France                             
##  [765] FR-France                             
##  [766] FR-France                             
##  [767] FR-France                             
##  [768] FR-France                             
##  [769] FR-France                             
##  [770] FR-France                             
##  [771] FR-France                             
##  [772] FR-France                             
##  [773] FR-France                             
##  [774] FR-France                             
##  [775] FR-France                             
##  [776] FR-France                             
##  [777] FR-France                             
##  [778] FR-France                             
##  [779] FR-France                             
##  [780] FR-France                             
##  [781] FR-France                             
##  [782] FR-France                             
##  [783] FR-France                             
##  [784] FR-France                             
##  [785] FR-France                             
##  [786] FR-France                             
##  [787] FR-France                             
##  [788] FR-France                             
##  [789] FR-France                             
##  [790] FR-France                             
##  [791] FR-France                             
##  [792] FR-France                             
##  [793] FR-France                             
##  [794] FR-France                             
##  [795] FR-France                             
##  [796] FR-France                             
##  [797] FR-France                             
##  [798] FR-France                             
##  [799] FR-France                             
##  [800] FR-France                             
##  [801] FR-France                             
##  [802] FR-France                             
##  [803] FR-France                             
##  [804] FR-France                             
##  [805] FR-France                             
##  [806] FR-France                             
##  [807] FR-France                             
##  [808] FR-France                             
##  [809] FR-France                             
##  [810] FR-France                             
##  [811] FR-France                             
##  [812] FR-France                             
##  [813] FR-France                             
##  [814] FR-France                             
##  [815] FR-France                             
##  [816] FR-France                             
##  [817] FR-France                             
##  [818] FR-France                             
##  [819] FR-France                             
##  [820] FR-France                             
##  [821] FR-France                             
##  [822] FR-France                             
##  [823] FR-France                             
##  [824] FR-France                             
##  [825] FR-France                             
##  [826] FR-France                             
##  [827] FR-France                             
##  [828] FR-France                             
##  [829] FR-France                             
##  [830] FR-France                             
##  [831] FR-France                             
##  [832] FR-France                             
##  [833] FR-France                             
##  [834] FR-France                             
##  [835] FR-France                             
##  [836] FR-France                             
##  [837] FR-France                             
##  [838] FR-France                             
##  [839] FR-France                             
##  [840] FR-France                             
##  [841] FR-France                             
##  [842] FR-France                             
##  [843] FR-France                             
##  [844] FR-France                             
##  [845] FR-France                             
##  [846] FR-France                             
##  [847] FR-France                             
##  [848] FR-France                             
##  [849] FR-France                             
##  [850] FR-France                             
##  [851] FR-France                             
##  [852] FR-France                             
##  [853] FR-France                             
##  [854] FR-France                             
##  [855] FR-France                             
##  [856] FR-France                             
##  [857] FR-France                             
##  [858] FR-France                             
##  [859] FR-France                             
##  [860] FR-France                             
##  [861] FR-France                             
##  [862] FR-France                             
##  [863] FR-France                             
##  [864] FR-France                             
##  [865] FR-France                             
##  [866] FR-France                             
##  [867] FR-France                             
##  [868] FR-France                             
##  [869] FR-France                             
##  [870] FR-France                             
##  [871] FR-France                             
##  [872] FR-France                             
##  [873] FR-France                             
##  [874] FR-France                             
##  [875] FR-France                             
##  [876] FR-France                             
##  [877] FR-France                             
##  [878] FR-France                             
##  [879] FR-France                             
##  [880] FR-France                             
##  [881] FR-France                             
##  [882] FR-France                             
##  [883] FR-France                             
##  [884] FR-France                             
##  [885] FR-France                             
##  [886] FR-France                             
##  [887] FR-France                             
##  [888] FR-France                             
##  [889] FR-France                             
##  [890] FR-France                             
##  [891] FR-France                             
##  [892] FR-France                             
##  [893] FR-France                             
##  [894] FR-France                             
##  [895] FR-France                             
##  [896] FR-France                             
##  [897] FR-France                             
##  [898] FR-France                             
##  [899] FR-France                             
##  [900] FR-France                             
##  [901] FR-France                             
##  [902] FR-France                             
##  [903] FR-France                             
##  [904] FR-France                             
##  [905] FR-France                             
##  [906] FR-France                             
##  [907] FR-France                             
##  [908] FR-France                             
##  [909] FR-France                             
##  [910] FR-France                             
##  [911] FR-France                             
##  [912] FR-France                             
##  [913] FR-France                             
##  [914] FR-France                             
##  [915] FR-France                             
##  [916] FR-France                             
##  [917] FR-France                             
##  [918] FR-France                             
##  [919] FR-France                             
##  [920] FR-France                             
##  [921] FR-France                             
##  [922] FR-France                             
##  [923] FR-France                             
##  [924] FR-France                             
##  [925] FR-France                             
##  [926] FR-France                             
##  [927] FR-France                             
##  [928] FR-France                             
##  [929] FR-France                             
##  [930] FR-France                             
##  [931] FR-France                             
##  [932] FR-France                             
##  [933] FR-France                             
##  [934] FR-France                             
##  [935] FR-France                             
##  [936] FR-France                             
##  [937] FR-France                             
##  [938] FR-France                             
##  [939] FR-France                             
##  [940] FR-France                             
##  [941] FR-France                             
##  [942] FR-France                             
##  [943] FR-France                             
##  [944] FR-France                             
##  [945] FR-France                             
##  [946] FR-France                             
##  [947] FR-France                             
##  [948] FR-France                             
##  [949] FR-France                             
##  [950] FR-France                             
##  [951] FR-France                             
##  [952] FR-France                             
##  [953] FR-France                             
##  [954] FR-France                             
##  [955] FR-France                             
##  [956] FR-France                             
##  [957] FR-France                             
##  [958] FR-France                             
##  [959] FR-France                             
##  [960] FR-France                             
##  [961] FR-France                             
##  [962] FR-France                             
##  [963] FR-France                             
##  [964] FR-France                             
##  [965] FR-France                             
##  [966] FR-France                             
##  [967] FR-France                             
##  [968] FR-France                             
##  [969] FR-France                             
##  [970] FR-France                             
##  [971] FR-France                             
##  [972] FR-France                             
##  [973] FR-France                             
##  [974] FR-France                             
##  [975] FR-France                             
##  [976] FR-France                             
##  [977] FR-France                             
##  [978] FR-France                             
##  [979] FR-France                             
##  [980] FR-France                             
##  [981] FR-France                             
##  [982] FR-France                             
##  [983] FR-France                             
##  [984] FR-France                             
##  [985] FR-France                             
##  [986] FR-France                             
##  [987] FR-France                             
##  [988] FR-France                             
##  [989] FR-France                             
##  [990] FR-France                             
##  [991] FR-France                             
##  [992] FR-France                             
##  [993] FR-France                             
##  [994] FR-France                             
##  [995] FR-France                             
##  [996] FR-France                             
##  [997] FR-France                             
##  [998] FR-France                             
##  [999] FR-France                             
## [1000] FR-France                             
## [1001] FR-France                             
## [1002] FR-France                             
## [1003] FR-France                             
## [1004] FR-France                             
## [1005] FR-France                             
## [1006] FR-France                             
## [1007] FR-France                             
## [1008] FR-France                             
## [1009] FR-France                             
## [1010] FR-France                             
## [1011] FR-France                             
## [1012] FR-France                             
## [1013] FR-France                             
## [1014] FR-France                             
## [1015] FR-France                             
## [1016] FR-France                             
## [1017] FR-France                             
## [1018] FR-France                             
## [1019] FR-France                             
## [1020] FR-France                             
## [1021] FR-France                             
## [1022] FR-France                             
## [1023] FR-France                             
## [1024] FR-France                             
## [1025] FR-France                             
## [1026] FR-France                             
## [1027] FR-France                             
## [1028] FR-France                             
## [1029] FR-France                             
## [1030] FR-France                             
## [1031] FR-France                             
## [1032] FR-France                             
## [1033] FR-France                             
## [1034] FR-France                             
## [1035] FR-France                             
## [1036] FR-France                             
## [1037] FR-France                             
## [1038] FR-France                             
## [1039] FR-France                             
## [1040] FR-France                             
## [1041] FR-France                             
## [1042] FR-France                             
## [1043] FR-France                             
## [1044] FR-France                             
## [1045] FR-France                             
## [1046] FR-France                             
## [1047] FR-France                             
## [1048] FR-France                             
## [1049] FR-France                             
## [1050] FR-France                             
## [1051] FR-France                             
## [1052] FR-France                             
## [1053] FR-France                             
## [1054] FR-France                             
## [1055] FR-France                             
## [1056] FR-France                             
## [1057] FR-France                             
## [1058] FR-France                             
## [1059] FR-France                             
## [1060] FR-France                             
## [1061] FR-France                             
## [1062] FR-France                             
## [1063] FR-France                             
## [1064] FR-France                             
## [1065] FR-France                             
## [1066] FR-France                             
## [1067] FR-France                             
## [1068] FR-France                             
## [1069] FR-France                             
## [1070] FR-France                             
## [1071] FR-France                             
## [1072] FR-France                             
## [1073] FR-France                             
## [1074] FR-France                             
## [1075] FR-France                             
## [1076] FR-France                             
## [1077] FR-France                             
## [1078] FR-France                             
## [1079] FR-France                             
## [1080] FR-France                             
## [1081] FR-France                             
## [1082] FR-France                             
## [1083] FR-France                             
## [1084] FR-France                             
## [1085] FR-France                             
## [1086] FR-France                             
## [1087] FR-France                             
## [1088] FR-France                             
## [1089] FR-France                             
## [1090] FR-France                             
## [1091] FR-France                             
## [1092] FR-France                             
## [1093] FR-France                             
## [1094] FR-France                             
## [1095] FR-France                             
## [1096] FR-France                             
## [1097] FR-France                             
## [1098] FR-France                             
## [1099] FR-France                             
## [1100] FR-France                             
## [1101] FR-France                             
## [1102] FR-France                             
## [1103] FR-France                             
## [1104] FR-France                             
## [1105] FR-France                             
## [1106] FR-France                             
## [1107] FR-France                             
## [1108] FR-France                             
## [1109] FR-France                             
## [1110] FR-France                             
## [1111] FR-France                             
## [1112] FR-France                             
## [1113] FR-France                             
## [1114] FR-France                             
## [1115] FR-France                             
## [1116] FR-France                             
## [1117] FR-France                             
## [1118] FR-France                             
## [1119] FR-France                             
## [1120] FR-France                             
## [1121] FR-France                             
## [1122] FR-France                             
## [1123] FR-France                             
## [1124] FR-France                             
## [1125] FR-France                             
## [1126] FR-France                             
## [1127] FR-France                             
## [1128] FR-France                             
## [1129] FR-France                             
## [1130] FR-France                             
## [1131] FR-France                             
## [1132] FR-France                             
## [1133] FR-France                             
## [1134] FR-France                             
## [1135] FR-France                             
## [1136] FR-France                             
## [1137] FR-France                             
## [1138] FR-France                             
## [1139] FR-France                             
## [1140] FR-France                             
## [1141] FR-France                             
## [1142] FR-France                             
## [1143] FR-France                             
## [1144] FR-France                             
## [1145] FR-France                             
## [1146] FR-France                             
## [1147] FR-France                             
## [1148] FR-France                             
## [1149] FR-France                             
## [1150] FR-France                             
## [1151] FR-France                             
## [1152] FR-France                             
## [1153] FR-France                             
## [1154] FR-France                             
## [1155] FR-France                             
## [1156] FR-France                             
## [1157] FR-France                             
## [1158] FR-France                             
## [1159] FR-France                             
## [1160] FR-France                             
## [1161] FR-France                             
## [1162] FR-France                             
## [1163] FR-France                             
## [1164] FR-France                             
## [1165] FR-France                             
## [1166] FR-France                             
## [1167] FR-France                             
## [1168] FR-France                             
## [1169] FR-France                             
## [1170] FR-France                             
## [1171] FR-France                             
## [1172] FR-France                             
## [1173] FR-France                             
## [1174] FR-France                             
## [1175] FR-France                             
## [1176] FR-France                             
## [1177] FR-France                             
## [1178] FR-France                             
## [1179] FR-France                             
## [1180] FR-France                             
## [1181] FR-France                             
## [1182] FR-France                             
## [1183] FR-France                             
## [1184] FR-France                             
## [1185] FR-France                             
## [1186] FR-France                             
## [1187] FR-France                             
## [1188] FR-France                             
## [1189] FR-France                             
## [1190] FR-France                             
## [1191] FR-France                             
## [1192] FR-France                             
## [1193] FR-France                             
## [1194] FR-France                             
## [1195] FR-France                             
## [1196] FR-France                             
## [1197] FR-France                             
## [1198] FR-France                             
## [1199] FR-France                             
## [1200] FR-France                             
## [1201] FR-France                             
## [1202] FR-France                             
## [1203] FR-France                             
## [1204] FR-France                             
## [1205] FR-France                             
## [1206] FR-France                             
## [1207] FR-France                             
## [1208] FR-France                             
## [1209] FR-France                             
## [1210] FR-France                             
## [1211] FR-France                             
## [1212] FR-France                             
## [1213] FR-France                             
## [1214] FR-France                             
## [1215] FR-France                             
## [1216] FR-France                             
## [1217] FR-France                             
## [1218] FR-France                             
## [1219] FR-France                             
## [1220] FR-France                             
## [1221] FR-France                             
## [1222] FR-France                             
## [1223] FR-France                             
## [1224] FR-France                             
## [1225] FR-France                             
## [1226] FR-France                             
## [1227] FR-France                             
## [1228] FR-France                             
## [1229] FR-France                             
## [1230] FR-France                             
## [1231] FR-France                             
## [1232] FR-France                             
## [1233] FR-France                             
## [1234] FR-France                             
## [1235] FR-France                             
## [1236] FR-France                             
## [1237] FR-France                             
## [1238] FR-France                             
## [1239] FR-France                             
## [1240] FR-France                             
## [1241] FR-France                             
## [1242] FR-France                             
## [1243] FR-France                             
## [1244] FR-France                             
## [1245] FR-France                             
## [1246] FR-France                             
## [1247] FR-France                             
## [1248] FR-France                             
## [1249] FR-France                             
## [1250] FR-France                             
## [1251] FR-France                             
## [1252] FR-France                             
## [1253] FR-France                             
## [1254] FR-France                             
## [1255] FR-France                             
## [1256] FR-France                             
## [1257] FR-France                             
## [1258] FR-France                             
## [1259] FR-France                             
## [1260] FR-France                             
## [1261] FR-France                             
## [1262] FR-France                             
## [1263] FR-France                             
## [1264] FR-France                             
## [1265] FR-France                             
## [1266] FR-France                             
## [1267] FR-France                             
## [1268] FR-France                             
## [1269] FR-France                             
## [1270] FR-France                             
## [1271] FR-France                             
## [1272] FR-France                             
## [1273] FR-France                             
## [1274] FR-France                             
## [1275] FR-France                             
## [1276] FR-France                             
## [1277] FR-France                             
## [1278] FR-France                             
## [1279] FR-France                             
## [1280] FR-France                             
## [1281] FR-France                             
## [1282] FR-France                             
## [1283] FR-France                             
## [1284] FR-France                             
## [1285] FR-France                             
## [1286] FR-France                             
## [1287] FR-France                             
## [1288] FR-France                             
## [1289] FR-France                             
## [1290] FR-France                             
## [1291] FR-France                             
## [1292] FR-France                             
## [1293] FR-France                             
## [1294] FR-France                             
## [1295] FR-France                             
## [1296] FR-France                             
## [1297] FR-France                             
## [1298] FR-France                             
## [1299] FR-France                             
## [1300] FR-France                             
## [1301] FR-France                             
## [1302] FR-France                             
## [1303] FR-France                             
## [1304] FR-France                             
## [1305] FR-France                             
## [1306] FR-France                             
## [1307] FR-France                             
## [1308] FR-France                             
## [1309] FR-France                             
## [1310] FR-France                             
## [1311] FR-France                             
## [1312] FR-France                             
## [1313] FR-France                             
## [1314] FR-France                             
## [1315] FR-France                             
## [1316] FR-France                             
## [1317] FR-France                             
## [1318] FR-France                             
## [1319] FR-France                             
## [1320] FR-France                             
## [1321] FR-France                             
## [1322] FR-France                             
## [1323] FR-France                             
## [1324] FR-France                             
## [1325] FR-France                             
## [1326] FR-France                             
## [1327] FR-France                             
## [1328] FR-France                             
## [1329] FR-France                             
## [1330] FR-France                             
## [1331] FR-France                             
## [1332] FR-France                             
## [1333] FR-France                             
## [1334] FR-France                             
## [1335] FR-France                             
## [1336] FR-France                             
## [1337] FR-France                             
## [1338] FR-France                             
## [1339] FR-France                             
## [1340] FR-France                             
## [1341] FR-France                             
## [1342] FR-France                             
## [1343] FR-France                             
## [1344] FR-France                             
## [1345] FR-France                             
## [1346] FR-France                             
## [1347] FR-France                             
## [1348] FR-France                             
## [1349] FR-France                             
## [1350] FR-France                             
## [1351] FR-France                             
## [1352] FR-France                             
## [1353] FR-France                             
## [1354] FR-France                             
## [1355] FR-France                             
## [1356] FR-France                             
## [1357] FR-France                             
## [1358] FR-France                             
## [1359] FR-France                             
## [1360] FR-France                             
## [1361] FR-France                             
## [1362] FR-France                             
## [1363] FR-France                             
## [1364] FR-France                             
## [1365] FR-France                             
## [1366] FR-France                             
## [1367] FR-France                             
## [1368] FR-France                             
## [1369] FR-France                             
## [1370] FR-France                             
## [1371] FR-France                             
## [1372] FR-France                             
## [1373] FR-France                             
## [1374] FR-France                             
## [1375] FR-France                             
## [1376] FR-France                             
## [1377] FR-France                             
## [1378] FR-France                             
## [1379] FR-France                             
## [1380] FR-France                             
## [1381] FR-France                             
## [1382] FR-France                             
## [1383] FR-France                             
## [1384] FR-France                             
## [1385] FR-France                             
## [1386] FR-France                             
## [1387] FR-France                             
## [1388] FR-France                             
## [1389] FR-France                             
## [1390] FR-France                             
## [1391] FR-France                             
## [1392] FR-France                             
## [1393] FR-France                             
## [1394] FR-France                             
## [1395] FR-France                             
## [1396] FR-France                             
## [1397] FR-France                             
## [1398] FR-France                             
## [1399] FR-France                             
## [1400] FR-France                             
## [1401] FR-France                             
## [1402] FR-France                             
## [1403] FR-France                             
## [1404] FR-France                             
## [1405] FR-France                             
## [1406] FR-France                             
## [1407] FR-France                             
## [1408] FR-France                             
## [1409] FR-France                             
## [1410] FR-France                             
## [1411] FR-France                             
## [1412] FR-France                             
## [1413] FR-France                             
## [1414] FR-France                             
## [1415] FR-France                             
## [1416] FR-France                             
## [1417] FR-France                             
## [1418] FR-France                             
## [1419] FR-France                             
## [1420] FR-France                             
## [1421] FR-France                             
## [1422] FR-France                             
## [1423] FR-France                             
## [1424] FR-France                             
## [1425] FR-France                             
## [1426] FR-France                             
## [1427] FR-France                             
## [1428] FR-France                             
## [1429] FR-France                             
## [1430] FR-France                             
## [1431] FR-France                             
## [1432] FR-France                             
## [1433] FR-France                             
## [1434] FR-France                             
## [1435] FR-France                             
## [1436] FR-France                             
## [1437] FR-France                             
## [1438] FR-France                             
## [1439] FR-France                             
## [1440] FR-France                             
## [1441] FR-France                             
## [1442] FR-France                             
## [1443] FR-France                             
## [1444] FR-France                             
## [1445] FR-France                             
## [1446] FR-France                             
## [1447] FR-France                             
## [1448] FR-France                             
## [1449] FR-France                             
## [1450] FR-France                             
## [1451] FR-France                             
## [1452] FR-France                             
## [1453] FR-France                             
## [1454] FR-France                             
## [1455] FR-France                             
## [1456] FR-France                             
## [1457] FR-France                             
## [1458] FR-France                             
## [1459] FR-France                             
## [1460] FR-France                             
## [1461] FR-France                             
## [1462] FR-France                             
## [1463] FR-France                             
## [1464] FR-France                             
## [1465] FR-France                             
## [1466] FR-France                             
## [1467] FR-France                             
## [1468] FR-France                             
## [1469] FR-France                             
## [1470] FR-France                             
## [1471] FR-France                             
## [1472] FR-France                             
## [1473] FR-France                             
## [1474] FR-France                             
## [1475] FR-France                             
## [1476] FR-France                             
## [1477] FR-France                             
## [1478] FR-France                             
## [1479] FR-France                             
## [1480] FR-France                             
## [1481] FR-France                             
## [1482] FR-France                             
## [1483] FR-France                             
## [1484] FR-France                             
## [1485] FR-France                             
## [1486] FR-France                             
## [1487] FR-France                             
## [1488] FR-France                             
## [1489] FR-France                             
## [1490] FR-France                             
## [1491] FR-France                             
## [1492] FR-France                             
## [1493] FR-France                             
## [1494] FR-France                             
## [1495] FR-France                             
## [1496] FR-France                             
## [1497] FR-France                             
## [1498] FR-France                             
## [1499] FR-France                             
## [1500] FR-France                             
## [1501] FR-France                             
## [1502] FR-France                             
## [1503] FR-France                             
## [1504] FR-France                             
## [1505] FR-France                             
## [1506] FR-France                             
## [1507] FR-France                             
## [1508] FR-France                             
## [1509] FR-France                             
## [1510] FR-France                             
## [1511] FR-France                             
## [1512] FR-France                             
## [1513] FR-France                             
## [1514] FR-France                             
## [1515] FR-France                             
## [1516] FR-France                             
## [1517] FR-France                             
## [1518] FR-France                             
## [1519] FR-France                             
## [1520] FR-France                             
## [1521] FR-France                             
## [1522] FR-France                             
## [1523] FR-France                             
## [1524] FR-France                             
## [1525] FR-France                             
## [1526] FR-France                             
## [1527] FR-France                             
## [1528] FR-France                             
## [1529] FR-France                             
## [1530] FR-France                             
## [1531] FR-France                             
## [1532] FR-France                             
## [1533] FR-France                             
## [1534] FR-France                             
## [1535] FR-France                             
## [1536] FR-France                             
## [1537] FR-France                             
## [1538] FR-France                             
## [1539] FR-France                             
## [1540] FR-France                             
## [1541] FR-France                             
## [1542] FR-France                             
## [1543] FR-France                             
## [1544] FR-France                             
## [1545] FR-France                             
## [1546] FR-France                             
## [1547] FR-France                             
## [1548] FR-France                             
## [1549] FR-France                             
## [1550] FR-France                             
## [1551] FR-France                             
## [1552] FR-France                             
## [1553] FR-France                             
## [1554] FR-France                             
## [1555] FR-France                             
## [1556] FR-France                             
## [1557] FR-France                             
## [1558] FR-France                             
## [1559] FR-France                             
## [1560] FR-France                             
## [1561] FR-France                             
## [1562] FR-France                             
## [1563] FR-France                             
## [1564] FR-France                             
## [1565] FR-France                             
## [1566] FR-France                             
## [1567] FR-France                             
## [1568] FR-France                             
## [1569] FR-France                             
## [1570] FR-France                             
## [1571] FR-France                             
## [1572] FR-France                             
## [1573] FR-France                             
## [1574] FR-France                             
## [1575] FR-France                             
## [1576] FR-France                             
## [1577] FR-France                             
## [1578] FR-France                             
## [1579] FR-France                             
## [1580] FR-France                             
## [1581] FR-France                             
## [1582] FR-France                             
## [1583] FR-France                             
## [1584] FR-France                             
## [1585] FR-France                             
## [1586] FR-France                             
## [1587] FR-France                             
## [1588] FR-France                             
## [1589] FR-France                             
## [1590] FR-France                             
## [1591] FR-France                             
## [1592] FR-France                             
## [1593] FR-France                             
## [1594] FR-France                             
## [1595] FR-France                             
## [1596] FR-France                             
## [1597] FR-France                             
## [1598] FR-France                             
## [1599] FR-France                             
## [1600] FR-France                             
## [1601] FR-France                             
## [1602] FR-France                             
## [1603] FR-France                             
## [1604] FR-France                             
## [1605] FR-France                             
## [1606] FR-France                             
## [1607] FR-France                             
## [1608] FR-France                             
## [1609] FR-France                             
## [1610] FR-France                             
## [1611] FR-France                             
## [1612] FR-France                             
## [1613] FR-France                             
## [1614] FR-France                             
## [1615] FR-France                             
## [1616] FR-France                             
## [1617] FR-France                             
## [1618] FR-France                             
## [1619] FR-France                             
## [1620] FR-France                             
## [1621] FR-France                             
## [1622] FR-France                             
## [1623] FR-France                             
## [1624] FR-France                             
## [1625] FR-France                             
## [1626] FR-France                             
## [1627] FR-France                             
## [1628] FR-France                             
## [1629] FR-France                             
## [1630] FR-France                             
## [1631] FR-France                             
## [1632] FR-France                             
## [1633] FR-France                             
## [1634] FR-France                             
## [1635] FR-France                             
## [1636] FR-France                             
## [1637] FR-France                             
## [1638] FR-France                             
## [1639] FR-France                             
## [1640] FR-France                             
## [1641] FR-France                             
## [1642] FR-France                             
## [1643] FR-France                             
## [1644] FR-France                             
## [1645] FR-France                             
## [1646] FR-France                             
## [1647] FR-France                             
## [1648] FR-France                             
## [1649] FR-France                             
## [1650] FR-France                             
## [1651] FR-France                             
## [1652] FR-France                             
## [1653] FR-France                             
## [1654] FR-France                             
## [1655] FR-France                             
## [1656] FR-France                             
## [1657] FR-France                             
## [1658] FR-France                             
## [1659] FR-France                             
## [1660] FR-France                             
## [1661] FR-France                             
## [1662] FR-France                             
## [1663] FR-France                             
## [1664] FR-France                             
## [1665] FR-France                             
## [1666] FR-France                             
## [1667] FR-France                             
## [1668] FR-France                             
## [1669] FR-France                             
## [1670] FR-France                             
## [1671] FR-France                             
## [1672] FR-France                             
## [1673] FR-France                             
## [1674] FR-France                             
## [1675] FR-France                             
## [1676] FR-France                             
## [1677] FR-France                             
## [1678] FR-France                             
## [1679] FR-France                             
## [1680] FR-France                             
## [1681] FR-France                             
## [1682] FR-France                             
## [1683] FR-France                             
## [1684] FR-France                             
## [1685] FR-France                             
## [1686] FR-France                             
## [1687] FR-France                             
## [1688] FR-France                             
## [1689] FR-France                             
## [1690] FR-France                             
## [1691] FR-France                             
## [1692] FR-France                             
## [1693] FR-France                             
## [1694] FR-France                             
## [1695] FR-France                             
## [1696] FR-France                             
## [1697] FR-France                             
## [1698] FR-France                             
## [1699] FR-France                             
## [1700] FR-France                             
## [1701] FR-France                             
## [1702] FR-France                             
## [1703] FR-France                             
## [1704] FR-France                             
## [1705] FR-France                             
## [1706] FR-France                             
## [1707] FR-France                             
## [1708] FR-France                             
## [1709] FR-France                             
## [1710] FR-France                             
## [1711] FR-France                             
## [1712] FR-France                             
## [1713] FR-France                             
## [1714] FR-France                             
## [1715] FR-France                             
## [1716] FR-France                             
## [1717] FR-France                             
## [1718] FR-France                             
## [1719] FR-France                             
## [1720] FR-France                             
## [1721] FR-France                             
## [1722] FR-France                             
## [1723] FR-France                             
## [1724] FR-France                             
## [1725] FR-France                             
## [1726] FR-France                             
## [1727] FR-France                             
## [1728] FR-France                             
## [1729] FR-France                             
## [1730] FR-France                             
## [1731] FR-France                             
## [1732] FR-France                             
## [1733] FR-France                             
## [1734] FR-France                             
## [1735] FR-France                             
## [1736] FR-France                             
## [1737] FR-France                             
## [1738] FR-France                             
## [1739] FR-France                             
## [1740] FR-France                             
## [1741] FR-France                             
## [1742] FR-France                             
## [1743] FR-France                             
## [1744] FR-France                             
## [1745] FR-France                             
## [1746] FR-France                             
## [1747] FR-France                             
## [1748] FR-France                             
## [1749] FR-France                             
## [1750] FR-France                             
## [1751] FR-France                             
## [1752] FR-France                             
## [1753] FR-France                             
## [1754] FR-France                             
## [1755] FR-France                             
## [1756] FR-France                             
## [1757] FR-France                             
## [1758] FR-France                             
## [1759] FR-France                             
## [1760] FR-France                             
## [1761] FR-France                             
## [1762] FR-France                             
## [1763] FR-France                             
## [1764] FR-France                             
## [1765] FR-France                             
## [1766] FR-France                             
## [1767] FR-France                             
## [1768] FR-France                             
## [1769] FR-France                             
## [1770] FR-France                             
## [1771] FR-France                             
## [1772] FR-France                             
## [1773] FR-France                             
## [1774] FR-France                             
## [1775] FR-France                             
## [1776] FR-France                             
## [1777] FR-France                             
## [1778] FR-France                             
## [1779] FR-France                             
## [1780] FR-France                             
## [1781] FR-France                             
## [1782] FR-France                             
## [1783] FR-France                             
## [1784] FR-France                             
## [1785] FR-France                             
## [1786] FR-France                             
## [1787] FR-France                             
## [1788] FR-France                             
## [1789] FR-France                             
## [1790] FR-France                             
## [1791] FR-France                             
## [1792] FR-France                             
## [1793] FR-France                             
## [1794] FR-France                             
## [1795] FR-France                             
## [1796] FR-France                             
## [1797] FR-France                             
## [1798] FR-France                             
## [1799] FR-France                             
## [1800] FR-France                             
## [1801] FR-France                             
## [1802] FR-France                             
## [1803] FR-France                             
## [1804] FR-France                             
## [1805] FR-France                             
## [1806] FR-France                             
## [1807] FR-France                             
## [1808] FR-France                             
## [1809] FR-France                             
## [1810] FR-France                             
## [1811] FR-France                             
## [1812] FR-France                             
## [1813] FR-France                             
## [1814] FR-France                             
## [1815] FR-France                             
## [1816] FR-France                             
## [1817] FR-France                             
## [1818] FR-France                             
## [1819] FR-France                             
## [1820] FR-France                             
## [1821] FR-France                             
## [1822] FR-France                             
## [1823] FR-France                             
## [1824] FR-France                             
## [1825] FR-France                             
## [1826] FR-France                             
## [1827] FR-France                             
## [1828] FR-France                             
## [1829] FR-France                             
## [1830] FR-France                             
## [1831] FR-France                             
## [1832] FR-France                             
## [1833] FR-France                             
## [1834] FR-France                             
## [1835] FR-France                             
## [1836] FR-France                             
## [1837] FR-France                             
## [1838] FR-France                             
## [1839] FR-France                             
## [1840] FR-France                             
## [1841] FR-France                             
## [1842] FR-France                             
## [1843] FR-France                             
## [1844] FR-France                             
## [1845] FR-France                             
## [1846] FR-France                             
## [1847] FR-France                             
## [1848] FR-France                             
## [1849] FR-France                             
## [1850] FR-France                             
## [1851] FR-France                             
## [1852] FR-France                             
## [1853] FR-France                             
## [1854] FR-France                             
## [1855] FR-France                             
## [1856] FR-France                             
## [1857] FR-France                             
## [1858] FR-France                             
## [1859] FR-France                             
## [1860] FR-France                             
## [1861] FR-France                             
## [1862] FR-France                             
## [1863] FR-France                             
## [1864] FR-France                             
## [1865] FR-France                             
## [1866] FR-France                             
## [1867] FR-France                             
## [1868] FR-France                             
## [1869] FR-France                             
## [1870] FR-France                             
## [1871] FR-France                             
## [1872] FR-France                             
## [1873] FR-France                             
## [1874] FR-France                             
## [1875] FR-France                             
## [1876] FR-France                             
## [1877] FR-France                             
## [1878] FR-France                             
## [1879] FR-France                             
## [1880] FR-France                             
## [1881] FR-France                             
## [1882] FR-France                             
## [1883] FR-France                             
## [1884] FR-France                             
## [1885] FR-France                             
## [1886] FR-France                             
## [1887] FR-France                             
## [1888] FR-France                             
## [1889] FR-France                             
## [1890] FR-France                             
## [1891] FR-France                             
## [1892] FR-France                             
## [1893] FR-France                             
## [1894] FR-France                             
## [1895] FR-France                             
## [1896] FR-France                             
## [1897] FR-France                             
## [1898] FR-France                             
## [1899] FR-France                             
## [1900] FR-France                             
## [1901] FR-France                             
## [1902] FR-France                             
## [1903] FR-France                             
## [1904] FR-France                             
## [1905] FR-France                             
## [1906] FR-France                             
## [1907] FR-France                             
## [1908] FR-France                             
## [1909] FR-France                             
## [1910] FR-France                             
## [1911] FR-France                             
## [1912] FR-France                             
## [1913] FR-France                             
## [1914] FR-France                             
## [1915] FR-France                             
## [1916] FR-France                             
## [1917] FR-France                             
## [1918] FR-France                             
## [1919] FR-France                             
## [1920] FR-France                             
## [1921] FR-France                             
## [1922] FR-France                             
## [1923] FR-France                             
## [1924] FR-France                             
## [1925] FR-France                             
## [1926] FR-France                             
## [1927] FR-France                             
## [1928] FR-France                             
## [1929] FR-France                             
## [1930] FR-France                             
## [1931] FR-France                             
## [1932] FR-France                             
## [1933] FR-France                             
## [1934] FR-France                             
## [1935] FR-France                             
## [1936] FR-France                             
## [1937] FR-France                             
## [1938] FR-France                             
## [1939] FR-France                             
## [1940] FR-France                             
## [1941] FR-France                             
## [1942] FR-France                             
## [1943] FR-France                             
## [1944] FR-France                             
## [1945] FR-France                             
## [1946] FR-France                             
## [1947] FR-France                             
## [1948] FR-France                             
## [1949] FR-France                             
## [1950] FR-France                             
## [1951] FR-France                             
## [1952] FR-France                             
## [1953] FR-France                             
## [1954] FR-France                             
## [1955] FR-France                             
## [1956] FR-France                             
## [1957] FR-France                             
## [1958] FR-France                             
## [1959] FR-France                             
## [1960] FR-France                             
## [1961] FR-France                             
## [1962] FR-France                             
## [1963] FR-France                             
## [1964] FR-France                             
## [1965] FR-France                             
## [1966] FR-France                             
## [1967] FR-France                             
## [1968] FR-France                             
## [1969] FR-France                             
## [1970] FR-France                             
## [1971] FR-France                             
## [1972] FR-France                             
## [1973] FR-France                             
## [1974] FR-France                             
## [1975] FR-France                             
## [1976] FR-France                             
## [1977] FR-France                             
## [1978] FR-France                             
## [1979] FR-France                             
## [1980] FR-France                             
## [1981] FR-France                             
## [1982] FR-France                             
## [1983] FR-France                             
## [1984] FR-France                             
## [1985] FR-France                             
## [1986] FR-France                             
## [1987] FR-France                             
## [1988] FR-France                             
## [1989] FR-France                             
## [1990] FR-France                             
## [1991] FR-France                             
## [1992] FR-France                             
## [1993] FR-France                             
## [1994] FR-France                             
## [1995] FR-France                             
## [1996] FR-France                             
## [1997] FR-France                             
## [1998] FR-France                             
## [1999] FR-France                             
## [2000] FR-France                             
## [2001] FR-France                             
## [2002] FR-France                             
## [2003] FR-France                             
## [2004] FR-France                             
## [2005] FR-France                             
## [2006] FR-France                             
## [2007] FR-France                             
## [2008] FR-France                             
## [2009] FR-France                             
## [2010] FR-France                             
## [2011] FR-France                             
## [2012] FR-France                             
## [2013] FR-France                             
## [2014] FR-France                             
## [2015] FR-France                             
## [2016] FR-France                             
## [2017] FR-France                             
## [2018] FR-France                             
## [2019] FR-France                             
## [2020] FR-France                             
## [2021] FR-France                             
## [2022] FR-France                             
## [2023] FR-France                             
## [2024] FR-France                             
## [2025] FR-France                             
## [2026] FR-France                             
## [2027] FR-France                             
## [2028] FR-France                             
## [2029] FR-France                             
## [2030] FR-France                             
## [2031] FR-France                             
## [2032] FR-France                             
## [2033] FR-France                             
## [2034] FR-France                             
## [2035] FR-France                             
## [2036] FR-France                             
## [2037] FR-France                             
## [2038] FR-France                             
## [2039] FR-France                             
## [2040] FR-France                             
## [2041] FR-France                             
## [2042] FR-France                             
## [2043] FR-France                             
## [2044] FR-France                             
## [2045] FR-France                             
## [2046] FR-France                             
## [2047] FR-France                             
## [2048] FR-France                             
## [2049] FR-France                             
## [2050] FR-France                             
## [2051] FR-France                             
## [2052] FR-France                             
## [2053] FR-France                             
## [2054] FR-France                             
## [2055] FR-France                             
## [2056] FR-France                             
## [2057] FR-France                             
## [2058] FR-France                             
## [2059] FR-France                             
## [2060] FR-France                             
## [2061] FR-France                             
## [2062] FR-France                             
## [2063] FR-France                             
## [2064] FR-France                             
## [2065] FR-France                             
## [2066] FR-France                             
## [2067] FR-France                             
## [2068] FR-France                             
## [2069] FR-France                             
## [2070] FR-France                             
## [2071] FR-France                             
## [2072] FR-France                             
## [2073] FR-France                             
## [2074] FR-France                             
## [2075] FR-France                             
## [2076] FR-France                             
## [2077] FR-France                             
## [2078] FR-France                             
## [2079] FR-France                             
## [2080] FR-France                             
## [2081] FR-France                             
## [2082] FR-France                             
## [2083] FR-France                             
## [2084] FR-France                             
## [2085] FR-France                             
## [2086] FR-France                             
## [2087] FR-France                             
## [2088] FR-France                             
## [2089] FR-France                             
## [2090] FR-France                             
## [2091] FR-France                             
## [2092] FR-France                             
## [2093] FR-France                             
## [2094] FR-France                             
## [2095] FR-France                             
## [2096] FR-France                             
## [2097] FR-France                             
## [2098] FR-France                             
## [2099] FR-France                             
## [2100] FR-France                             
## [2101] FR-France                             
## [2102] FR-France                             
## [2103] FR-France                             
## [2104] FR-France                             
## [2105] FR-France                             
## [2106] FR-France                             
## [2107] FR-France                             
## [2108] FR-France                             
## [2109] FR-France                             
## [2110] FR-France                             
## [2111] FR-France                             
## [2112] FR-France                             
## [2113] FR-France                             
## [2114] FR-France                             
## [2115] FR-France                             
## [2116] FR-France                             
## [2117] FR-France                             
## [2118] FR-France                             
## [2119] FR-France                             
## [2120] FR-France                             
## [2121] FR-France                             
## [2122] FR-France                             
## [2123] FR-France                             
## [2124] FR-France                             
## [2125] FR-France                             
## [2126] FR-France                             
## [2127] FR-France                             
## [2128] FR-France                             
## [2129] FR-France                             
## [2130] FR-France                             
## [2131] FR-France                             
## [2132] FR-France                             
## [2133] FR-France                             
## [2134] FR-France                             
## [2135] FR-France                             
## [2136] FR-France                             
## [2137] FR-France                             
## [2138] FR-France                             
## [2139] FR-France                             
## [2140] FR-France                             
## [2141] FR-France                             
## [2142] FR-France                             
## [2143] FR-France                             
## [2144] FR-France                             
## [2145] FR-France                             
## [2146] FR-France                             
## [2147] FR-France                             
## [2148] FR-France                             
## [2149] FR-France                             
## [2150] FR-France                             
## [2151] FR-France                             
## [2152] FR-France                             
## [2153] FR-France                             
## [2154] FR-France                             
## [2155] FR-France                             
## [2156] FR-France                             
## [2157] FR-France                             
## [2158] FR-France                             
## [2159] FR-France                             
## [2160] FR-France                             
## [2161] FR-France                             
## [2162] FR-France                             
## [2163] FR-France                             
## [2164] FR-France                             
## [2165] FR-France                             
## [2166] FR-France                             
## [2167] FR-France                             
## [2168] FR-France                             
## [2169] FR-France                             
## [2170] FR-France                             
## [2171] FR-France                             
## [2172] FR-France                             
## [2173] FR-France                             
## [2174] FR-France                             
## [2175] FR-France                             
## [2176] FR-France                             
## [2177] FR-France                             
## [2178] FR-France                             
## [2179] FR-France                             
## [2180] FR-France                             
## [2181] FR-France                             
## [2182] FR-France                             
## [2183] FR-France                             
## [2184] FR-France                             
## [2185] FR-France                             
## [2186] FR-France                             
## [2187] FR-France                             
## [2188] FR-France                             
## [2189] FR-France                             
## [2190] FR-France                             
## [2191] FR-France                             
## [2192] FR-France                             
## [2193] FR-France                             
## [2194] FR-France                             
## [2195] FR-France                             
## [2196] FR-France                             
## [2197] FR-France                             
## [2198] FR-France                             
## [2199] FR-France                             
## [2200] FR-France                             
## [2201] FR-France                             
## [2202] FR-France                             
## [2203] FR-France                             
## [2204] FR-France                             
## [2205] FR-France                             
## [2206] FR-France                             
## [2207] FR-France                             
## [2208] FR-France                             
## [2209] FR-France                             
## [2210] FR-France                             
## [2211] FR-France                             
## [2212] FR-France                             
## [2213] FR-France                             
## [2214] FR-France                             
## [2215] FR-France                             
## [2216] FR-France                             
## [2217] FR-France                             
## [2218] FR-France                             
## [2219] FR-France                             
## [2220] FR-France                             
## [2221] FR-France                             
## [2222] FR-France                             
## [2223] FR-France                             
## [2224] FR-France                             
## [2225] FR-France                             
## [2226] FR-France                             
## [2227] FR-France                             
## [2228] FR-France                             
## [2229] FR-France                             
## [2230] FR-France                             
## [2231] FR-France                             
## [2232] FR-France                             
## [2233] FR-France                             
## [2234] FR-France                             
## [2235] FR-France                             
## [2236] FR-France                             
## [2237] FR-France                             
## [2238] FR-France                             
## [2239] FR-France                             
## [2240] FR-France                             
## [2241] FR-France                             
## [2242] FR-France                             
## [2243] FR-France                             
## [2244] FR-France                             
## [2245] FR-France                             
## [2246] FR-France                             
## [2247] FR-France                             
## [2248] FR-France                             
## [2249] FR-France                             
## [2250] FR-France                             
## [2251] FR-France                             
## [2252] FR-France                             
## [2253] FR-France                             
## [2254] FR-France                             
## [2255] FR-France                             
## [2256] FR-France                             
## [2257] FR-France                             
## [2258] FR-France                             
## [2259] FR-France                             
## [2260] FR-France                             
## [2261] FR-France                             
## [2262] FR-France                             
## [2263] FR-France                             
## [2264] FR-France                             
## [2265] FR-France                             
## [2266] FR-France                             
## [2267] FR-France                             
## [2268] FR-France                             
## [2269] FR-France                             
## [2270] FR-France                             
## [2271] FR-France                             
## [2272] FR-France                             
## [2273] FR-France                             
## [2274] FR-France                             
## [2275] FR-France                             
## [2276] FR-France                             
## [2277] FR-France                             
## [2278] FR-France                             
## [2279] FR-France                             
## [2280] FR-France                             
## [2281] FR-France                             
## [2282] FR-France                             
## [2283] FR-France                             
## [2284] FR-France                             
## [2285] FR-France                             
## [2286] FR-France                             
## [2287] FR-France                             
## [2288] FR-France                             
## [2289] FR-France                             
## [2290] FR-France                             
## [2291] FR-France                             
## [2292] FR-France                             
## [2293] FR-France                             
## [2294] FR-France                             
## [2295] FR-France                             
## [2296] KR-Korea (South)                      
## [2297] KR-Korea (South)                      
## [2298] KR-Korea (South)                      
## [2299] KR-Korea (South)                      
## [2300] KR-Korea (South)                      
## [2301] KR-Korea (South)                      
## [2302] KR-Korea (South)                      
## [2303] KR-Korea (South)                      
## [2304] KR-Korea (South)                      
## [2305] KR-Korea (South)                      
## [2306] KR-Korea (South)                      
## [2307] KR-Korea (South)                      
## [2308] KR-Korea (South)                      
## [2309] KR-Korea (South)                      
## [2310] KR-Korea (South)                      
## [2311] KR-Korea (South)                      
## [2312] KR-Korea (South)                      
## [2313] KR-Korea (South)                      
## [2314] KR-Korea (South)                      
## [2315] KR-Korea (South)                      
## [2316] KR-Korea (South)                      
## [2317] KR-Korea (South)                      
## [2318] KR-Korea (South)                      
## [2319] KR-Korea (South)                      
## [2320] KR-Korea (South)                      
## [2321] KR-Korea (South)                      
## [2322] KR-Korea (South)                      
## [2323] KR-Korea (South)                      
## [2324] KR-Korea (South)                      
## [2325] KR-Korea (South)                      
## [2326] KR-Korea (South)                      
## [2327] KR-Korea (South)                      
## [2328] KR-Korea (South)                      
## [2329] KR-Korea (South)                      
## [2330] KR-Korea (South)                      
## [2331] KR-Korea (South)                      
## [2332] KR-Korea (South)                      
## [2333] KR-Korea (South)                      
## [2334] KR-Korea (South)                      
## [2335] KR-Korea (South)                      
## [2336] KR-Korea (South)                      
## [2337] KR-Korea (South)                      
## [2338] KR-Korea (South)                      
## [2339] KR-Korea (South)                      
## [2340] KR-Korea (South)                      
## [2341] KR-Korea (South)                      
## [2342] KR-Korea (South)                      
## [2343] KR-Korea (South)                      
## [2344] KR-Korea (South)                      
## [2345] KR-Korea (South)                      
## [2346] KR-Korea (South)                      
## [2347] KR-Korea (South)                      
## [2348] KR-Korea (South)                      
## [2349] KR-Korea (South)                      
## [2350] KR-Korea (South)                      
## [2351] KR-Korea (South)                      
## [2352] KR-Korea (South)                      
## [2353] KR-Korea (South)                      
## [2354] KR-Korea (South)                      
## [2355] KR-Korea (South)                      
## [2356] KR-Korea (South)                      
## [2357] KR-Korea (South)                      
## [2358] KR-Korea (South)                      
## [2359] KR-Korea (South)                      
## [2360] KR-Korea (South)                      
## [2361] KR-Korea (South)                      
## [2362] KR-Korea (South)                      
## [2363] KR-Korea (South)                      
## [2364] KR-Korea (South)                      
## [2365] KR-Korea (South)                      
## [2366] KR-Korea (South)                      
## [2367] KR-Korea (South)                      
## [2368] KR-Korea (South)                      
## [2369] KR-Korea (South)                      
## [2370] KR-Korea (South)                      
## [2371] KR-Korea (South)                      
## [2372] KR-Korea (South)                      
## [2373] KR-Korea (South)                      
## [2374] KR-Korea (South)                      
## [2375] KR-Korea (South)                      
## [2376] KR-Korea (South)                      
## [2377] KR-Korea (South)                      
## [2378] KR-Korea (South)                      
## [2379] KR-Korea (South)                      
## [2380] KR-Korea (South)                      
## [2381] KR-Korea (South)                      
## [2382] KR-Korea (South)                      
## [2383] KR-Korea (South)                      
## [2384] KR-Korea (South)                      
## [2385] KR-Korea (South)                      
## [2386] KR-Korea (South)                      
## [2387] KR-Korea (South)                      
## [2388] KR-Korea (South)                      
## [2389] KR-Korea (South)                      
## [2390] KR-Korea (South)                      
## [2391] KR-Korea (South)                      
## [2392] KR-Korea (South)                      
## [2393] KR-Korea (South)                      
## [2394] KR-Korea (South)                      
## [2395] KR-Korea (South)                      
## [2396] KR-Korea (South)                      
## [2397] KR-Korea (South)                      
## [2398] KR-Korea (South)                      
## [2399] KR-Korea (South)                      
## [2400] KR-Korea (South)                      
## [2401] KR-Korea (South)                      
## [2402] KR-Korea (South)                      
## [2403] KR-Korea (South)                      
## [2404] KR-Korea (South)                      
## [2405] KR-Korea (South)                      
## [2406] KR-Korea (South)                      
## [2407] KR-Korea (South)                      
## [2408] KR-Korea (South)                      
## [2409] KR-Korea (South)                      
## [2410] KR-Korea (South)                      
## [2411] KR-Korea (South)                      
## [2412] KR-Korea (South)                      
## [2413] KR-Korea (South)                      
## [2414] KR-Korea (South)                      
## [2415] KR-Korea (South)                      
## [2416] KR-Korea (South)                      
## [2417] KR-Korea (South)                      
## [2418] KR-Korea (South)                      
## [2419] KR-Korea (South)                      
## [2420] KR-Korea (South)                      
## [2421] KR-Korea (South)                      
## [2422] KR-Korea (South)                      
## [2423] KR-Korea (South)                      
## [2424] KR-Korea (South)                      
## [2425] KR-Korea (South)                      
## [2426] KR-Korea (South)                      
## [2427] KR-Korea (South)                      
## [2428] KR-Korea (South)                      
## [2429] KR-Korea (South)                      
## [2430] KR-Korea (South)                      
## [2431] KR-Korea (South)                      
## [2432] KR-Korea (South)                      
## [2433] KR-Korea (South)                      
## [2434] KR-Korea (South)                      
## [2435] KR-Korea (South)                      
## [2436] KR-Korea (South)                      
## [2437] KR-Korea (South)                      
## [2438] KR-Korea (South)                      
## [2439] KR-Korea (South)                      
## [2440] KR-Korea (South)                      
## [2441] KR-Korea (South)                      
## [2442] KR-Korea (South)                      
## [2443] KR-Korea (South)                      
## [2444] KR-Korea (South)                      
## [2445] KR-Korea (South)                      
## [2446] KR-Korea (South)                      
## [2447] KR-Korea (South)                      
## [2448] KR-Korea (South)                      
## [2449] KR-Korea (South)                      
## [2450] KR-Korea (South)                      
## [2451] KR-Korea (South)                      
## [2452] KR-Korea (South)                      
## [2453] KR-Korea (South)                      
## [2454] KR-Korea (South)                      
## [2455] KR-Korea (South)                      
## [2456] KR-Korea (South)                      
## [2457] KR-Korea (South)                      
## [2458] KR-Korea (South)                      
## [2459] KR-Korea (South)                      
## [2460] KR-Korea (South)                      
## [2461] KR-Korea (South)                      
## [2462] KR-Korea (South)                      
## [2463] KR-Korea (South)                      
## [2464] KR-Korea (South)                      
## [2465] KR-Korea (South)                      
## [2466] KR-Korea (South)                      
## [2467] KR-Korea (South)                      
## [2468] KR-Korea (South)                      
## [2469] KR-Korea (South)                      
## [2470] KR-Korea (South)                      
## [2471] KR-Korea (South)                      
## [2472] KR-Korea (South)                      
## [2473] KR-Korea (South)                      
## [2474] KR-Korea (South)                      
## [2475] KR-Korea (South)                      
## [2476] KR-Korea (South)                      
## [2477] KR-Korea (South)                      
## [2478] KR-Korea (South)                      
## [2479] KR-Korea (South)                      
## [2480] KR-Korea (South)                      
## [2481] KR-Korea (South)                      
## [2482] KR-Korea (South)                      
## [2483] KR-Korea (South)                      
## [2484] KR-Korea (South)                      
## [2485] KR-Korea (South)                      
## [2486] KR-Korea (South)                      
## [2487] KR-Korea (South)                      
## [2488] KR-Korea (South)                      
## [2489] KR-Korea (South)                      
## [2490] KR-Korea (South)                      
## [2491] KR-Korea (South)                      
## [2492] KR-Korea (South)                      
## [2493] KR-Korea (South)                      
## [2494] KR-Korea (South)                      
## [2495] KR-Korea (South)                      
## [2496] KR-Korea (South)                      
## [2497] KR-Korea (South)                      
## [2498] KR-Korea (South)                      
## [2499] KR-Korea (South)                      
## [2500] KR-Korea (South)                      
## [2501] KR-Korea (South)                      
## [2502] KR-Korea (South)                      
## [2503] KR-Korea (South)                      
## [2504] KR-Korea (South)                      
## [2505] KR-Korea (South)                      
## [2506] KR-Korea (South)                      
## [2507] KR-Korea (South)                      
## [2508] KR-Korea (South)                      
## [2509] KR-Korea (South)                      
## [2510] KR-Korea (South)                      
## [2511] KR-Korea (South)                      
## [2512] KR-Korea (South)                      
## [2513] KR-Korea (South)                      
## [2514] KR-Korea (South)                      
## [2515] KR-Korea (South)                      
## [2516] KR-Korea (South)                      
## [2517] KR-Korea (South)                      
## [2518] KR-Korea (South)                      
## [2519] KR-Korea (South)                      
## [2520] KR-Korea (South)                      
## [2521] KR-Korea (South)                      
## [2522] KR-Korea (South)                      
## [2523] KR-Korea (South)                      
## [2524] KR-Korea (South)                      
## [2525] KR-Korea (South)                      
## [2526] KR-Korea (South)                      
## [2527] KR-Korea (South)                      
## [2528] KR-Korea (South)                      
## [2529] KR-Korea (South)                      
## [2530] KR-Korea (South)                      
## [2531] KR-Korea (South)                      
## [2532] KR-Korea (South)                      
## [2533] KR-Korea (South)                      
## [2534] KR-Korea (South)                      
## [2535] KR-Korea (South)                      
## [2536] KR-Korea (South)                      
## [2537] KR-Korea (South)                      
## [2538] KR-Korea (South)                      
## [2539] KR-Korea (South)                      
## [2540] KR-Korea (South)                      
## [2541] KR-Korea (South)                      
## [2542] KR-Korea (South)                      
## [2543] KR-Korea (South)                      
## [2544] KR-Korea (South)                      
## [2545] KR-Korea (South)                      
## [2546] KR-Korea (South)                      
## [2547] KR-Korea (South)                      
## [2548] KR-Korea (South)                      
## [2549] KR-Korea (South)                      
## [2550] KR-Korea (South)                      
## [2551] KR-Korea (South)                      
## [2552] KR-Korea (South)                      
## [2553] KR-Korea (South)                      
## [2554] KR-Korea (South)                      
## [2555] KR-Korea (South)                      
## [2556] KR-Korea (South)                      
## [2557] KR-Korea (South)                      
## [2558] KR-Korea (South)                      
## [2559] KR-Korea (South)                      
## [2560] KR-Korea (South)                      
## [2561] KR-Korea (South)                      
## [2562] KR-Korea (South)                      
## [2563] KR-Korea (South)                      
## [2564] KR-Korea (South)                      
## [2565] KR-Korea (South)                      
## [2566] KR-Korea (South)                      
## [2567] KR-Korea (South)                      
## [2568] KR-Korea (South)                      
## [2569] KR-Korea (South)                      
## [2570] KR-Korea (South)                      
## [2571] KR-Korea (South)                      
## [2572] KR-Korea (South)                      
## [2573] KR-Korea (South)                      
## [2574] KR-Korea (South)                      
## [2575] KR-Korea (South)                      
## [2576] KR-Korea (South)                      
## [2577] KR-Korea (South)                      
## [2578] KR-Korea (South)                      
## [2579] KR-Korea (South)                      
## [2580] KR-Korea (South)                      
## [2581] KR-Korea (South)                      
## [2582] KR-Korea (South)                      
## [2583] KR-Korea (South)                      
## [2584] KR-Korea (South)                      
## [2585] KR-Korea (South)                      
## [2586] KR-Korea (South)                      
## [2587] KR-Korea (South)                      
## [2588] KR-Korea (South)                      
## [2589] KR-Korea (South)                      
## [2590] KR-Korea (South)                      
## [2591] KR-Korea (South)                      
## [2592] KR-Korea (South)                      
## [2593] KR-Korea (South)                      
## [2594] KR-Korea (South)                      
## [2595] KR-Korea (South)                      
## [2596] KR-Korea (South)                      
## [2597] KR-Korea (South)                      
## [2598] KR-Korea (South)                      
## [2599] KR-Korea (South)                      
## [2600] KR-Korea (South)                      
## [2601] KR-Korea (South)                      
## [2602] KR-Korea (South)                      
## [2603] KR-Korea (South)                      
## [2604] KR-Korea (South)                      
## [2605] KR-Korea (South)                      
## [2606] KR-Korea (South)                      
## [2607] KR-Korea (South)                      
## [2608] KR-Korea (South)                      
## [2609] KR-Korea (South)                      
## [2610] KR-Korea (South)                      
## [2611] KR-Korea (South)                      
## [2612] KR-Korea (South)                      
## [2613] KR-Korea (South)                      
## [2614] KR-Korea (South)                      
## [2615] KR-Korea (South)                      
## [2616] KR-Korea (South)                      
## [2617] KR-Korea (South)                      
## [2618] KR-Korea (South)                      
## [2619] KR-Korea (South)                      
## [2620] KR-Korea (South)                      
## [2621] KR-Korea (South)                      
## [2622] KR-Korea (South)                      
## [2623] KR-Korea (South)                      
## [2624] KR-Korea (South)                      
## [2625] KR-Korea (South)                      
## [2626] KR-Korea (South)                      
## [2627] KR-Korea (South)                      
## [2628] KR-Korea (South)                      
## [2629] KR-Korea (South)                      
## [2630] KR-Korea (South)                      
## [2631] KR-Korea (South)                      
## [2632] KR-Korea (South)                      
## [2633] KR-Korea (South)                      
## [2634] KR-Korea (South)                      
## [2635] KR-Korea (South)                      
## [2636] KR-Korea (South)                      
## [2637] KR-Korea (South)                      
## [2638] KR-Korea (South)                      
## [2639] KR-Korea (South)                      
## [2640] KR-Korea (South)                      
## [2641] KR-Korea (South)                      
## [2642] KR-Korea (South)                      
## [2643] KR-Korea (South)                      
## [2644] KR-Korea (South)                      
## [2645] KR-Korea (South)                      
## [2646] KR-Korea (South)                      
## [2647] KR-Korea (South)                      
## [2648] KR-Korea (South)                      
## [2649] KR-Korea (South)                      
## [2650] KR-Korea (South)                      
## [2651] KR-Korea (South)                      
## [2652] KR-Korea (South)                      
## [2653] KR-Korea (South)                      
## [2654] KR-Korea (South)                      
## [2655] KR-Korea (South)                      
## [2656] KR-Korea (South)                      
## [2657] KR-Korea (South)                      
## [2658] KR-Korea (South)                      
## [2659] KR-Korea (South)                      
## [2660] KR-Korea (South)                      
## [2661] KR-Korea (South)                      
## [2662] KR-Korea (South)                      
## [2663] KR-Korea (South)                      
## [2664] KR-Korea (South)                      
## [2665] KR-Korea (South)                      
## [2666] KR-Korea (South)                      
## [2667] KR-Korea (South)                      
## [2668] KR-Korea (South)                      
## [2669] KR-Korea (South)                      
## [2670] KR-Korea (South)                      
## [2671] KR-Korea (South)                      
## [2672] KR-Korea (South)                      
## [2673] KR-Korea (South)                      
## [2674] KR-Korea (South)                      
## [2675] KR-Korea (South)                      
## [2676] KR-Korea (South)                      
## [2677] KR-Korea (South)                      
## [2678] KR-Korea (South)                      
## [2679] KR-Korea (South)                      
## [2680] KR-Korea (South)                      
## [2681] KR-Korea (South)                      
## [2682] KR-Korea (South)                      
## [2683] KR-Korea (South)                      
## [2684] KR-Korea (South)                      
## [2685] KR-Korea (South)                      
## [2686] KR-Korea (South)                      
## [2687] KR-Korea (South)                      
## [2688] KR-Korea (South)                      
## [2689] KR-Korea (South)                      
## [2690] KR-Korea (South)                      
## [2691] KR-Korea (South)                      
## [2692] KR-Korea (South)                      
## [2693] KR-Korea (South)                      
## [2694] KR-Korea (South)                      
## [2695] KR-Korea (South)                      
## [2696] KR-Korea (South)                      
## [2697] KR-Korea (South)                      
## [2698] KR-Korea (South)                      
## [2699] KR-Korea (South)                      
## [2700] KR-Korea (South)                      
## [2701] KR-Korea (South)                      
## [2702] KR-Korea (South)                      
## [2703] KR-Korea (South)                      
## [2704] KR-Korea (South)                      
## [2705] KR-Korea (South)                      
## [2706] KR-Korea (South)                      
## [2707] KR-Korea (South)                      
## [2708] KR-Korea (South)                      
## [2709] KR-Korea (South)                      
## [2710] KR-Korea (South)                      
## [2711] KR-Korea (South)                      
## [2712] KR-Korea (South)                      
## [2713] KR-Korea (South)                      
## [2714] KR-Korea (South)                      
## [2715] KR-Korea (South)                      
## [2716] KR-Korea (South)                      
## [2717] KR-Korea (South)                      
## [2718] KR-Korea (South)                      
## [2719] KR-Korea (South)                      
## [2720] KR-Korea (South)                      
## [2721] KR-Korea (South)                      
## [2722] KR-Korea (South)                      
## [2723] KR-Korea (South)                      
## [2724] KR-Korea (South)                      
## [2725] KR-Korea (South)                      
## [2726] KR-Korea (South)                      
## [2727] KR-Korea (South)                      
## [2728] KR-Korea (South)                      
## [2729] KR-Korea (South)                      
## [2730] KR-Korea (South)                      
## [2731] KR-Korea (South)                      
## [2732] KR-Korea (South)                      
## [2733] KR-Korea (South)                      
## [2734] KR-Korea (South)                      
## [2735] KR-Korea (South)                      
## [2736] KR-Korea (South)                      
## [2737] KR-Korea (South)                      
## [2738] KR-Korea (South)                      
## [2739] KR-Korea (South)                      
## [2740] KR-Korea (South)                      
## [2741] KR-Korea (South)                      
## [2742] KR-Korea (South)                      
## [2743] KR-Korea (South)                      
## [2744] KR-Korea (South)                      
## [2745] KR-Korea (South)                      
## [2746] KR-Korea (South)                      
## [2747] KR-Korea (South)                      
## [2748] KR-Korea (South)                      
## [2749] KR-Korea (South)                      
## [2750] KR-Korea (South)                      
## [2751] KR-Korea (South)                      
## [2752] KR-Korea (South)                      
## [2753] KR-Korea (South)                      
## [2754] KR-Korea (South)                      
## [2755] KR-Korea (South)                      
## [2756] KR-Korea (South)                      
## [2757] KR-Korea (South)                      
## [2758] KR-Korea (South)                      
## [2759] KR-Korea (South)                      
## [2760] KR-Korea (South)                      
## [2761] KR-Korea (South)                      
## [2762] KR-Korea (South)                      
## [2763] KR-Korea (South)                      
## [2764] KR-Korea (South)                      
## [2765] KR-Korea (South)                      
## [2766] KR-Korea (South)                      
## [2767] KR-Korea (South)                      
## [2768] KR-Korea (South)                      
## [2769] KR-Korea (South)                      
## [2770] KR-Korea (South)                      
## [2771] KR-Korea (South)                      
## [2772] KR-Korea (South)                      
## [2773] KR-Korea (South)                      
## [2774] KR-Korea (South)                      
## [2775] KR-Korea (South)                      
## [2776] KR-Korea (South)                      
## [2777] KR-Korea (South)                      
## [2778] KR-Korea (South)                      
## [2779] KR-Korea (South)                      
## [2780] KR-Korea (South)                      
## [2781] KR-Korea (South)                      
## [2782] KR-Korea (South)                      
## [2783] KR-Korea (South)                      
## [2784] KR-Korea (South)                      
## [2785] KR-Korea (South)                      
## [2786] KR-Korea (South)                      
## [2787] KR-Korea (South)                      
## [2788] KR-Korea (South)                      
## [2789] KR-Korea (South)                      
## [2790] KR-Korea (South)                      
## [2791] KR-Korea (South)                      
## [2792] KR-Korea (South)                      
## [2793] KR-Korea (South)                      
## [2794] KR-Korea (South)                      
## [2795] KR-Korea (South)                      
## [2796] KR-Korea (South)                      
## [2797] KR-Korea (South)                      
## [2798] KR-Korea (South)                      
## [2799] KR-Korea (South)                      
## [2800] KR-Korea (South)                      
## [2801] KR-Korea (South)                      
## [2802] KR-Korea (South)                      
## [2803] KR-Korea (South)                      
## [2804] KR-Korea (South)                      
## [2805] KR-Korea (South)                      
## [2806] KR-Korea (South)                      
## [2807] KR-Korea (South)                      
## [2808] KR-Korea (South)                      
## [2809] KR-Korea (South)                      
## [2810] KR-Korea (South)                      
## [2811] KR-Korea (South)                      
## [2812] KR-Korea (South)                      
## [2813] KR-Korea (South)                      
## [2814] KR-Korea (South)                      
## [2815] KR-Korea (South)                      
## [2816] KR-Korea (South)                      
## [2817] KR-Korea (South)                      
## [2818] KR-Korea (South)                      
## [2819] KR-Korea (South)                      
## [2820] KR-Korea (South)                      
## [2821] KR-Korea (South)                      
## [2822] KR-Korea (South)                      
## [2823] KR-Korea (South)                      
## [2824] KR-Korea (South)                      
## [2825] KR-Korea (South)                      
## [2826] KR-Korea (South)                      
## [2827] KR-Korea (South)                      
## [2828] KR-Korea (South)                      
## [2829] KR-Korea (South)                      
## [2830] KR-Korea (South)                      
## [2831] KR-Korea (South)                      
## [2832] KR-Korea (South)                      
## [2833] KR-Korea (South)                      
## [2834] KR-Korea (South)                      
## [2835] KR-Korea (South)                      
## [2836] KR-Korea (South)                      
## [2837] KR-Korea (South)                      
## [2838] KR-Korea (South)                      
## [2839] KR-Korea (South)                      
## [2840] KR-Korea (South)                      
## [2841] KR-Korea (South)                      
## [2842] KR-Korea (South)                      
## [2843] KR-Korea (South)                      
## [2844] KR-Korea (South)                      
## [2845] KR-Korea (South)                      
## [2846] KR-Korea (South)                      
## [2847] KR-Korea (South)                      
## [2848] KR-Korea (South)                      
## [2849] KR-Korea (South)                      
## [2850] KR-Korea (South)                      
## [2851] KR-Korea (South)                      
## [2852] KR-Korea (South)                      
## [2853] KR-Korea (South)                      
## [2854] KR-Korea (South)                      
## [2855] KR-Korea (South)                      
## [2856] KR-Korea (South)                      
## [2857] KR-Korea (South)                      
## [2858] KR-Korea (South)                      
## [2859] KR-Korea (South)                      
## [2860] KR-Korea (South)                      
## [2861] KR-Korea (South)                      
## [2862] KR-Korea (South)                      
## [2863] KR-Korea (South)                      
## [2864] KR-Korea (South)                      
## [2865] KR-Korea (South)                      
## [2866] KR-Korea (South)                      
## [2867] KR-Korea (South)                      
## [2868] KR-Korea (South)                      
## [2869] KR-Korea (South)                      
## [2870] KR-Korea (South)                      
## [2871] KR-Korea (South)                      
## [2872] KR-Korea (South)                      
## [2873] KR-Korea (South)                      
## [2874] KR-Korea (South)                      
## [2875] KR-Korea (South)                      
## [2876] KR-Korea (South)                      
## [2877] KR-Korea (South)                      
## [2878] KR-Korea (South)                      
## [2879] KR-Korea (South)                      
## [2880] KR-Korea (South)                      
## [2881] KR-Korea (South)                      
## [2882] KR-Korea (South)                      
## [2883] KR-Korea (South)                      
## [2884] KR-Korea (South)                      
## [2885] KR-Korea (South)                      
## [2886] KR-Korea (South)                      
## [2887] KR-Korea (South)                      
## [2888] KR-Korea (South)                      
## [2889] KR-Korea (South)                      
## [2890] KR-Korea (South)                      
## [2891] KR-Korea (South)                      
## [2892] KR-Korea (South)                      
## [2893] KR-Korea (South)                      
## [2894] KR-Korea (South)                      
## [2895] KR-Korea (South)                      
## [2896] KR-Korea (South)                      
## [2897] KR-Korea (South)                      
## [2898] KR-Korea (South)                      
## [2899] KR-Korea (South)                      
## [2900] KR-Korea (South)                      
## [2901] KR-Korea (South)                      
## [2902] KR-Korea (South)                      
## [2903] KR-Korea (South)                      
## [2904] KR-Korea (South)                      
## [2905] KR-Korea (South)                      
## [2906] KR-Korea (South)                      
## [2907] KR-Korea (South)                      
## [2908] KR-Korea (South)                      
## [2909] KR-Korea (South)                      
## [2910] KR-Korea (South)                      
## [2911] KR-Korea (South)                      
## [2912] KR-Korea (South)                      
## [2913] KR-Korea (South)                      
## [2914] KR-Korea (South)                      
## [2915] KR-Korea (South)                      
## [2916] KR-Korea (South)                      
## [2917] KR-Korea (South)                      
## [2918] KR-Korea (South)                      
## [2919] KR-Korea (South)                      
## [2920] KR-Korea (South)                      
## [2921] KR-Korea (South)                      
## [2922] KR-Korea (South)                      
## [2923] KR-Korea (South)                      
## [2924] KR-Korea (South)                      
## [2925] KR-Korea (South)                      
## [2926] KR-Korea (South)                      
## [2927] KR-Korea (South)                      
## [2928] KR-Korea (South)                      
## [2929] KR-Korea (South)                      
## [2930] KR-Korea (South)                      
## [2931] KR-Korea (South)                      
## [2932] KR-Korea (South)                      
## [2933] KR-Korea (South)                      
## [2934] KR-Korea (South)                      
## [2935] KR-Korea (South)                      
## [2936] KR-Korea (South)                      
## [2937] KR-Korea (South)                      
## [2938] KR-Korea (South)                      
## [2939] KR-Korea (South)                      
## [2940] KR-Korea (South)                      
## [2941] KR-Korea (South)                      
## [2942] KR-Korea (South)                      
## [2943] KR-Korea (South)                      
## [2944] KR-Korea (South)                      
## [2945] KR-Korea (South)                      
## [2946] KR-Korea (South)                      
## [2947] KR-Korea (South)                      
## [2948] KR-Korea (South)                      
## [2949] KR-Korea (South)                      
## [2950] KR-Korea (South)                      
## [2951] KR-Korea (South)                      
## [2952] KR-Korea (South)                      
## [2953] KR-Korea (South)                      
## [2954] KR-Korea (South)                      
## [2955] KR-Korea (South)                      
## [2956] KR-Korea (South)                      
## [2957] KR-Korea (South)                      
## [2958] KR-Korea (South)                      
## [2959] KR-Korea (South)                      
## [2960] KR-Korea (South)                      
## [2961] KR-Korea (South)                      
## [2962] KR-Korea (South)                      
## [2963] KR-Korea (South)                      
## [2964] KR-Korea (South)                      
## [2965] KR-Korea (South)                      
## [2966] KR-Korea (South)                      
## [2967] KR-Korea (South)                      
## [2968] KR-Korea (South)                      
## [2969] KR-Korea (South)                      
## [2970] KR-Korea (South)                      
## [2971] KR-Korea (South)                      
## [2972] KR-Korea (South)                      
## [2973] KR-Korea (South)                      
## [2974] KR-Korea (South)                      
## [2975] KR-Korea (South)                      
## [2976] KR-Korea (South)                      
## [2977] KR-Korea (South)                      
## [2978] KR-Korea (South)                      
## [2979] KR-Korea (South)                      
## [2980] KR-Korea (South)                      
## [2981] KR-Korea (South)                      
## [2982] KR-Korea (South)                      
## [2983] KR-Korea (South)                      
## [2984] KR-Korea (South)                      
## [2985] KR-Korea (South)                      
## [2986] KR-Korea (South)                      
## [2987] KR-Korea (South)                      
## [2988] KR-Korea (South)                      
## [2989] KR-Korea (South)                      
## [2990] KR-Korea (South)                      
## [2991] KR-Korea (South)                      
## [2992] KR-Korea (South)                      
## [2993] KR-Korea (South)                      
## [2994] KR-Korea (South)                      
## [2995] KR-Korea (South)                      
## [2996] KR-Korea (South)                      
## [2997] KR-Korea (South)                      
## [2998] KR-Korea (South)                      
## [2999] KR-Korea (South)                      
## [3000] KR-Korea (South)                      
## [3001] KR-Korea (South)                      
## [3002] KR-Korea (South)                      
## [3003] KR-Korea (South)                      
## [3004] KR-Korea (South)                      
## [3005] KR-Korea (South)                      
## [3006] KR-Korea (South)                      
## [3007] KR-Korea (South)                      
## [3008] KR-Korea (South)                      
## [3009] KR-Korea (South)                      
## [3010] KR-Korea (South)                      
## [3011] KR-Korea (South)                      
## [3012] KR-Korea (South)                      
## [3013] KR-Korea (South)                      
## [3014] KR-Korea (South)                      
## [3015] KR-Korea (South)                      
## [3016] KR-Korea (South)                      
## [3017] KR-Korea (South)                      
## [3018] KR-Korea (South)                      
## [3019] KR-Korea (South)                      
## [3020] KR-Korea (South)                      
## [3021] KR-Korea (South)                      
## [3022] KR-Korea (South)                      
## [3023] KR-Korea (South)                      
## [3024] KR-Korea (South)                      
## [3025] KR-Korea (South)                      
## [3026] KR-Korea (South)                      
## [3027] KR-Korea (South)                      
## [3028] KR-Korea (South)                      
## [3029] KR-Korea (South)                      
## [3030] KR-Korea (South)                      
## [3031] KR-Korea (South)                      
## [3032] KR-Korea (South)                      
## [3033] KR-Korea (South)                      
## [3034] KR-Korea (South)                      
## [3035] KR-Korea (South)                      
## [3036] KR-Korea (South)                      
## [3037] KR-Korea (South)                      
## [3038] KR-Korea (South)                      
## [3039] KR-Korea (South)                      
## [3040] KR-Korea (South)                      
## [3041] KR-Korea (South)                      
## [3042] KR-Korea (South)                      
## [3043] KR-Korea (South)                      
## [3044] KR-Korea (South)                      
## [3045] KR-Korea (South)                      
## [3046] KR-Korea (South)                      
## [3047] KR-Korea (South)                      
## [3048] KR-Korea (South)                      
## [3049] KR-Korea (South)                      
## [3050] KR-Korea (South)                      
## [3051] KR-Korea (South)                      
## [3052] KR-Korea (South)                      
## [3053] KR-Korea (South)                      
## [3054] KR-Korea (South)                      
## [3055] KR-Korea (South)                      
## [3056] KR-Korea (South)                      
## [3057] KR-Korea (South)                      
## [3058] KR-Korea (South)                      
## [3059] KR-Korea (South)                      
## [3060] KR-Korea (South)                      
## [3061] KR-Korea (South)                      
## [3062] KR-Korea (South)                      
## [3063] KR-Korea (South)                      
## [3064] KR-Korea (South)                      
## [3065] KR-Korea (South)                      
## [3066] KR-Korea (South)                      
## [3067] KR-Korea (South)                      
## [3068] KR-Korea (South)                      
## [3069] KR-Korea (South)                      
## [3070] KR-Korea (South)                      
## [3071] KR-Korea (South)                      
## [3072] KR-Korea (South)                      
## [3073] KR-Korea (South)                      
## [3074] KR-Korea (South)                      
## [3075] KR-Korea (South)                      
## [3076] KR-Korea (South)                      
## [3077] KR-Korea (South)                      
## [3078] KR-Korea (South)                      
## [3079] KR-Korea (South)                      
## [3080] KR-Korea (South)                      
## [3081] KR-Korea (South)                      
## [3082] KR-Korea (South)                      
## [3083] KR-Korea (South)                      
## [3084] KR-Korea (South)                      
## [3085] KR-Korea (South)                      
## [3086] KR-Korea (South)                      
## [3087] KR-Korea (South)                      
## [3088] KR-Korea (South)                      
## [3089] KR-Korea (South)                      
## [3090] KR-Korea (South)                      
## [3091] KR-Korea (South)                      
## [3092] KR-Korea (South)                      
## [3093] KR-Korea (South)                      
## [3094] KR-Korea (South)                      
## [3095] KR-Korea (South)                      
## [3096] KR-Korea (South)                      
## [3097] KR-Korea (South)                      
## [3098] KR-Korea (South)                      
## [3099] KR-Korea (South)                      
## [3100] KR-Korea (South)                      
## [3101] KR-Korea (South)                      
## [3102] KR-Korea (South)                      
## [3103] KR-Korea (South)                      
## [3104] KR-Korea (South)                      
## [3105] KR-Korea (South)                      
## [3106] KR-Korea (South)                      
## [3107] KR-Korea (South)                      
## [3108] KR-Korea (South)                      
## [3109] KR-Korea (South)                      
## [3110] KR-Korea (South)                      
## [3111] KR-Korea (South)                      
## [3112] KR-Korea (South)                      
## [3113] KR-Korea (South)                      
## [3114] KR-Korea (South)                      
## [3115] KR-Korea (South)                      
## [3116] KR-Korea (South)                      
## [3117] KR-Korea (South)                      
## [3118] KR-Korea (South)                      
## [3119] KR-Korea (South)                      
## [3120] KR-Korea (South)                      
## [3121] KR-Korea (South)                      
## [3122] KR-Korea (South)                      
## [3123] KR-Korea (South)                      
## [3124] KR-Korea (South)                      
## [3125] KR-Korea (South)                      
## [3126] KR-Korea (South)                      
## [3127] KR-Korea (South)                      
## [3128] KR-Korea (South)                      
## [3129] KR-Korea (South)                      
## [3130] KR-Korea (South)                      
## [3131] KR-Korea (South)                      
## [3132] KR-Korea (South)                      
## [3133] KR-Korea (South)                      
## [3134] KR-Korea (South)                      
## [3135] KR-Korea (South)                      
## [3136] KR-Korea (South)                      
## [3137] KR-Korea (South)                      
## [3138] KR-Korea (South)                      
## [3139] KR-Korea (South)                      
## [3140] KR-Korea (South)                      
## [3141] KR-Korea (South)                      
## [3142] KR-Korea (South)                      
## [3143] KR-Korea (South)                      
## [3144] KR-Korea (South)                      
## [3145] KR-Korea (South)                      
## [3146] KR-Korea (South)                      
## [3147] KR-Korea (South)                      
## [3148] KR-Korea (South)                      
## [3149] KR-Korea (South)                      
## [3150] KR-Korea (South)                      
## [3151] KR-Korea (South)                      
## [3152] KR-Korea (South)                      
## [3153] KR-Korea (South)                      
## [3154] KR-Korea (South)                      
## [3155] KR-Korea (South)                      
## [3156] KR-Korea (South)                      
## [3157] KR-Korea (South)                      
## [3158] KR-Korea (South)                      
## [3159] KR-Korea (South)                      
## [3160] KR-Korea (South)                      
## [3161] KR-Korea (South)                      
## [3162] KR-Korea (South)                      
## [3163] KR-Korea (South)                      
## [3164] KR-Korea (South)                      
## [3165] KR-Korea (South)                      
## [3166] KR-Korea (South)                      
## [3167] KR-Korea (South)                      
## [3168] KR-Korea (South)                      
## [3169] KR-Korea (South)                      
## [3170] KR-Korea (South)                      
## [3171] KR-Korea (South)                      
## [3172] KR-Korea (South)                      
## [3173] KR-Korea (South)                      
## [3174] KR-Korea (South)                      
## [3175] KR-Korea (South)                      
## [3176] KR-Korea (South)                      
## [3177] KR-Korea (South)                      
## [3178] KR-Korea (South)                      
## [3179] KR-Korea (South)                      
## [3180] KR-Korea (South)                      
## [3181] KR-Korea (South)                      
## [3182] KR-Korea (South)                      
## [3183] KR-Korea (South)                      
## [3184] KR-Korea (South)                      
## [3185] KR-Korea (South)                      
## [3186] KR-Korea (South)                      
## [3187] KR-Korea (South)                      
## [3188] KR-Korea (South)                      
## [3189] KR-Korea (South)                      
## [3190] KR-Korea (South)                      
## [3191] KR-Korea (South)                      
## [3192] KR-Korea (South)                      
## [3193] KR-Korea (South)                      
## [3194] KR-Korea (South)                      
## [3195] KR-Korea (South)                      
## [3196] KR-Korea (South)                      
## [3197] KR-Korea (South)                      
## [3198] KR-Korea (South)                      
## [3199] KR-Korea (South)                      
## [3200] KR-Korea (South)                      
## [3201] KR-Korea (South)                      
## [3202] KR-Korea (South)                      
## [3203] KR-Korea (South)                      
## [3204] KR-Korea (South)                      
## [3205] KR-Korea (South)                      
## [3206] KR-Korea (South)                      
## [3207] KR-Korea (South)                      
## [3208] KR-Korea (South)                      
## [3209] KR-Korea (South)                      
## [3210] KR-Korea (South)                      
## [3211] KR-Korea (South)                      
## [3212] KR-Korea (South)                      
## [3213] KR-Korea (South)                      
## [3214] KR-Korea (South)                      
## [3215] KR-Korea (South)                      
## [3216] KR-Korea (South)                      
## [3217] KR-Korea (South)                      
## [3218] KR-Korea (South)                      
## [3219] KR-Korea (South)                      
## [3220] KR-Korea (South)                      
## [3221] KR-Korea (South)                      
## [3222] KR-Korea (South)                      
## [3223] KR-Korea (South)                      
## [3224] KR-Korea (South)                      
## [3225] KR-Korea (South)                      
## [3226] KR-Korea (South)                      
## [3227] KR-Korea (South)                      
## [3228] KR-Korea (South)                      
## [3229] KR-Korea (South)                      
## [3230] KR-Korea (South)                      
## [3231] KR-Korea (South)                      
## [3232] KR-Korea (South)                      
## [3233] KR-Korea (South)                      
## [3234] KR-Korea (South)                      
## [3235] KR-Korea (South)                      
## [3236] KR-Korea (South)                      
## [3237] KR-Korea (South)                      
## [3238] KR-Korea (South)                      
## [3239] KR-Korea (South)                      
## [3240] KR-Korea (South)                      
## [3241] KR-Korea (South)                      
## [3242] KR-Korea (South)                      
## [3243] KR-Korea (South)                      
## [3244] KR-Korea (South)                      
## [3245] KR-Korea (South)                      
## [3246] KR-Korea (South)                      
## [3247] KR-Korea (South)                      
## [3248] KR-Korea (South)                      
## [3249] KR-Korea (South)                      
## [3250] KR-Korea (South)                      
## [3251] KR-Korea (South)                      
## [3252] KR-Korea (South)                      
## [3253] KR-Korea (South)                      
## [3254] KR-Korea (South)                      
## [3255] KR-Korea (South)                      
## [3256] KR-Korea (South)                      
## [3257] KR-Korea (South)                      
## [3258] KR-Korea (South)                      
## [3259] KR-Korea (South)                      
## [3260] KR-Korea (South)                      
## [3261] KR-Korea (South)                      
## [3262] KR-Korea (South)                      
## [3263] KR-Korea (South)                      
## [3264] KR-Korea (South)                      
## [3265] KR-Korea (South)                      
## [3266] KR-Korea (South)                      
## [3267] KR-Korea (South)                      
## [3268] KR-Korea (South)                      
## [3269] KR-Korea (South)                      
## [3270] KR-Korea (South)                      
## [3271] KR-Korea (South)                      
## [3272] KR-Korea (South)                      
## [3273] KR-Korea (South)                      
## [3274] KR-Korea (South)                      
## [3275] KR-Korea (South)                      
## [3276] KR-Korea (South)                      
## [3277] KR-Korea (South)                      
## [3278] KR-Korea (South)                      
## [3279] KR-Korea (South)                      
## [3280] KR-Korea (South)                      
## [3281] KR-Korea (South)                      
## [3282] KR-Korea (South)                      
## [3283] KR-Korea (South)                      
## [3284] KR-Korea (South)                      
## [3285] KR-Korea (South)                      
## [3286] KR-Korea (South)                      
## [3287] KR-Korea (South)                      
## [3288] KR-Korea (South)                      
## [3289] KR-Korea (South)                      
## [3290] KR-Korea (South)                      
## [3291] KR-Korea (South)                      
## [3292] KR-Korea (South)                      
## [3293] KR-Korea (South)                      
## [3294] KR-Korea (South)                      
## [3295] KR-Korea (South)                      
## [3296] KR-Korea (South)                      
## [3297] KR-Korea (South)                      
## [3298] KR-Korea (South)                      
## [3299] KR-Korea (South)                      
## [3300] KR-Korea (South)                      
## [3301] KR-Korea (South)                      
## [3302] KR-Korea (South)                      
## [3303] KR-Korea (South)                      
## [3304] KR-Korea (South)                      
## [3305] KR-Korea (South)                      
## [3306] KR-Korea (South)                      
## [3307] KR-Korea (South)                      
## [3308] KR-Korea (South)                      
## [3309] KR-Korea (South)                      
## [3310] KR-Korea (South)                      
## [3311] KR-Korea (South)                      
## [3312] KR-Korea (South)                      
## [3313] KR-Korea (South)                      
## [3314] KR-Korea (South)                      
## [3315] KR-Korea (South)                      
## [3316] KR-Korea (South)                      
## [3317] KR-Korea (South)                      
## [3318] KR-Korea (South)                      
## [3319] KR-Korea (South)                      
## [3320] KR-Korea (South)                      
## [3321] KR-Korea (South)                      
## [3322] KR-Korea (South)                      
## [3323] KR-Korea (South)                      
## [3324] KR-Korea (South)                      
## [3325] KR-Korea (South)                      
## [3326] KR-Korea (South)                      
## [3327] KR-Korea (South)                      
## [3328] KR-Korea (South)                      
## [3329] KR-Korea (South)                      
## [3330] KR-Korea (South)                      
## [3331] KR-Korea (South)                      
## [3332] KR-Korea (South)                      
## [3333] KR-Korea (South)                      
## [3334] KR-Korea (South)                      
## [3335] KR-Korea (South)                      
## [3336] KR-Korea (South)                      
## [3337] KR-Korea (South)                      
## [3338] KR-Korea (South)                      
## [3339] KR-Korea (South)                      
## [3340] KR-Korea (South)                      
## [3341] KR-Korea (South)                      
## [3342] KR-Korea (South)                      
## [3343] KR-Korea (South)                      
## [3344] KR-Korea (South)                      
## [3345] KR-Korea (South)                      
## [3346] KR-Korea (South)                      
## [3347] KR-Korea (South)                      
## [3348] KR-Korea (South)                      
## [3349] KR-Korea (South)                      
## [3350] KR-Korea (South)                      
## [3351] KR-Korea (South)                      
## [3352] KR-Korea (South)                      
## [3353] KR-Korea (South)                      
## [3354] KR-Korea (South)                      
## [3355] KR-Korea (South)                      
## [3356] KR-Korea (South)                      
## [3357] KR-Korea (South)                      
## [3358] KR-Korea (South)                      
## [3359] KR-Korea (South)                      
## [3360] KR-Korea (South)                      
## [3361] KR-Korea (South)                      
## [3362] KR-Korea (South)                      
## [3363] KR-Korea (South)                      
## [3364] KR-Korea (South)                      
## [3365] KR-Korea (South)                      
## [3366] KR-Korea (South)                      
## [3367] KR-Korea (South)                      
## [3368] KR-Korea (South)                      
## [3369] KR-Korea (South)                      
## [3370] KR-Korea (South)                      
## [3371] KR-Korea (South)                      
## [3372] KR-Korea (South)                      
## [3373] KR-Korea (South)                      
## [3374] KR-Korea (South)                      
## [3375] KR-Korea (South)                      
## [3376] KR-Korea (South)                      
## [3377] KR-Korea (South)                      
## [3378] KR-Korea (South)                      
## [3379] KR-Korea (South)                      
## [3380] KR-Korea (South)                      
## [3381] KR-Korea (South)                      
## [3382] KR-Korea (South)                      
## [3383] KR-Korea (South)                      
## [3384] KR-Korea (South)                      
## [3385] KR-Korea (South)                      
## [3386] KR-Korea (South)                      
## [3387] KR-Korea (South)                      
## [3388] KR-Korea (South)                      
## [3389] KR-Korea (South)                      
## [3390] KR-Korea (South)                      
## [3391] KR-Korea (South)                      
## [3392] KR-Korea (South)                      
## [3393] KR-Korea (South)                      
## [3394] KR-Korea (South)                      
## [3395] KR-Korea (South)                      
## [3396] KR-Korea (South)                      
## [3397] KR-Korea (South)                      
## [3398] KR-Korea (South)                      
## [3399] KR-Korea (South)                      
## [3400] KR-Korea (South)                      
## [3401] KR-Korea (South)                      
## [3402] KR-Korea (South)                      
## [3403] KR-Korea (South)                      
## [3404] KR-Korea (South)                      
## [3405] KR-Korea (South)                      
## [3406] KR-Korea (South)                      
## [3407] KR-Korea (South)                      
## [3408] KR-Korea (South)                      
## [3409] KR-Korea (South)                      
## [3410] KR-Korea (South)                      
## [3411] KR-Korea (South)                      
## [3412] KR-Korea (South)                      
## [3413] KR-Korea (South)                      
## [3414] KR-Korea (South)                      
## [3415] KR-Korea (South)                      
## [3416] KR-Korea (South)                      
## [3417] KR-Korea (South)                      
## [3418] KR-Korea (South)                      
## [3419] KR-Korea (South)                      
## [3420] KR-Korea (South)                      
## [3421] KR-Korea (South)                      
## [3422] KR-Korea (South)                      
## [3423] KR-Korea (South)                      
## [3424] KR-Korea (South)                      
## [3425] KR-Korea (South)                      
## [3426] KR-Korea (South)                      
## [3427] KR-Korea (South)                      
## [3428] KR-Korea (South)                      
## [3429] KR-Korea (South)                      
## [3430] KR-Korea (South)                      
## [3431] KR-Korea (South)                      
## [3432] KR-Korea (South)                      
## [3433] KR-Korea (South)                      
## [3434] KR-Korea (South)                      
## [3435] KR-Korea (South)                      
## [3436] KR-Korea (South)                      
## [3437] KR-Korea (South)                      
## [3438] KR-Korea (South)                      
## [3439] KR-Korea (South)                      
## [3440] KR-Korea (South)                      
## [3441] KR-Korea (South)                      
## [3442] KR-Korea (South)                      
## [3443] KR-Korea (South)                      
## [3444] KR-Korea (South)                      
## [3445] KR-Korea (South)                      
## [3446] KR-Korea (South)                      
## [3447] KR-Korea (South)                      
## [3448] KR-Korea (South)                      
## [3449] KR-Korea (South)                      
## [3450] KR-Korea (South)                      
## [3451] KR-Korea (South)                      
## [3452] KR-Korea (South)                      
## [3453] KR-Korea (South)                      
## [3454] KR-Korea (South)                      
## [3455] KR-Korea (South)                      
## [3456] KR-Korea (South)                      
## [3457] KR-Korea (South)                      
## [3458] KR-Korea (South)                      
## [3459] KR-Korea (South)                      
## [3460] KR-Korea (South)                      
## [3461] KR-Korea (South)                      
## [3462] KR-Korea (South)                      
## [3463] KR-Korea (South)                      
## [3464] KR-Korea (South)                      
## [3465] KR-Korea (South)                      
## [3466] KR-Korea (South)                      
## [3467] KR-Korea (South)                      
## [3468] KR-Korea (South)                      
## [3469] KR-Korea (South)                      
## [3470] KR-Korea (South)                      
## [3471] KR-Korea (South)                      
## [3472] KR-Korea (South)                      
## [3473] KR-Korea (South)                      
## [3474] KR-Korea (South)                      
## [3475] KR-Korea (South)                      
## [3476] KR-Korea (South)                      
## [3477] KR-Korea (South)                      
## [3478] KR-Korea (South)                      
## [3479] KR-Korea (South)                      
## [3480] KR-Korea (South)                      
## [3481] KR-Korea (South)                      
## [3482] KR-Korea (South)                      
## [3483] KR-Korea (South)                      
## [3484] KR-Korea (South)                      
## [3485] KR-Korea (South)                      
## [3486] KR-Korea (South)                      
## [3487] KR-Korea (South)                      
## [3488] KR-Korea (South)                      
## [3489] KR-Korea (South)                      
## [3490] KR-Korea (South)                      
## [3491] KR-Korea (South)                      
## [3492] KR-Korea (South)                      
## [3493] KR-Korea (South)                      
## [3494] KR-Korea (South)                      
## [3495] KR-Korea (South)                      
## [3496] KR-Korea (South)                      
## [3497] KR-Korea (South)                      
## [3498] KR-Korea (South)                      
## [3499] KR-Korea (South)                      
## [3500] KR-Korea (South)                      
## [3501] KR-Korea (South)                      
## [3502] KR-Korea (South)                      
## [3503] KR-Korea (South)                      
## [3504] KR-Korea (South)                      
## [3505] KR-Korea (South)                      
## [3506] KR-Korea (South)                      
## [3507] KR-Korea (South)                      
## [3508] KR-Korea (South)                      
## [3509] KR-Korea (South)                      
## [3510] KR-Korea (South)                      
## [3511] KR-Korea (South)                      
## [3512] KR-Korea (South)                      
## [3513] KR-Korea (South)                      
## [3514] KR-Korea (South)                      
## [3515] KR-Korea (South)                      
## [3516] KR-Korea (South)                      
## [3517] KR-Korea (South)                      
## [3518] KR-Korea (South)                      
## [3519] KR-Korea (South)                      
## [3520] KR-Korea (South)                      
## [3521] KR-Korea (South)                      
## [3522] KR-Korea (South)                      
## [3523] KR-Korea (South)                      
## [3524] KR-Korea (South)                      
## [3525] KR-Korea (South)                      
## [3526] KR-Korea (South)                      
## [3527] KR-Korea (South)                      
## [3528] KR-Korea (South)                      
## [3529] KR-Korea (South)                      
## [3530] KR-Korea (South)                      
## [3531] KR-Korea (South)                      
## [3532] KR-Korea (South)                      
## [3533] KR-Korea (South)                      
## [3534] KR-Korea (South)                      
## [3535] KR-Korea (South)                      
## [3536] KR-Korea (South)                      
## [3537] KR-Korea (South)                      
## [3538] KR-Korea (South)                      
## [3539] KR-Korea (South)                      
## [3540] KR-Korea (South)                      
## [3541] KR-Korea (South)                      
## [3542] KR-Korea (South)                      
## [3543] KR-Korea (South)                      
## [3544] KR-Korea (South)                      
## [3545] KR-Korea (South)                      
## [3546] KR-Korea (South)                      
## [3547] KR-Korea (South)                      
## [3548] KR-Korea (South)                      
## [3549] KR-Korea (South)                      
## [3550] KR-Korea (South)                      
## [3551] KR-Korea (South)                      
## [3552] KR-Korea (South)                      
## [3553] KR-Korea (South)                      
## [3554] KR-Korea (South)                      
## [3555] KR-Korea (South)                      
## [3556] KR-Korea (South)                      
## [3557] KR-Korea (South)                      
## [3558] KR-Korea (South)                      
## [3559] KR-Korea (South)                      
## [3560] KR-Korea (South)                      
## [3561] KR-Korea (South)                      
## [3562] KR-Korea (South)                      
## [3563] KR-Korea (South)                      
## [3564] KR-Korea (South)                      
## [3565] KR-Korea (South)                      
## [3566] KR-Korea (South)                      
## [3567] KR-Korea (South)                      
## [3568] KR-Korea (South)                      
## [3569] KR-Korea (South)                      
## [3570] KR-Korea (South)                      
## [3571] KR-Korea (South)                      
## [3572] KR-Korea (South)                      
## [3573] KR-Korea (South)                      
## [3574] KR-Korea (South)                      
## [3575] KR-Korea (South)                      
## [3576] KR-Korea (South)                      
## [3577] KR-Korea (South)                      
## [3578] KR-Korea (South)                      
## [3579] KR-Korea (South)                      
## [3580] KR-Korea (South)                      
## [3581] KR-Korea (South)                      
## [3582] KR-Korea (South)                      
## [3583] KR-Korea (South)                      
## [3584] KR-Korea (South)                      
## [3585] KR-Korea (South)                      
## [3586] KR-Korea (South)                      
## [3587] KR-Korea (South)                      
## [3588] KR-Korea (South)                      
## [3589] KR-Korea (South)                      
## [3590] KR-Korea (South)                      
## [3591] KR-Korea (South)                      
## [3592] KR-Korea (South)                      
## [3593] KR-Korea (South)                      
## [3594] KR-Korea (South)                      
## [3595] KR-Korea (South)                      
## [3596] KR-Korea (South)                      
## [3597] KR-Korea (South)                      
## [3598] KR-Korea (South)                      
## [3599] KR-Korea (South)                      
## [3600] KR-Korea (South)                      
## [3601] KR-Korea (South)                      
## [3602] KR-Korea (South)                      
## [3603] KR-Korea (South)                      
## [3604] KR-Korea (South)                      
## [3605] KR-Korea (South)                      
## [3606] KR-Korea (South)                      
## [3607] KR-Korea (South)                      
## [3608] KR-Korea (South)                      
## [3609] KR-Korea (South)                      
## [3610] KR-Korea (South)                      
## [3611] KR-Korea (South)                      
## [3612] KR-Korea (South)                      
## [3613] KR-Korea (South)                      
## [3614] KR-Korea (South)                      
## [3615] KR-Korea (South)                      
## [3616] KR-Korea (South)                      
## [3617] KR-Korea (South)                      
## [3618] KR-Korea (South)                      
## [3619] KR-Korea (South)                      
## [3620] KR-Korea (South)                      
## [3621] KR-Korea (South)                      
## [3622] KR-Korea (South)                      
## [3623] KR-Korea (South)                      
## [3624] KR-Korea (South)                      
## [3625] KR-Korea (South)                      
## [3626] KR-Korea (South)                      
## [3627] KR-Korea (South)                      
## [3628] KR-Korea (South)                      
## [3629] KR-Korea (South)                      
## [3630] KR-Korea (South)                      
## [3631] KR-Korea (South)                      
## [3632] KR-Korea (South)                      
## [3633] KR-Korea (South)                      
## [3634] KR-Korea (South)                      
## [3635] KR-Korea (South)                      
## [3636] KR-Korea (South)                      
## [3637] KR-Korea (South)                      
## [3638] KR-Korea (South)                      
## [3639] KR-Korea (South)                      
## [3640] KR-Korea (South)                      
## [3641] KR-Korea (South)                      
## [3642] KR-Korea (South)                      
## [3643] KR-Korea (South)                      
## [3644] KR-Korea (South)                      
## [3645] KR-Korea (South)                      
## [3646] KR-Korea (South)                      
## [3647] KR-Korea (South)                      
## [3648] KR-Korea (South)                      
## [3649] KR-Korea (South)                      
## [3650] KR-Korea (South)                      
## [3651] KR-Korea (South)                      
## [3652] KR-Korea (South)                      
## [3653] KR-Korea (South)                      
## [3654] KR-Korea (South)                      
## [3655] KR-Korea (South)                      
## [3656] KR-Korea (South)                      
## [3657] KR-Korea (South)                      
## [3658] KR-Korea (South)                      
## [3659] KR-Korea (South)                      
## [3660] KR-Korea (South)                      
## [3661] KR-Korea (South)                      
## [3662] KR-Korea (South)                      
## [3663] KR-Korea (South)                      
## [3664] KR-Korea (South)                      
## [3665] KR-Korea (South)                      
## [3666] KR-Korea (South)                      
## [3667] KR-Korea (South)                      
## [3668] KR-Korea (South)                      
## [3669] KR-Korea (South)                      
## [3670] KR-Korea (South)                      
## [3671] KR-Korea (South)                      
## [3672] KR-Korea (South)                      
## [3673] KR-Korea (South)                      
## [3674] KR-Korea (South)                      
## [3675] KR-Korea (South)                      
## [3676] KR-Korea (South)                      
## [3677] KR-Korea (South)                      
## [3678] KR-Korea (South)                      
## [3679] KR-Korea (South)                      
## [3680] KR-Korea (South)                      
## [3681] KR-Korea (South)                      
## [3682] KR-Korea (South)                      
## [3683] KR-Korea (South)                      
## [3684] KR-Korea (South)                      
## [3685] KR-Korea (South)                      
## [3686] KR-Korea (South)                      
## [3687] KR-Korea (South)                      
## [3688] KR-Korea (South)                      
## [3689] KR-Korea (South)                      
## [3690] KR-Korea (South)                      
## [3691] KR-Korea (South)                      
## [3692] KR-Korea (South)                      
## [3693] KR-Korea (South)                      
## [3694] KR-Korea (South)                      
## [3695] KR-Korea (South)                      
## [3696] KR-Korea (South)                      
## [3697] KR-Korea (South)                      
## [3698] KR-Korea (South)                      
## [3699] KR-Korea (South)                      
## [3700] KR-Korea (South)                      
## [3701] KR-Korea (South)                      
## [3702] KR-Korea (South)                      
## [3703] KR-Korea (South)                      
## [3704] KR-Korea (South)                      
## [3705] KR-Korea (South)                      
## [3706] KR-Korea (South)                      
## [3707] KR-Korea (South)                      
## [3708] KR-Korea (South)                      
## [3709] KR-Korea (South)                      
## [3710] KR-Korea (South)                      
## [3711] KR-Korea (South)                      
## [3712] KR-Korea (South)                      
## [3713] KR-Korea (South)                      
## [3714] KR-Korea (South)                      
## [3715] KR-Korea (South)                      
## [3716] KR-Korea (South)                      
## [3717] KR-Korea (South)                      
## [3718] KR-Korea (South)                      
## [3719] KR-Korea (South)                      
## [3720] KR-Korea (South)                      
## [3721] KR-Korea (South)                      
## [3722] KR-Korea (South)                      
## [3723] KR-Korea (South)                      
## [3724] KR-Korea (South)                      
## [3725] KR-Korea (South)                      
## [3726] KR-Korea (South)                      
## [3727] KR-Korea (South)                      
## [3728] KR-Korea (South)                      
## [3729] KR-Korea (South)                      
## [3730] KR-Korea (South)                      
## [3731] KR-Korea (South)                      
## [3732] KR-Korea (South)                      
## [3733] KR-Korea (South)                      
## [3734] KR-Korea (South)                      
## [3735] KR-Korea (South)                      
## [3736] KR-Korea (South)                      
## [3737] KR-Korea (South)                      
## [3738] KR-Korea (South)                      
## [3739] KR-Korea (South)                      
## [3740] KR-Korea (South)                      
## [3741] KR-Korea (South)                      
## [3742] KR-Korea (South)                      
## [3743] KR-Korea (South)                      
## [3744] KR-Korea (South)                      
## [3745] KR-Korea (South)                      
## [3746] KR-Korea (South)                      
## [3747] KR-Korea (South)                      
## [3748] KR-Korea (South)                      
## [3749] KR-Korea (South)                      
## [3750] KR-Korea (South)                      
## [3751] KR-Korea (South)                      
## [3752] KR-Korea (South)                      
## [3753] KR-Korea (South)                      
## [3754] KR-Korea (South)                      
## [3755] KR-Korea (South)                      
## [3756] KR-Korea (South)                      
## [3757] KR-Korea (South)                      
## [3758] KR-Korea (South)                      
## [3759] KR-Korea (South)                      
## [3760] KR-Korea (South)                      
## [3761] KR-Korea (South)                      
## [3762] KR-Korea (South)                      
## [3763] KR-Korea (South)                      
## [3764] KR-Korea (South)                      
## [3765] KR-Korea (South)                      
## [3766] KR-Korea (South)                      
## [3767] KR-Korea (South)                      
## [3768] KR-Korea (South)                      
## [3769] KR-Korea (South)                      
## [3770] KR-Korea (South)                      
## [3771] KR-Korea (South)                      
## [3772] KR-Korea (South)                      
## [3773] KR-Korea (South)                      
## [3774] KR-Korea (South)                      
## [3775] KR-Korea (South)                      
## [3776] KR-Korea (South)                      
## [3777] KR-Korea (South)                      
## [3778] KR-Korea (South)                      
## [3779] KR-Korea (South)                      
## [3780] KR-Korea (South)                      
## [3781] KR-Korea (South)                      
## [3782] KR-Korea (South)                      
## [3783] KR-Korea (South)                      
## [3784] KR-Korea (South)                      
## [3785] KR-Korea (South)                      
## [3786] KR-Korea (South)                      
## [3787] KR-Korea (South)                      
## [3788] KR-Korea (South)                      
## [3789] KR-Korea (South)                      
## [3790] KR-Korea (South)                      
## [3791] KR-Korea (South)                      
## [3792] KR-Korea (South)                      
## [3793] KR-Korea (South)                      
## [3794] KR-Korea (South)                      
## [3795] KR-Korea (South)                      
## [3796] KR-Korea (South)                      
## [3797] KR-Korea (South)                      
## [3798] KR-Korea (South)                      
## [3799] KR-Korea (South)                      
## [3800] KR-Korea (South)                      
## [3801] KR-Korea (South)                      
## [3802] KR-Korea (South)                      
## [3803] KR-Korea (South)                      
## [3804] KR-Korea (South)                      
## [3805] KR-Korea (South)                      
## [3806] KR-Korea (South)                      
## [3807] KR-Korea (South)                      
## [3808] KR-Korea (South)                      
## [3809] KR-Korea (South)                      
## [3810] KR-Korea (South)                      
## [3811] KR-Korea (South)                      
## [3812] KR-Korea (South)                      
## [3813] KR-Korea (South)                      
## [3814] KR-Korea (South)                      
## [3815] KR-Korea (South)                      
## [3816] KR-Korea (South)                      
## [3817] KR-Korea (South)                      
## [3818] KR-Korea (South)                      
## [3819] KR-Korea (South)                      
## [3820] KR-Korea (South)                      
## [3821] KR-Korea (South)                      
## [3822] KR-Korea (South)                      
## [3823] KR-Korea (South)                      
## [3824] KR-Korea (South)                      
## [3825] KR-Korea (South)                      
## [3826] KR-Korea (South)                      
## [3827] KR-Korea (South)                      
## [3828] KR-Korea (South)                      
## [3829] KR-Korea (South)                      
## [3830] KR-Korea (South)                      
## [3831] KR-Korea (South)                      
## [3832] KR-Korea (South)                      
## [3833] KR-Korea (South)                      
## [3834] KR-Korea (South)                      
## [3835] KR-Korea (South)                      
## [3836] KR-Korea (South)                      
## [3837] KR-Korea (South)                      
## [3838] KR-Korea (South)                      
## [3839] KR-Korea (South)                      
## [3840] KR-Korea (South)                      
## [3841] KR-Korea (South)                      
## [3842] KR-Korea (South)                      
## [3843] KR-Korea (South)                      
## [3844] KR-Korea (South)                      
## [3845] KR-Korea (South)                      
## [3846] KR-Korea (South)                      
## [3847] KR-Korea (South)                      
## [3848] KR-Korea (South)                      
## [3849] KR-Korea (South)                      
## [3850] KR-Korea (South)                      
## [3851] KR-Korea (South)                      
## [3852] KR-Korea (South)                      
## [3853] KR-Korea (South)                      
## [3854] KR-Korea (South)                      
## [3855] KR-Korea (South)                      
## [3856] KR-Korea (South)                      
## [3857] KR-Korea (South)                      
## [3858] KR-Korea (South)                      
## [3859] KR-Korea (South)                      
## [3860] KR-Korea (South)                      
## [3861] KR-Korea (South)                      
## [3862] KR-Korea (South)                      
## [3863] KR-Korea (South)                      
## [3864] KR-Korea (South)                      
## [3865] KR-Korea (South)                      
## [3866] KR-Korea (South)                      
## [3867] KR-Korea (South)                      
## [3868] KR-Korea (South)                      
## [3869] KR-Korea (South)                      
## [3870] KR-Korea (South)                      
## [3871] KR-Korea (South)                      
## [3872] NZ-New Zealand                        
## [3873] NZ-New Zealand                        
## [3874] NZ-New Zealand                        
## [3875] NZ-New Zealand                        
## [3876] NZ-New Zealand                        
## [3877] NZ-New Zealand                        
## [3878] NZ-New Zealand                        
## [3879] NZ-New Zealand                        
## [3880] NZ-New Zealand                        
## [3881] NZ-New Zealand                        
## [3882] NZ-New Zealand                        
## [3883] NZ-New Zealand                        
## [3884] NZ-New Zealand                        
## [3885] NZ-New Zealand                        
## [3886] NZ-New Zealand                        
## [3887] NZ-New Zealand                        
## [3888] NZ-New Zealand                        
## [3889] NZ-New Zealand                        
## [3890] NZ-New Zealand                        
## [3891] NZ-New Zealand                        
## [3892] NZ-New Zealand                        
## [3893] NZ-New Zealand                        
## [3894] NZ-New Zealand                        
## [3895] NZ-New Zealand                        
## [3896] NZ-New Zealand                        
## [3897] NZ-New Zealand                        
## [3898] NZ-New Zealand                        
## [3899] NZ-New Zealand                        
## [3900] NZ-New Zealand                        
## [3901] NZ-New Zealand                        
## [3902] NZ-New Zealand                        
## [3903] NZ-New Zealand                        
## [3904] NZ-New Zealand                        
## [3905] NZ-New Zealand                        
## [3906] NZ-New Zealand                        
## [3907] NZ-New Zealand                        
## [3908] NZ-New Zealand                        
## [3909] NZ-New Zealand                        
## [3910] NZ-New Zealand                        
## [3911] NZ-New Zealand                        
## [3912] NZ-New Zealand                        
## [3913] NZ-New Zealand                        
## [3914] NZ-New Zealand                        
## [3915] NZ-New Zealand                        
## [3916] NZ-New Zealand                        
## [3917] NZ-New Zealand                        
## [3918] NZ-New Zealand                        
## [3919] NZ-New Zealand                        
## [3920] NZ-New Zealand                        
## [3921] NZ-New Zealand                        
## [3922] NZ-New Zealand                        
## [3923] NZ-New Zealand                        
## [3924] NZ-New Zealand                        
## [3925] NZ-New Zealand                        
## [3926] NZ-New Zealand                        
## [3927] NZ-New Zealand                        
## [3928] NZ-New Zealand                        
## [3929] NZ-New Zealand                        
## [3930] NZ-New Zealand                        
## [3931] NZ-New Zealand                        
## [3932] NZ-New Zealand                        
## [3933] NZ-New Zealand                        
## [3934] NZ-New Zealand                        
## [3935] NZ-New Zealand                        
## [3936] NZ-New Zealand                        
## [3937] NZ-New Zealand                        
## [3938] NZ-New Zealand                        
## [3939] NZ-New Zealand                        
## [3940] NZ-New Zealand                        
## [3941] NZ-New Zealand                        
## [3942] NZ-New Zealand                        
## [3943] NZ-New Zealand                        
## [3944] NZ-New Zealand                        
## [3945] NZ-New Zealand                        
## [3946] NZ-New Zealand                        
## [3947] NZ-New Zealand                        
## [3948] NZ-New Zealand                        
## [3949] NZ-New Zealand                        
## [3950] NZ-New Zealand                        
## [3951] NZ-New Zealand                        
## [3952] NZ-New Zealand                        
## [3953] NZ-New Zealand                        
## [3954] NZ-New Zealand                        
## [3955] NZ-New Zealand                        
## [3956] NZ-New Zealand                        
## [3957] NZ-New Zealand                        
## [3958] NZ-New Zealand                        
## [3959] NZ-New Zealand                        
## [3960] NZ-New Zealand                        
## [3961] NZ-New Zealand                        
## [3962] NZ-New Zealand                        
## [3963] NZ-New Zealand                        
## [3964] NZ-New Zealand                        
## [3965] NZ-New Zealand                        
## [3966] NZ-New Zealand                        
## [3967] NZ-New Zealand                        
## [3968] NZ-New Zealand                        
## [3969] NZ-New Zealand                        
## [3970] NZ-New Zealand                        
## [3971] NZ-New Zealand                        
## [3972] NZ-New Zealand                        
## [3973] NZ-New Zealand                        
## [3974] NZ-New Zealand                        
## [3975] NZ-New Zealand                        
## [3976] NZ-New Zealand                        
## [3977] NZ-New Zealand                        
## [3978] NZ-New Zealand                        
## [3979] NZ-New Zealand                        
## [3980] NZ-New Zealand                        
## [3981] NZ-New Zealand                        
## [3982] NZ-New Zealand                        
## [3983] NZ-New Zealand                        
## [3984] NZ-New Zealand                        
## [3985] NZ-New Zealand                        
## [3986] NZ-New Zealand                        
## [3987] NZ-New Zealand                        
## [3988] NZ-New Zealand                        
## [3989] NZ-New Zealand                        
## [3990] NZ-New Zealand                        
## [3991] NZ-New Zealand                        
## [3992] NZ-New Zealand                        
## [3993] NZ-New Zealand                        
## [3994] NZ-New Zealand                        
## [3995] NZ-New Zealand                        
## [3996] NZ-New Zealand                        
## [3997] NZ-New Zealand                        
## [3998] NZ-New Zealand                        
## [3999] NZ-New Zealand                        
## [4000] NZ-New Zealand                        
## [4001] NZ-New Zealand                        
## [4002] NZ-New Zealand                        
## [4003] NZ-New Zealand                        
## [4004] NZ-New Zealand                        
## [4005] NZ-New Zealand                        
## [4006] NZ-New Zealand                        
## [4007] NZ-New Zealand                        
## [4008] NZ-New Zealand                        
## [4009] NZ-New Zealand                        
## [4010] NZ-New Zealand                        
## [4011] NZ-New Zealand                        
## [4012] NZ-New Zealand                        
## [4013] NZ-New Zealand                        
## [4014] NZ-New Zealand                        
## [4015] NZ-New Zealand                        
## [4016] NZ-New Zealand                        
## [4017] NZ-New Zealand                        
## [4018] NZ-New Zealand                        
## [4019] NZ-New Zealand                        
## [4020] NZ-New Zealand                        
## [4021] NZ-New Zealand                        
## [4022] NZ-New Zealand                        
## [4023] NZ-New Zealand                        
## [4024] NZ-New Zealand                        
## [4025] NZ-New Zealand                        
## [4026] NZ-New Zealand                        
## [4027] NZ-New Zealand                        
## [4028] NZ-New Zealand                        
## [4029] NZ-New Zealand                        
## [4030] NZ-New Zealand                        
## [4031] NZ-New Zealand                        
## [4032] NZ-New Zealand                        
## [4033] NZ-New Zealand                        
## [4034] NZ-New Zealand                        
## [4035] NZ-New Zealand                        
## [4036] NZ-New Zealand                        
## [4037] NZ-New Zealand                        
## [4038] NZ-New Zealand                        
## [4039] NZ-New Zealand                        
## [4040] NZ-New Zealand                        
## [4041] NZ-New Zealand                        
## [4042] NZ-New Zealand                        
## [4043] NZ-New Zealand                        
## [4044] NZ-New Zealand                        
## [4045] NZ-New Zealand                        
## [4046] NZ-New Zealand                        
## [4047] NZ-New Zealand                        
## [4048] NZ-New Zealand                        
## [4049] NZ-New Zealand                        
## [4050] NZ-New Zealand                        
## [4051] NZ-New Zealand                        
## [4052] NZ-New Zealand                        
## [4053] NZ-New Zealand                        
## [4054] NZ-New Zealand                        
## [4055] NZ-New Zealand                        
## [4056] NZ-New Zealand                        
## [4057] NZ-New Zealand                        
## [4058] NZ-New Zealand                        
## [4059] NZ-New Zealand                        
## [4060] NZ-New Zealand                        
## [4061] NZ-New Zealand                        
## [4062] NZ-New Zealand                        
## [4063] NZ-New Zealand                        
## [4064] NZ-New Zealand                        
## [4065] NZ-New Zealand                        
## [4066] NZ-New Zealand                        
## [4067] NZ-New Zealand                        
## [4068] NZ-New Zealand                        
## [4069] NZ-New Zealand                        
## [4070] NZ-New Zealand                        
## [4071] NZ-New Zealand                        
## [4072] NZ-New Zealand                        
## [4073] NZ-New Zealand                        
## [4074] NZ-New Zealand                        
## [4075] NZ-New Zealand                        
## [4076] NZ-New Zealand                        
## [4077] NZ-New Zealand                        
## [4078] NZ-New Zealand                        
## [4079] NZ-New Zealand                        
## [4080] NZ-New Zealand                        
## [4081] NZ-New Zealand                        
## [4082] NZ-New Zealand                        
## [4083] NZ-New Zealand                        
## [4084] NZ-New Zealand                        
## [4085] NZ-New Zealand                        
## [4086] NZ-New Zealand                        
## [4087] NZ-New Zealand                        
## [4088] NZ-New Zealand                        
## [4089] NZ-New Zealand                        
## [4090] NZ-New Zealand                        
## [4091] NZ-New Zealand                        
## [4092] NZ-New Zealand                        
## [4093] NZ-New Zealand                        
## [4094] NZ-New Zealand                        
## [4095] NZ-New Zealand                        
## [4096] NZ-New Zealand                        
## [4097] NZ-New Zealand                        
## [4098] NZ-New Zealand                        
## [4099] NZ-New Zealand                        
## [4100] NZ-New Zealand                        
## [4101] NZ-New Zealand                        
## [4102] NZ-New Zealand                        
## [4103] NZ-New Zealand                        
## [4104] NZ-New Zealand                        
## [4105] NZ-New Zealand                        
## [4106] NZ-New Zealand                        
## [4107] NZ-New Zealand                        
## [4108] NZ-New Zealand                        
## [4109] NZ-New Zealand                        
## [4110] NZ-New Zealand                        
## [4111] NZ-New Zealand                        
## [4112] NZ-New Zealand                        
## [4113] NZ-New Zealand                        
## [4114] NZ-New Zealand                        
## [4115] NZ-New Zealand                        
## [4116] NZ-New Zealand                        
## [4117] NZ-New Zealand                        
## [4118] NZ-New Zealand                        
## [4119] NZ-New Zealand                        
## [4120] NZ-New Zealand                        
## [4121] NZ-New Zealand                        
## [4122] NZ-New Zealand                        
## [4123] NZ-New Zealand                        
## [4124] NZ-New Zealand                        
## [4125] NZ-New Zealand                        
## [4126] NZ-New Zealand                        
## [4127] NZ-New Zealand                        
## [4128] NZ-New Zealand                        
## [4129] NZ-New Zealand                        
## [4130] NZ-New Zealand                        
## [4131] NZ-New Zealand                        
## [4132] NZ-New Zealand                        
## [4133] NZ-New Zealand                        
## [4134] NZ-New Zealand                        
## [4135] NZ-New Zealand                        
## [4136] NZ-New Zealand                        
## [4137] NZ-New Zealand                        
## [4138] NZ-New Zealand                        
## [4139] NZ-New Zealand                        
## [4140] NZ-New Zealand                        
## [4141] NZ-New Zealand                        
## [4142] NZ-New Zealand                        
## [4143] NZ-New Zealand                        
## [4144] NZ-New Zealand                        
## [4145] NZ-New Zealand                        
## [4146] NZ-New Zealand                        
## [4147] NZ-New Zealand                        
## [4148] NZ-New Zealand                        
## [4149] NZ-New Zealand                        
## [4150] NZ-New Zealand                        
## [4151] NZ-New Zealand                        
## [4152] NZ-New Zealand                        
## [4153] NZ-New Zealand                        
## [4154] NZ-New Zealand                        
## [4155] NZ-New Zealand                        
## [4156] NZ-New Zealand                        
## [4157] NZ-New Zealand                        
## [4158] NZ-New Zealand                        
## [4159] NZ-New Zealand                        
## [4160] NZ-New Zealand                        
## [4161] NZ-New Zealand                        
## [4162] NZ-New Zealand                        
## [4163] NZ-New Zealand                        
## [4164] NZ-New Zealand                        
## [4165] NZ-New Zealand                        
## [4166] NZ-New Zealand                        
## [4167] NZ-New Zealand                        
## [4168] NZ-New Zealand                        
## [4169] NZ-New Zealand                        
## [4170] NZ-New Zealand                        
## [4171] NZ-New Zealand                        
## [4172] NZ-New Zealand                        
## [4173] NZ-New Zealand                        
## [4174] NZ-New Zealand                        
## [4175] NZ-New Zealand                        
## [4176] NZ-New Zealand                        
## [4177] NZ-New Zealand                        
## [4178] NZ-New Zealand                        
## [4179] NZ-New Zealand                        
## [4180] NZ-New Zealand                        
## [4181] NZ-New Zealand                        
## [4182] NZ-New Zealand                        
## [4183] NZ-New Zealand                        
## [4184] NZ-New Zealand                        
## [4185] NZ-New Zealand                        
## [4186] NZ-New Zealand                        
## [4187] NZ-New Zealand                        
## [4188] NZ-New Zealand                        
## [4189] NZ-New Zealand                        
## [4190] NZ-New Zealand                        
## [4191] NZ-New Zealand                        
## [4192] NZ-New Zealand                        
## [4193] NZ-New Zealand                        
## [4194] NZ-New Zealand                        
## [4195] NZ-New Zealand                        
## [4196] NZ-New Zealand                        
## [4197] NZ-New Zealand                        
## [4198] NZ-New Zealand                        
## [4199] NZ-New Zealand                        
## [4200] NZ-New Zealand                        
## [4201] NZ-New Zealand                        
## [4202] NZ-New Zealand                        
## [4203] NZ-New Zealand                        
## [4204] NZ-New Zealand                        
## [4205] NZ-New Zealand                        
## [4206] NZ-New Zealand                        
## [4207] NZ-New Zealand                        
## [4208] NZ-New Zealand                        
## [4209] NZ-New Zealand                        
## [4210] NZ-New Zealand                        
## [4211] NZ-New Zealand                        
## [4212] NZ-New Zealand                        
## [4213] NZ-New Zealand                        
## [4214] NZ-New Zealand                        
## [4215] NZ-New Zealand                        
## [4216] NZ-New Zealand                        
## [4217] NZ-New Zealand                        
## [4218] NZ-New Zealand                        
## [4219] NZ-New Zealand                        
## [4220] NZ-New Zealand                        
## [4221] NZ-New Zealand                        
## [4222] NZ-New Zealand                        
## [4223] NZ-New Zealand                        
## [4224] NZ-New Zealand                        
## [4225] NZ-New Zealand                        
## [4226] NZ-New Zealand                        
## [4227] NZ-New Zealand                        
## [4228] NZ-New Zealand                        
## [4229] NZ-New Zealand                        
## [4230] NZ-New Zealand                        
## [4231] NZ-New Zealand                        
## [4232] NZ-New Zealand                        
## [4233] NZ-New Zealand                        
## [4234] NZ-New Zealand                        
## [4235] NZ-New Zealand                        
## [4236] NZ-New Zealand                        
## [4237] NZ-New Zealand                        
## [4238] NZ-New Zealand                        
## [4239] NZ-New Zealand                        
## [4240] NZ-New Zealand                        
## [4241] NZ-New Zealand                        
## [4242] NZ-New Zealand                        
## [4243] NZ-New Zealand                        
## [4244] NZ-New Zealand                        
## [4245] NZ-New Zealand                        
## [4246] NZ-New Zealand                        
## [4247] NZ-New Zealand                        
## [4248] NZ-New Zealand                        
## [4249] NZ-New Zealand                        
## [4250] NZ-New Zealand                        
## [4251] NZ-New Zealand                        
## [4252] NZ-New Zealand                        
## [4253] NZ-New Zealand                        
## [4254] NZ-New Zealand                        
## [4255] NZ-New Zealand                        
## [4256] NZ-New Zealand                        
## [4257] NZ-New Zealand                        
## [4258] NZ-New Zealand                        
## [4259] NZ-New Zealand                        
## [4260] NZ-New Zealand                        
## [4261] NZ-New Zealand                        
## [4262] NZ-New Zealand                        
## [4263] NZ-New Zealand                        
## [4264] NZ-New Zealand                        
## [4265] NZ-New Zealand                        
## [4266] NZ-New Zealand                        
## [4267] NZ-New Zealand                        
## [4268] NZ-New Zealand                        
## [4269] NZ-New Zealand                        
## [4270] NZ-New Zealand                        
## [4271] NZ-New Zealand                        
## [4272] NZ-New Zealand                        
## [4273] NZ-New Zealand                        
## [4274] NZ-New Zealand                        
## [4275] NZ-New Zealand                        
## [4276] NZ-New Zealand                        
## [4277] NZ-New Zealand                        
## [4278] NZ-New Zealand                        
## [4279] NZ-New Zealand                        
## [4280] NZ-New Zealand                        
## [4281] NZ-New Zealand                        
## [4282] NZ-New Zealand                        
## [4283] NZ-New Zealand                        
## [4284] NZ-New Zealand                        
## [4285] NZ-New Zealand                        
## [4286] NZ-New Zealand                        
## [4287] NZ-New Zealand                        
## [4288] NZ-New Zealand                        
## [4289] NZ-New Zealand                        
## [4290] NZ-New Zealand                        
## [4291] NZ-New Zealand                        
## [4292] NZ-New Zealand                        
## [4293] NZ-New Zealand                        
## [4294] NZ-New Zealand                        
## [4295] NZ-New Zealand                        
## [4296] NZ-New Zealand                        
## [4297] NZ-New Zealand                        
## [4298] NZ-New Zealand                        
## [4299] NZ-New Zealand                        
## [4300] NZ-New Zealand                        
## [4301] NZ-New Zealand                        
## [4302] NZ-New Zealand                        
## [4303] NZ-New Zealand                        
## [4304] NZ-New Zealand                        
## [4305] NZ-New Zealand                        
## [4306] NZ-New Zealand                        
## [4307] NZ-New Zealand                        
## [4308] NZ-New Zealand                        
## [4309] NZ-New Zealand                        
## [4310] NZ-New Zealand                        
## [4311] NZ-New Zealand                        
## [4312] NZ-New Zealand                        
## [4313] NZ-New Zealand                        
## [4314] NZ-New Zealand                        
## [4315] NZ-New Zealand                        
## [4316] NZ-New Zealand                        
## [4317] NZ-New Zealand                        
## [4318] NZ-New Zealand                        
## [4319] NZ-New Zealand                        
## [4320] NZ-New Zealand                        
## [4321] NZ-New Zealand                        
## [4322] NZ-New Zealand                        
## [4323] NZ-New Zealand                        
## [4324] NZ-New Zealand                        
## [4325] NZ-New Zealand                        
## [4326] NZ-New Zealand                        
## [4327] NZ-New Zealand                        
## [4328] NZ-New Zealand                        
## [4329] NZ-New Zealand                        
## [4330] NZ-New Zealand                        
## [4331] NZ-New Zealand                        
## [4332] NZ-New Zealand                        
## [4333] NZ-New Zealand                        
## [4334] NZ-New Zealand                        
## [4335] NZ-New Zealand                        
## [4336] NZ-New Zealand                        
## [4337] NZ-New Zealand                        
## [4338] NZ-New Zealand                        
## [4339] NZ-New Zealand                        
## [4340] NZ-New Zealand                        
## [4341] NZ-New Zealand                        
## [4342] NZ-New Zealand                        
## [4343] NZ-New Zealand                        
## [4344] NZ-New Zealand                        
## [4345] NZ-New Zealand                        
## [4346] NZ-New Zealand                        
## [4347] NZ-New Zealand                        
## [4348] NZ-New Zealand                        
## [4349] NZ-New Zealand                        
## [4350] NZ-New Zealand                        
## [4351] NZ-New Zealand                        
## [4352] NZ-New Zealand                        
## [4353] NZ-New Zealand                        
## [4354] NZ-New Zealand                        
## [4355] NZ-New Zealand                        
## [4356] NZ-New Zealand                        
## [4357] NZ-New Zealand                        
## [4358] NZ-New Zealand                        
## [4359] NZ-New Zealand                        
## [4360] NZ-New Zealand                        
## [4361] NZ-New Zealand                        
## [4362] NZ-New Zealand                        
## [4363] NZ-New Zealand                        
## [4364] NZ-New Zealand                        
## [4365] NZ-New Zealand                        
## [4366] NZ-New Zealand                        
## [4367] NZ-New Zealand                        
## [4368] NZ-New Zealand                        
## [4369] NZ-New Zealand                        
## [4370] NZ-New Zealand                        
## [4371] NZ-New Zealand                        
## [4372] NZ-New Zealand                        
## [4373] NZ-New Zealand                        
## [4374] NZ-New Zealand                        
## [4375] NZ-New Zealand                        
## [4376] NZ-New Zealand                        
## [4377] NZ-New Zealand                        
## [4378] NZ-New Zealand                        
## [4379] NZ-New Zealand                        
## [4380] NZ-New Zealand                        
## [4381] NZ-New Zealand                        
## [4382] NZ-New Zealand                        
## [4383] NZ-New Zealand                        
## [4384] NZ-New Zealand                        
## [4385] NZ-New Zealand                        
## [4386] NZ-New Zealand                        
## [4387] NZ-New Zealand                        
## [4388] NZ-New Zealand                        
## [4389] NZ-New Zealand                        
## [4390] NZ-New Zealand                        
## [4391] NZ-New Zealand                        
## [4392] NZ-New Zealand                        
## [4393] NZ-New Zealand                        
## [4394] NZ-New Zealand                        
## [4395] NZ-New Zealand                        
## [4396] NZ-New Zealand                        
## [4397] NZ-New Zealand                        
## [4398] NZ-New Zealand                        
## [4399] NZ-New Zealand                        
## [4400] NZ-New Zealand                        
## [4401] NZ-New Zealand                        
## [4402] NZ-New Zealand                        
## [4403] NZ-New Zealand                        
## [4404] NZ-New Zealand                        
## [4405] NZ-New Zealand                        
## [4406] NZ-New Zealand                        
## [4407] NZ-New Zealand                        
## [4408] NZ-New Zealand                        
## [4409] NZ-New Zealand                        
## [4410] NZ-New Zealand                        
## [4411] NZ-New Zealand                        
## [4412] NZ-New Zealand                        
## [4413] NZ-New Zealand                        
## [4414] NZ-New Zealand                        
## [4415] NZ-New Zealand                        
## [4416] NZ-New Zealand                        
## [4417] NZ-New Zealand                        
## [4418] NZ-New Zealand                        
## [4419] NZ-New Zealand                        
## [4420] NZ-New Zealand                        
## [4421] NZ-New Zealand                        
## [4422] NZ-New Zealand                        
## [4423] NZ-New Zealand                        
## [4424] NZ-New Zealand                        
## [4425] NZ-New Zealand                        
## [4426] NZ-New Zealand                        
## [4427] NZ-New Zealand                        
## [4428] NZ-New Zealand                        
## [4429] NZ-New Zealand                        
## [4430] NZ-New Zealand                        
## [4431] NZ-New Zealand                        
## [4432] NZ-New Zealand                        
## [4433] NZ-New Zealand                        
## [4434] NZ-New Zealand                        
## [4435] NZ-New Zealand                        
## [4436] NZ-New Zealand                        
## [4437] NZ-New Zealand                        
## [4438] NZ-New Zealand                        
## [4439] NZ-New Zealand                        
## [4440] NZ-New Zealand                        
## [4441] NZ-New Zealand                        
## [4442] NZ-New Zealand                        
## [4443] NZ-New Zealand                        
## [4444] NZ-New Zealand                        
## [4445] NZ-New Zealand                        
## [4446] NZ-New Zealand                        
## [4447] NZ-New Zealand                        
## [4448] NZ-New Zealand                        
## [4449] NZ-New Zealand                        
## [4450] NZ-New Zealand                        
## [4451] NZ-New Zealand                        
## [4452] NZ-New Zealand                        
## [4453] NZ-New Zealand                        
## [4454] NZ-New Zealand                        
## [4455] NZ-New Zealand                        
## [4456] NZ-New Zealand                        
## [4457] NZ-New Zealand                        
## [4458] NZ-New Zealand                        
## [4459] NZ-New Zealand                        
## [4460] NZ-New Zealand                        
## [4461] NZ-New Zealand                        
## [4462] NZ-New Zealand                        
## [4463] NZ-New Zealand                        
## [4464] NZ-New Zealand                        
## [4465] NZ-New Zealand                        
## [4466] NZ-New Zealand                        
## [4467] NZ-New Zealand                        
## [4468] NZ-New Zealand                        
## [4469] NZ-New Zealand                        
## [4470] NZ-New Zealand                        
## [4471] NZ-New Zealand                        
## [4472] NZ-New Zealand                        
## [4473] NZ-New Zealand                        
## [4474] NZ-New Zealand                        
## [4475] NZ-New Zealand                        
## [4476] NZ-New Zealand                        
## [4477] NZ-New Zealand                        
## [4478] NZ-New Zealand                        
## [4479] NZ-New Zealand                        
## [4480] NZ-New Zealand                        
## [4481] NZ-New Zealand                        
## [4482] NZ-New Zealand                        
## [4483] NZ-New Zealand                        
## [4484] NZ-New Zealand                        
## [4485] NZ-New Zealand                        
## [4486] NZ-New Zealand                        
## [4487] NZ-New Zealand                        
## [4488] NZ-New Zealand                        
## [4489] NZ-New Zealand                        
## [4490] NZ-New Zealand                        
## [4491] NZ-New Zealand                        
## [4492] NZ-New Zealand                        
## [4493] NZ-New Zealand                        
## [4494] NZ-New Zealand                        
## [4495] NZ-New Zealand                        
## [4496] NZ-New Zealand                        
## [4497] NZ-New Zealand                        
## [4498] NZ-New Zealand                        
## [4499] NZ-New Zealand                        
## [4500] NZ-New Zealand                        
## [4501] NZ-New Zealand                        
## [4502] NZ-New Zealand                        
## [4503] NZ-New Zealand                        
## [4504] NZ-New Zealand                        
## [4505] NZ-New Zealand                        
## [4506] NZ-New Zealand                        
## [4507] NZ-New Zealand                        
## [4508] NZ-New Zealand                        
## [4509] NZ-New Zealand                        
## [4510] NZ-New Zealand                        
## [4511] NZ-New Zealand                        
## [4512] NZ-New Zealand                        
## [4513] NZ-New Zealand                        
## [4514] NZ-New Zealand                        
## [4515] NZ-New Zealand                        
## [4516] NZ-New Zealand                        
## [4517] NZ-New Zealand                        
## [4518] NZ-New Zealand                        
## [4519] NZ-New Zealand                        
## [4520] NZ-New Zealand                        
## [4521] NZ-New Zealand                        
## [4522] NZ-New Zealand                        
## [4523] NZ-New Zealand                        
## [4524] NZ-New Zealand                        
## [4525] NZ-New Zealand                        
## [4526] NZ-New Zealand                        
## [4527] NZ-New Zealand                        
## [4528] NZ-New Zealand                        
## [4529] NZ-New Zealand                        
## [4530] NZ-New Zealand                        
## [4531] NZ-New Zealand                        
## [4532] NZ-New Zealand                        
## [4533] NZ-New Zealand                        
## [4534] NZ-New Zealand                        
## [4535] NZ-New Zealand                        
## [4536] NZ-New Zealand                        
## [4537] NZ-New Zealand                        
## [4538] NZ-New Zealand                        
## [4539] NZ-New Zealand                        
## [4540] NZ-New Zealand                        
## [4541] NZ-New Zealand                        
## [4542] NZ-New Zealand                        
## [4543] NZ-New Zealand                        
## [4544] NZ-New Zealand                        
## [4545] NZ-New Zealand                        
## [4546] NZ-New Zealand                        
## [4547] NZ-New Zealand                        
## [4548] NZ-New Zealand                        
## [4549] NZ-New Zealand                        
## [4550] NZ-New Zealand                        
## [4551] NZ-New Zealand                        
## [4552] NZ-New Zealand                        
## [4553] NZ-New Zealand                        
## [4554] NZ-New Zealand                        
## [4555] NZ-New Zealand                        
## [4556] NZ-New Zealand                        
## [4557] NZ-New Zealand                        
## [4558] NZ-New Zealand                        
## [4559] NZ-New Zealand                        
## [4560] NZ-New Zealand                        
## [4561] NZ-New Zealand                        
## [4562] NZ-New Zealand                        
## [4563] NZ-New Zealand                        
## [4564] NZ-New Zealand                        
## [4565] NZ-New Zealand                        
## [4566] NZ-New Zealand                        
## [4567] NZ-New Zealand                        
## [4568] NZ-New Zealand                        
## [4569] NZ-New Zealand                        
## [4570] NZ-New Zealand                        
## [4571] NZ-New Zealand                        
## [4572] NZ-New Zealand                        
## [4573] NZ-New Zealand                        
## [4574] NZ-New Zealand                        
## [4575] NZ-New Zealand                        
## [4576] NZ-New Zealand                        
## [4577] NZ-New Zealand                        
## [4578] NZ-New Zealand                        
## [4579] NZ-New Zealand                        
## [4580] NZ-New Zealand                        
## [4581] NZ-New Zealand                        
## [4582] NZ-New Zealand                        
## [4583] NZ-New Zealand                        
## [4584] NZ-New Zealand                        
## [4585] NZ-New Zealand                        
## [4586] NZ-New Zealand                        
## [4587] NZ-New Zealand                        
## [4588] NZ-New Zealand                        
## [4589] NZ-New Zealand                        
## [4590] NZ-New Zealand                        
## [4591] NZ-New Zealand                        
## [4592] NZ-New Zealand                        
## [4593] NZ-New Zealand                        
## [4594] NZ-New Zealand                        
## [4595] NZ-New Zealand                        
## [4596] NZ-New Zealand                        
## [4597] NZ-New Zealand                        
## [4598] NZ-New Zealand                        
## [4599] NZ-New Zealand                        
## [4600] NZ-New Zealand                        
## [4601] NZ-New Zealand                        
## [4602] NZ-New Zealand                        
## [4603] NZ-New Zealand                        
## [4604] NZ-New Zealand                        
## [4605] NZ-New Zealand                        
## [4606] NZ-New Zealand                        
## [4607] NZ-New Zealand                        
## [4608] NZ-New Zealand                        
## [4609] NZ-New Zealand                        
## [4610] NZ-New Zealand                        
## [4611] NZ-New Zealand                        
## [4612] NZ-New Zealand                        
## [4613] NZ-New Zealand                        
## [4614] NZ-New Zealand                        
## [4615] NZ-New Zealand                        
## [4616] NZ-New Zealand                        
## [4617] NZ-New Zealand                        
## [4618] NZ-New Zealand                        
## [4619] NZ-New Zealand                        
## [4620] NZ-New Zealand                        
## [4621] NZ-New Zealand                        
## [4622] NZ-New Zealand                        
## [4623] NZ-New Zealand                        
## [4624] NZ-New Zealand                        
## [4625] NZ-New Zealand                        
## [4626] NZ-New Zealand                        
## [4627] NZ-New Zealand                        
## [4628] NZ-New Zealand                        
## [4629] NZ-New Zealand                        
## [4630] NZ-New Zealand                        
## [4631] NZ-New Zealand                        
## [4632] NZ-New Zealand                        
## [4633] NZ-New Zealand                        
## [4634] NZ-New Zealand                        
## [4635] NZ-New Zealand                        
## [4636] NZ-New Zealand                        
## [4637] NZ-New Zealand                        
## [4638] NZ-New Zealand                        
## [4639] NZ-New Zealand                        
## [4640] NZ-New Zealand                        
## [4641] NZ-New Zealand                        
## [4642] NZ-New Zealand                        
## [4643] NZ-New Zealand                        
## [4644] NZ-New Zealand                        
## [4645] NZ-New Zealand                        
## [4646] NZ-New Zealand                        
## [4647] NZ-New Zealand                        
## [4648] NZ-New Zealand                        
## [4649] NZ-New Zealand                        
## [4650] NZ-New Zealand                        
## [4651] NZ-New Zealand                        
## [4652] NZ-New Zealand                        
## [4653] NZ-New Zealand                        
## [4654] NZ-New Zealand                        
## [4655] NZ-New Zealand                        
## [4656] NZ-New Zealand                        
## [4657] NZ-New Zealand                        
## [4658] NZ-New Zealand                        
## [4659] NZ-New Zealand                        
## [4660] NZ-New Zealand                        
## [4661] NZ-New Zealand                        
## [4662] NZ-New Zealand                        
## [4663] NZ-New Zealand                        
## [4664] NZ-New Zealand                        
## [4665] NZ-New Zealand                        
## [4666] NZ-New Zealand                        
## [4667] NZ-New Zealand                        
## [4668] NZ-New Zealand                        
## [4669] NZ-New Zealand                        
## [4670] NZ-New Zealand                        
## [4671] NZ-New Zealand                        
## [4672] NZ-New Zealand                        
## [4673] NZ-New Zealand                        
## [4674] NZ-New Zealand                        
## [4675] NZ-New Zealand                        
## [4676] NZ-New Zealand                        
## [4677] NZ-New Zealand                        
## [4678] NZ-New Zealand                        
## [4679] NZ-New Zealand                        
## [4680] NZ-New Zealand                        
## [4681] NZ-New Zealand                        
## [4682] NZ-New Zealand                        
## [4683] NZ-New Zealand                        
## [4684] NZ-New Zealand                        
## [4685] NZ-New Zealand                        
## [4686] NZ-New Zealand                        
## [4687] NZ-New Zealand                        
## [4688] NZ-New Zealand                        
## [4689] NZ-New Zealand                        
## [4690] NZ-New Zealand                        
## [4691] NZ-New Zealand                        
## [4692] NZ-New Zealand                        
## [4693] NZ-New Zealand                        
## [4694] NZ-New Zealand                        
## [4695] NZ-New Zealand                        
## [4696] NZ-New Zealand                        
## [4697] NZ-New Zealand                        
## [4698] NZ-New Zealand                        
## [4699] NZ-New Zealand                        
## [4700] NZ-New Zealand                        
## [4701] NZ-New Zealand                        
## [4702] NZ-New Zealand                        
## [4703] NZ-New Zealand                        
## [4704] NZ-New Zealand                        
## [4705] NZ-New Zealand                        
## [4706] NZ-New Zealand                        
## [4707] NZ-New Zealand                        
## [4708] NZ-New Zealand                        
## [4709] NZ-New Zealand                        
## [4710] NZ-New Zealand                        
## [4711] NZ-New Zealand                        
## [4712] NZ-New Zealand                        
## [4713] NZ-New Zealand                        
## [4714] NZ-New Zealand                        
## [4715] NZ-New Zealand                        
## [4716] NZ-New Zealand                        
## [4717] NZ-New Zealand                        
## [4718] NZ-New Zealand                        
## [4719] NZ-New Zealand                        
## [4720] NZ-New Zealand                        
## [4721] NZ-New Zealand                        
## [4722] NZ-New Zealand                        
## [4723] NZ-New Zealand                        
## [4724] NZ-New Zealand                        
## [4725] NZ-New Zealand                        
## [4726] NZ-New Zealand                        
## [4727] NZ-New Zealand                        
## [4728] NZ-New Zealand                        
## [4729] NZ-New Zealand                        
## [4730] NZ-New Zealand                        
## [4731] NZ-New Zealand                        
## [4732] NZ-New Zealand                        
## [4733] NZ-New Zealand                        
## [4734] NZ-New Zealand                        
## [4735] NZ-New Zealand                        
## [4736] NZ-New Zealand                        
## [4737] NZ-New Zealand                        
## [4738] NZ-New Zealand                        
## [4739] NZ-New Zealand                        
## [4740] NZ-New Zealand                        
## [4741] NZ-New Zealand                        
## [4742] NZ-New Zealand                        
## [4743] NZ-New Zealand                        
## [4744] NZ-New Zealand                        
## [4745] NZ-New Zealand                        
## [4746] NZ-New Zealand                        
## [4747] NZ-New Zealand                        
## [4748] NZ-New Zealand                        
## [4749] NZ-New Zealand                        
## [4750] NZ-New Zealand                        
## [4751] NZ-New Zealand                        
## [4752] NZ-New Zealand                        
## [4753] NZ-New Zealand                        
## [4754] NZ-New Zealand                        
## [4755] NZ-New Zealand                        
## [4756] NZ-New Zealand                        
## [4757] NZ-New Zealand                        
## [4758] NZ-New Zealand                        
## [4759] NZ-New Zealand                        
## [4760] NZ-New Zealand                        
## [4761] NZ-New Zealand                        
## [4762] NZ-New Zealand                        
## [4763] NZ-New Zealand                        
## [4764] NZ-New Zealand                        
## [4765] NZ-New Zealand                        
## [4766] NZ-New Zealand                        
## [4767] NZ-New Zealand                        
## [4768] NZ-New Zealand                        
## [4769] NZ-New Zealand                        
## [4770] NZ-New Zealand                        
## [4771] NZ-New Zealand                        
## [4772] NZ-New Zealand                        
## [4773] NZ-New Zealand                        
## [4774] NZ-New Zealand                        
## [4775] NZ-New Zealand                        
## [4776] NZ-New Zealand                        
## [4777] NZ-New Zealand                        
## [4778] NZ-New Zealand                        
## [4779] NZ-New Zealand                        
## [4780] NZ-New Zealand                        
## [4781] NZ-New Zealand                        
## [4782] NZ-New Zealand                        
## [4783] NZ-New Zealand                        
## [4784] NZ-New Zealand                        
## [4785] NZ-New Zealand                        
## [4786] NZ-New Zealand                        
## [4787] NZ-New Zealand                        
## [4788] NZ-New Zealand                        
## [4789] NZ-New Zealand                        
## [4790] NZ-New Zealand                        
## [4791] NZ-New Zealand                        
## [4792] NZ-New Zealand                        
## [4793] NZ-New Zealand                        
## [4794] NZ-New Zealand                        
## [4795] NZ-New Zealand                        
## [4796] NZ-New Zealand                        
## [4797] NZ-New Zealand                        
## [4798] NZ-New Zealand                        
## [4799] NZ-New Zealand                        
## [4800] NZ-New Zealand                        
## [4801] NZ-New Zealand                        
## [4802] NZ-New Zealand                        
## [4803] NZ-New Zealand                        
## [4804] NZ-New Zealand                        
## [4805] NZ-New Zealand                        
## [4806] NZ-New Zealand                        
## [4807] NZ-New Zealand                        
## [4808] NZ-New Zealand                        
## [4809] NZ-New Zealand                        
## [4810] NZ-New Zealand                        
## [4811] NZ-New Zealand                        
## [4812] NZ-New Zealand                        
## [4813] NZ-New Zealand                        
## [4814] NZ-New Zealand                        
## [4815] NZ-New Zealand                        
## [4816] NZ-New Zealand                        
## [4817] NZ-New Zealand                        
## [4818] NZ-New Zealand                        
## [4819] NZ-New Zealand                        
## [4820] NZ-New Zealand                        
## [4821] NZ-New Zealand                        
## [4822] NZ-New Zealand                        
## [4823] NZ-New Zealand                        
## [4824] NZ-New Zealand                        
## [4825] NZ-New Zealand                        
## [4826] NZ-New Zealand                        
## [4827] NZ-New Zealand                        
## [4828] NZ-New Zealand                        
## [4829] NZ-New Zealand                        
## [4830] NZ-New Zealand                        
## [4831] NZ-New Zealand                        
## [4832] NZ-New Zealand                        
## [4833] NZ-New Zealand                        
## [4834] NZ-New Zealand                        
## [4835] NZ-New Zealand                        
## [4836] NZ-New Zealand                        
## [4837] NZ-New Zealand                        
## [4838] NZ-New Zealand                        
## [4839] NZ-New Zealand                        
## [4840] NZ-New Zealand                        
## [4841] NZ-New Zealand                        
## [4842] NZ-New Zealand                        
## [4843] NZ-New Zealand                        
## [4844] NZ-New Zealand                        
## [4845] NZ-New Zealand                        
## [4846] NZ-New Zealand                        
## [4847] NZ-New Zealand                        
## [4848] NZ-New Zealand                        
## [4849] NZ-New Zealand                        
## [4850] NZ-New Zealand                        
## [4851] NZ-New Zealand                        
## [4852] NZ-New Zealand                        
## [4853] NZ-New Zealand                        
## [4854] NZ-New Zealand                        
## [4855] NZ-New Zealand                        
## [4856] NZ-New Zealand                        
## [4857] NZ-New Zealand                        
## [4858] NZ-New Zealand                        
## [4859] NZ-New Zealand                        
## [4860] NZ-New Zealand                        
## [4861] NZ-New Zealand                        
## [4862] NZ-New Zealand                        
## [4863] NZ-New Zealand                        
## [4864] NZ-New Zealand                        
## [4865] NZ-New Zealand                        
## [4866] NZ-New Zealand                        
## [4867] NZ-New Zealand                        
## [4868] NZ-New Zealand                        
## [4869] NZ-New Zealand                        
## [4870] NZ-New Zealand                        
## [4871] NZ-New Zealand                        
## [4872] NZ-New Zealand                        
## [4873] NZ-New Zealand                        
## [4874] NZ-New Zealand                        
## [4875] NZ-New Zealand                        
## [4876] NZ-New Zealand                        
## [4877] NZ-New Zealand                        
## [4878] NZ-New Zealand                        
## [4879] NZ-New Zealand                        
## [4880] NZ-New Zealand                        
## [4881] NZ-New Zealand                        
## [4882] NZ-New Zealand                        
## [4883] NZ-New Zealand                        
## [4884] NZ-New Zealand                        
## [4885] NZ-New Zealand                        
## [4886] NZ-New Zealand                        
## [4887] NZ-New Zealand                        
## [4888] NZ-New Zealand                        
## [4889] NZ-New Zealand                        
## [4890] NZ-New Zealand                        
## [4891] NZ-New Zealand                        
## [4892] NZ-New Zealand                        
## [4893] NZ-New Zealand                        
## [4894] NZ-New Zealand                        
## [4895] NZ-New Zealand                        
## [4896] NZ-New Zealand                        
## [4897] NZ-New Zealand                        
## [4898] NZ-New Zealand                        
## [4899] NZ-New Zealand                        
## [4900] NZ-New Zealand                        
## [4901] NZ-New Zealand                        
## [4902] NZ-New Zealand                        
## [4903] NZ-New Zealand                        
## [4904] NZ-New Zealand                        
## [4905] NZ-New Zealand                        
## [4906] NZ-New Zealand                        
## [4907] NZ-New Zealand                        
## [4908] NZ-New Zealand                        
## [4909] NZ-New Zealand                        
## [4910] NZ-New Zealand                        
## [4911] NZ-New Zealand                        
## [4912] NZ-New Zealand                        
## [4913] NZ-New Zealand                        
## [4914] NZ-New Zealand                        
## [4915] NZ-New Zealand                        
## [4916] NZ-New Zealand                        
## [4917] NZ-New Zealand                        
## [4918] NZ-New Zealand                        
## [4919] NZ-New Zealand                        
## [4920] NZ-New Zealand                        
## [4921] NZ-New Zealand                        
## [4922] NZ-New Zealand                        
## [4923] NZ-New Zealand                        
## [4924] NZ-New Zealand                        
## [4925] NZ-New Zealand                        
## [4926] NZ-New Zealand                        
## [4927] NZ-New Zealand                        
## [4928] NZ-New Zealand                        
## [4929] NZ-New Zealand                        
## [4930] NZ-New Zealand                        
## [4931] NZ-New Zealand                        
## [4932] NZ-New Zealand                        
## [4933] NZ-New Zealand                        
## [4934] NZ-New Zealand                        
## [4935] NZ-New Zealand                        
## [4936] NZ-New Zealand                        
## [4937] NZ-New Zealand                        
## [4938] NZ-New Zealand                        
## [4939] NZ-New Zealand                        
## [4940] NZ-New Zealand                        
## [4941] NZ-New Zealand                        
## [4942] NZ-New Zealand                        
## [4943] NZ-New Zealand                        
## [4944] NZ-New Zealand                        
## [4945] NZ-New Zealand                        
## [4946] NZ-New Zealand                        
## [4947] NZ-New Zealand                        
## [4948] NZ-New Zealand                        
## [4949] NZ-New Zealand                        
## [4950] NZ-New Zealand                        
## [4951] NZ-New Zealand                        
## [4952] NZ-New Zealand                        
## [4953] NZ-New Zealand                        
## [4954] NZ-New Zealand                        
## [4955] NZ-New Zealand                        
## [4956] NZ-New Zealand                        
## [4957] NZ-New Zealand                        
## [4958] NZ-New Zealand                        
## [4959] NZ-New Zealand                        
## [4960] NZ-New Zealand                        
## [4961] NZ-New Zealand                        
## [4962] NZ-New Zealand                        
## [4963] NZ-New Zealand                        
## [4964] NZ-New Zealand                        
## [4965] NZ-New Zealand                        
## [4966] NZ-New Zealand                        
## [4967] NZ-New Zealand                        
## [4968] NZ-New Zealand                        
## [4969] NZ-New Zealand                        
## [4970] NZ-New Zealand                        
## [4971] NZ-New Zealand                        
## [4972] NZ-New Zealand                        
## [4973] NZ-New Zealand                        
## [4974] NZ-New Zealand                        
## [4975] NZ-New Zealand                        
## [4976] NZ-New Zealand                        
## [4977] NZ-New Zealand                        
## [4978] NZ-New Zealand                        
## [4979] NZ-New Zealand                        
## [4980] NZ-New Zealand                        
## [4981] NZ-New Zealand                        
## [4982] NZ-New Zealand                        
## [4983] NZ-New Zealand                        
## [4984] NZ-New Zealand                        
## [4985] NZ-New Zealand                        
## [4986] NZ-New Zealand                        
## [4987] NZ-New Zealand                        
## [4988] NZ-New Zealand                        
## [4989] NZ-New Zealand                        
## [4990] NZ-New Zealand                        
## [4991] NZ-New Zealand                        
## [4992] NZ-New Zealand                        
## [4993] NZ-New Zealand                        
## [4994] NZ-New Zealand                        
## [4995] NZ-New Zealand                        
## [4996] NZ-New Zealand                        
## [4997] NZ-New Zealand                        
## [4998] NZ-New Zealand                        
## [4999] NZ-New Zealand                        
## [5000] NZ-New Zealand                        
## [5001] NZ-New Zealand                        
## [5002] NZ-New Zealand                        
## [5003] NZ-New Zealand                        
## [5004] NZ-New Zealand                        
## [5005] NZ-New Zealand                        
## [5006] NZ-New Zealand                        
## [5007] NZ-New Zealand                        
## [5008] NZ-New Zealand                        
## [5009] NZ-New Zealand                        
## [5010] NZ-New Zealand                        
## [5011] NZ-New Zealand                        
## [5012] NZ-New Zealand                        
## [5013] NZ-New Zealand                        
## [5014] NZ-New Zealand                        
## [5015] NZ-New Zealand                        
## [5016] NZ-New Zealand                        
## [5017] NZ-New Zealand                        
## [5018] NZ-New Zealand                        
## [5019] NZ-New Zealand                        
## [5020] NZ-New Zealand                        
## [5021] NZ-New Zealand                        
## [5022] NZ-New Zealand                        
## [5023] NZ-New Zealand                        
## [5024] NZ-New Zealand                        
## [5025] NZ-New Zealand                        
## [5026] NZ-New Zealand                        
## [5027] NZ-New Zealand                        
## [5028] NZ-New Zealand                        
## [5029] NZ-New Zealand                        
## [5030] NZ-New Zealand                        
## [5031] NZ-New Zealand                        
## [5032] NZ-New Zealand                        
## [5033] NZ-New Zealand                        
## [5034] NZ-New Zealand                        
## [5035] NZ-New Zealand                        
## [5036] NZ-New Zealand                        
## [5037] NZ-New Zealand                        
## [5038] NZ-New Zealand                        
## [5039] NZ-New Zealand                        
## [5040] NZ-New Zealand                        
## [5041] NZ-New Zealand                        
## [5042] NZ-New Zealand                        
## [5043] NZ-New Zealand                        
## [5044] NO-Norway                             
## [5045] NO-Norway                             
## [5046] NO-Norway                             
## [5047] NO-Norway                             
## [5048] NO-Norway                             
## [5049] NO-Norway                             
## [5050] NO-Norway                             
## [5051] NO-Norway                             
## [5052] NO-Norway                             
## [5053] NO-Norway                             
## [5054] NO-Norway                             
## [5055] NO-Norway                             
## [5056] NO-Norway                             
## [5057] NO-Norway                             
## [5058] NO-Norway                             
## [5059] NO-Norway                             
## [5060] NO-Norway                             
## [5061] NO-Norway                             
## [5062] NO-Norway                             
## [5063] NO-Norway                             
## [5064] NO-Norway                             
## [5065] NO-Norway                             
## [5066] NO-Norway                             
## [5067] NO-Norway                             
## [5068] NO-Norway                             
## [5069] NO-Norway                             
## [5070] NO-Norway                             
## [5071] NO-Norway                             
## [5072] NO-Norway                             
## [5073] NO-Norway                             
## [5074] NO-Norway                             
## [5075] NO-Norway                             
## [5076] NO-Norway                             
## [5077] NO-Norway                             
## [5078] NO-Norway                             
## [5079] NO-Norway                             
## [5080] NO-Norway                             
## [5081] NO-Norway                             
## [5082] NO-Norway                             
## [5083] NO-Norway                             
## [5084] NO-Norway                             
## [5085] NO-Norway                             
## [5086] NO-Norway                             
## [5087] NO-Norway                             
## [5088] NO-Norway                             
## [5089] NO-Norway                             
## [5090] NO-Norway                             
## [5091] NO-Norway                             
## [5092] NO-Norway                             
## [5093] NO-Norway                             
## [5094] NO-Norway                             
## [5095] NO-Norway                             
## [5096] NO-Norway                             
## [5097] NO-Norway                             
## [5098] NO-Norway                             
## [5099] NO-Norway                             
## [5100] NO-Norway                             
## [5101] NO-Norway                             
## [5102] NO-Norway                             
## [5103] NO-Norway                             
## [5104] NO-Norway                             
## [5105] NO-Norway                             
## [5106] NO-Norway                             
## [5107] NO-Norway                             
## [5108] NO-Norway                             
## [5109] NO-Norway                             
## [5110] NO-Norway                             
## [5111] NO-Norway                             
## [5112] NO-Norway                             
## [5113] NO-Norway                             
## [5114] NO-Norway                             
## [5115] NO-Norway                             
## [5116] NO-Norway                             
## [5117] NO-Norway                             
## [5118] NO-Norway                             
## [5119] NO-Norway                             
## [5120] NO-Norway                             
## [5121] NO-Norway                             
## [5122] NO-Norway                             
## [5123] NO-Norway                             
## [5124] NO-Norway                             
## [5125] NO-Norway                             
## [5126] NO-Norway                             
## [5127] NO-Norway                             
## [5128] NO-Norway                             
## [5129] NO-Norway                             
## [5130] NO-Norway                             
## [5131] NO-Norway                             
## [5132] NO-Norway                             
## [5133] NO-Norway                             
## [5134] NO-Norway                             
## [5135] NO-Norway                             
## [5136] NO-Norway                             
## [5137] NO-Norway                             
## [5138] NO-Norway                             
## [5139] NO-Norway                             
## [5140] NO-Norway                             
## [5141] NO-Norway                             
## [5142] NO-Norway                             
## [5143] NO-Norway                             
## [5144] NO-Norway                             
## [5145] NO-Norway                             
## [5146] NO-Norway                             
## [5147] NO-Norway                             
## [5148] NO-Norway                             
## [5149] NO-Norway                             
## [5150] NO-Norway                             
## [5151] NO-Norway                             
## [5152] NO-Norway                             
## [5153] NO-Norway                             
## [5154] NO-Norway                             
## [5155] NO-Norway                             
## [5156] NO-Norway                             
## [5157] NO-Norway                             
## [5158] NO-Norway                             
## [5159] NO-Norway                             
## [5160] NO-Norway                             
## [5161] NO-Norway                             
## [5162] NO-Norway                             
## [5163] NO-Norway                             
## [5164] NO-Norway                             
## [5165] NO-Norway                             
## [5166] NO-Norway                             
## [5167] NO-Norway                             
## [5168] NO-Norway                             
## [5169] NO-Norway                             
## [5170] NO-Norway                             
## [5171] NO-Norway                             
## [5172] NO-Norway                             
## [5173] NO-Norway                             
## [5174] NO-Norway                             
## [5175] NO-Norway                             
## [5176] NO-Norway                             
## [5177] NO-Norway                             
## [5178] NO-Norway                             
## [5179] NO-Norway                             
## [5180] NO-Norway                             
## [5181] NO-Norway                             
## [5182] NO-Norway                             
## [5183] NO-Norway                             
## [5184] NO-Norway                             
## [5185] NO-Norway                             
## [5186] NO-Norway                             
## [5187] NO-Norway                             
## [5188] NO-Norway                             
## [5189] NO-Norway                             
## [5190] NO-Norway                             
## [5191] NO-Norway                             
## [5192] NO-Norway                             
## [5193] NO-Norway                             
## [5194] NO-Norway                             
## [5195] NO-Norway                             
## [5196] NO-Norway                             
## [5197] NO-Norway                             
## [5198] NO-Norway                             
## [5199] NO-Norway                             
## [5200] NO-Norway                             
## [5201] NO-Norway                             
## [5202] NO-Norway                             
## [5203] NO-Norway                             
## [5204] NO-Norway                             
## [5205] NO-Norway                             
## [5206] NO-Norway                             
## [5207] NO-Norway                             
## [5208] NO-Norway                             
## [5209] NO-Norway                             
## [5210] NO-Norway                             
## [5211] NO-Norway                             
## [5212] NO-Norway                             
## [5213] NO-Norway                             
## [5214] NO-Norway                             
## [5215] NO-Norway                             
## [5216] NO-Norway                             
## [5217] NO-Norway                             
## [5218] NO-Norway                             
## [5219] NO-Norway                             
## [5220] NO-Norway                             
## [5221] NO-Norway                             
## [5222] NO-Norway                             
## [5223] NO-Norway                             
## [5224] NO-Norway                             
## [5225] NO-Norway                             
## [5226] NO-Norway                             
## [5227] NO-Norway                             
## [5228] NO-Norway                             
## [5229] NO-Norway                             
## [5230] NO-Norway                             
## [5231] NO-Norway                             
## [5232] NO-Norway                             
## [5233] NO-Norway                             
## [5234] NO-Norway                             
## [5235] NO-Norway                             
## [5236] NO-Norway                             
## [5237] NO-Norway                             
## [5238] NO-Norway                             
## [5239] NO-Norway                             
## [5240] NO-Norway                             
## [5241] NO-Norway                             
## [5242] NO-Norway                             
## [5243] NO-Norway                             
## [5244] NO-Norway                             
## [5245] NO-Norway                             
## [5246] NO-Norway                             
## [5247] NO-Norway                             
## [5248] NO-Norway                             
## [5249] NO-Norway                             
## [5250] NO-Norway                             
## [5251] NO-Norway                             
## [5252] NO-Norway                             
## [5253] NO-Norway                             
## [5254] NO-Norway                             
## [5255] NO-Norway                             
## [5256] NO-Norway                             
## [5257] NO-Norway                             
## [5258] NO-Norway                             
## [5259] NO-Norway                             
## [5260] NO-Norway                             
## [5261] NO-Norway                             
## [5262] NO-Norway                             
## [5263] NO-Norway                             
## [5264] NO-Norway                             
## [5265] NO-Norway                             
## [5266] NO-Norway                             
## [5267] NO-Norway                             
## [5268] NO-Norway                             
## [5269] NO-Norway                             
## [5270] NO-Norway                             
## [5271] NO-Norway                             
## [5272] NO-Norway                             
## [5273] NO-Norway                             
## [5274] NO-Norway                             
## [5275] NO-Norway                             
## [5276] NO-Norway                             
## [5277] NO-Norway                             
## [5278] NO-Norway                             
## [5279] NO-Norway                             
## [5280] NO-Norway                             
## [5281] NO-Norway                             
## [5282] NO-Norway                             
## [5283] NO-Norway                             
## [5284] NO-Norway                             
## [5285] NO-Norway                             
## [5286] NO-Norway                             
## [5287] NO-Norway                             
## [5288] NO-Norway                             
## [5289] NO-Norway                             
## [5290] NO-Norway                             
## [5291] NO-Norway                             
## [5292] NO-Norway                             
## [5293] NO-Norway                             
## [5294] NO-Norway                             
## [5295] NO-Norway                             
## [5296] NO-Norway                             
## [5297] NO-Norway                             
## [5298] NO-Norway                             
## [5299] NO-Norway                             
## [5300] NO-Norway                             
## [5301] NO-Norway                             
## [5302] NO-Norway                             
## [5303] NO-Norway                             
## [5304] NO-Norway                             
## [5305] NO-Norway                             
## [5306] NO-Norway                             
## [5307] NO-Norway                             
## [5308] NO-Norway                             
## [5309] NO-Norway                             
## [5310] NO-Norway                             
## [5311] NO-Norway                             
## [5312] NO-Norway                             
## [5313] NO-Norway                             
## [5314] NO-Norway                             
## [5315] NO-Norway                             
## [5316] NO-Norway                             
## [5317] NO-Norway                             
## [5318] NO-Norway                             
## [5319] NO-Norway                             
## [5320] NO-Norway                             
## [5321] NO-Norway                             
## [5322] NO-Norway                             
## [5323] NO-Norway                             
## [5324] NO-Norway                             
## [5325] NO-Norway                             
## [5326] NO-Norway                             
## [5327] NO-Norway                             
## [5328] NO-Norway                             
## [5329] NO-Norway                             
## [5330] NO-Norway                             
## [5331] NO-Norway                             
## [5332] NO-Norway                             
## [5333] NO-Norway                             
## [5334] NO-Norway                             
## [5335] NO-Norway                             
## [5336] NO-Norway                             
## [5337] NO-Norway                             
## [5338] NO-Norway                             
## [5339] NO-Norway                             
## [5340] NO-Norway                             
## [5341] NO-Norway                             
## [5342] NO-Norway                             
## [5343] NO-Norway                             
## [5344] NO-Norway                             
## [5345] NO-Norway                             
## [5346] NO-Norway                             
## [5347] NO-Norway                             
## [5348] NO-Norway                             
## [5349] NO-Norway                             
## [5350] NO-Norway                             
## [5351] NO-Norway                             
## [5352] NO-Norway                             
## [5353] NO-Norway                             
## [5354] NO-Norway                             
## [5355] NO-Norway                             
## [5356] NO-Norway                             
## [5357] NO-Norway                             
## [5358] NO-Norway                             
## [5359] NO-Norway                             
## [5360] NO-Norway                             
## [5361] NO-Norway                             
## [5362] NO-Norway                             
## [5363] NO-Norway                             
## [5364] NO-Norway                             
## [5365] NO-Norway                             
## [5366] NO-Norway                             
## [5367] NO-Norway                             
## [5368] NO-Norway                             
## [5369] NO-Norway                             
## [5370] NO-Norway                             
## [5371] NO-Norway                             
## [5372] NO-Norway                             
## [5373] NO-Norway                             
## [5374] NO-Norway                             
## [5375] NO-Norway                             
## [5376] NO-Norway                             
## [5377] NO-Norway                             
## [5378] NO-Norway                             
## [5379] NO-Norway                             
## [5380] NO-Norway                             
## [5381] NO-Norway                             
## [5382] NO-Norway                             
## [5383] NO-Norway                             
## [5384] NO-Norway                             
## [5385] NO-Norway                             
## [5386] NO-Norway                             
## [5387] NO-Norway                             
## [5388] NO-Norway                             
## [5389] NO-Norway                             
## [5390] NO-Norway                             
## [5391] NO-Norway                             
## [5392] NO-Norway                             
## [5393] NO-Norway                             
## [5394] NO-Norway                             
## [5395] NO-Norway                             
## [5396] NO-Norway                             
## [5397] NO-Norway                             
## [5398] NO-Norway                             
## [5399] NO-Norway                             
## [5400] NO-Norway                             
## [5401] NO-Norway                             
## [5402] NO-Norway                             
## [5403] NO-Norway                             
## [5404] NO-Norway                             
## [5405] NO-Norway                             
## [5406] NO-Norway                             
## [5407] NO-Norway                             
## [5408] NO-Norway                             
## [5409] NO-Norway                             
## [5410] NO-Norway                             
## [5411] NO-Norway                             
## [5412] NO-Norway                             
## [5413] NO-Norway                             
## [5414] NO-Norway                             
## [5415] NO-Norway                             
## [5416] NO-Norway                             
## [5417] NO-Norway                             
## [5418] NO-Norway                             
## [5419] NO-Norway                             
## [5420] NO-Norway                             
## [5421] NO-Norway                             
## [5422] NO-Norway                             
## [5423] NO-Norway                             
## [5424] NO-Norway                             
## [5425] NO-Norway                             
## [5426] NO-Norway                             
## [5427] NO-Norway                             
## [5428] NO-Norway                             
## [5429] NO-Norway                             
## [5430] NO-Norway                             
## [5431] NO-Norway                             
## [5432] NO-Norway                             
## [5433] NO-Norway                             
## [5434] NO-Norway                             
## [5435] NO-Norway                             
## [5436] NO-Norway                             
## [5437] NO-Norway                             
## [5438] NO-Norway                             
## [5439] NO-Norway                             
## [5440] NO-Norway                             
## [5441] NO-Norway                             
## [5442] NO-Norway                             
## [5443] NO-Norway                             
## [5444] NO-Norway                             
## [5445] NO-Norway                             
## [5446] NO-Norway                             
## [5447] NO-Norway                             
## [5448] NO-Norway                             
## [5449] NO-Norway                             
## [5450] NO-Norway                             
## [5451] NO-Norway                             
## [5452] NO-Norway                             
## [5453] NO-Norway                             
## [5454] NO-Norway                             
## [5455] NO-Norway                             
## [5456] NO-Norway                             
## [5457] NO-Norway                             
## [5458] NO-Norway                             
## [5459] NO-Norway                             
## [5460] NO-Norway                             
## [5461] NO-Norway                             
## [5462] NO-Norway                             
## [5463] NO-Norway                             
## [5464] NO-Norway                             
## [5465] NO-Norway                             
## [5466] NO-Norway                             
## [5467] NO-Norway                             
## [5468] NO-Norway                             
## [5469] NO-Norway                             
## [5470] NO-Norway                             
## [5471] NO-Norway                             
## [5472] NO-Norway                             
## [5473] NO-Norway                             
## [5474] NO-Norway                             
## [5475] NO-Norway                             
## [5476] NO-Norway                             
## [5477] NO-Norway                             
## [5478] NO-Norway                             
## [5479] NO-Norway                             
## [5480] NO-Norway                             
## [5481] NO-Norway                             
## [5482] NO-Norway                             
## [5483] NO-Norway                             
## [5484] NO-Norway                             
## [5485] NO-Norway                             
## [5486] NO-Norway                             
## [5487] NO-Norway                             
## [5488] NO-Norway                             
## [5489] NO-Norway                             
## [5490] NO-Norway                             
## [5491] NO-Norway                             
## [5492] NO-Norway                             
## [5493] NO-Norway                             
## [5494] NO-Norway                             
## [5495] NO-Norway                             
## [5496] NO-Norway                             
## [5497] NO-Norway                             
## [5498] NO-Norway                             
## [5499] NO-Norway                             
## [5500] NO-Norway                             
## [5501] NO-Norway                             
## [5502] NO-Norway                             
## [5503] NO-Norway                             
## [5504] NO-Norway                             
## [5505] NO-Norway                             
## [5506] NO-Norway                             
## [5507] NO-Norway                             
## [5508] NO-Norway                             
## [5509] NO-Norway                             
## [5510] NO-Norway                             
## [5511] NO-Norway                             
## [5512] NO-Norway                             
## [5513] NO-Norway                             
## [5514] NO-Norway                             
## [5515] NO-Norway                             
## [5516] NO-Norway                             
## [5517] NO-Norway                             
## [5518] NO-Norway                             
## [5519] NO-Norway                             
## [5520] NO-Norway                             
## [5521] NO-Norway                             
## [5522] NO-Norway                             
## [5523] NO-Norway                             
## [5524] NO-Norway                             
## [5525] NO-Norway                             
## [5526] NO-Norway                             
## [5527] NO-Norway                             
## [5528] NO-Norway                             
## [5529] NO-Norway                             
## [5530] NO-Norway                             
## [5531] NO-Norway                             
## [5532] NO-Norway                             
## [5533] NO-Norway                             
## [5534] NO-Norway                             
## [5535] NO-Norway                             
## [5536] NO-Norway                             
## [5537] NO-Norway                             
## [5538] NO-Norway                             
## [5539] NO-Norway                             
## [5540] NO-Norway                             
## [5541] NO-Norway                             
## [5542] NO-Norway                             
## [5543] NO-Norway                             
## [5544] NO-Norway                             
## [5545] NO-Norway                             
## [5546] NO-Norway                             
## [5547] NO-Norway                             
## [5548] NO-Norway                             
## [5549] NO-Norway                             
## [5550] NO-Norway                             
## [5551] NO-Norway                             
## [5552] NO-Norway                             
## [5553] NO-Norway                             
## [5554] NO-Norway                             
## [5555] NO-Norway                             
## [5556] NO-Norway                             
## [5557] NO-Norway                             
## [5558] NO-Norway                             
## [5559] NO-Norway                             
## [5560] NO-Norway                             
## [5561] NO-Norway                             
## [5562] NO-Norway                             
## [5563] NO-Norway                             
## [5564] NO-Norway                             
## [5565] NO-Norway                             
## [5566] NO-Norway                             
## [5567] NO-Norway                             
## [5568] NO-Norway                             
## [5569] NO-Norway                             
## [5570] NO-Norway                             
## [5571] NO-Norway                             
## [5572] NO-Norway                             
## [5573] NO-Norway                             
## [5574] NO-Norway                             
## [5575] NO-Norway                             
## [5576] NO-Norway                             
## [5577] NO-Norway                             
## [5578] NO-Norway                             
## [5579] NO-Norway                             
## [5580] NO-Norway                             
## [5581] NO-Norway                             
## [5582] NO-Norway                             
## [5583] NO-Norway                             
## [5584] NO-Norway                             
## [5585] NO-Norway                             
## [5586] NO-Norway                             
## [5587] NO-Norway                             
## [5588] NO-Norway                             
## [5589] NO-Norway                             
## [5590] NO-Norway                             
## [5591] NO-Norway                             
## [5592] NO-Norway                             
## [5593] NO-Norway                             
## [5594] NO-Norway                             
## [5595] NO-Norway                             
## [5596] NO-Norway                             
## [5597] NO-Norway                             
## [5598] NO-Norway                             
## [5599] NO-Norway                             
## [5600] NO-Norway                             
## [5601] NO-Norway                             
## [5602] NO-Norway                             
## [5603] NO-Norway                             
## [5604] NO-Norway                             
## [5605] NO-Norway                             
## [5606] NO-Norway                             
## [5607] NO-Norway                             
## [5608] NO-Norway                             
## [5609] NO-Norway                             
## [5610] NO-Norway                             
## [5611] NO-Norway                             
## [5612] NO-Norway                             
## [5613] NO-Norway                             
## [5614] NO-Norway                             
## [5615] NO-Norway                             
## [5616] NO-Norway                             
## [5617] NO-Norway                             
## [5618] NO-Norway                             
## [5619] NO-Norway                             
## [5620] NO-Norway                             
## [5621] NO-Norway                             
## [5622] NO-Norway                             
## [5623] NO-Norway                             
## [5624] NO-Norway                             
## [5625] NO-Norway                             
## [5626] NO-Norway                             
## [5627] NO-Norway                             
## [5628] NO-Norway                             
## [5629] NO-Norway                             
## [5630] NO-Norway                             
## [5631] NO-Norway                             
## [5632] NO-Norway                             
## [5633] NO-Norway                             
## [5634] NO-Norway                             
## [5635] NO-Norway                             
## [5636] NO-Norway                             
## [5637] NO-Norway                             
## [5638] NO-Norway                             
## [5639] NO-Norway                             
## [5640] NO-Norway                             
## [5641] NO-Norway                             
## [5642] NO-Norway                             
## [5643] NO-Norway                             
## [5644] NO-Norway                             
## [5645] NO-Norway                             
## [5646] NO-Norway                             
## [5647] NO-Norway                             
## [5648] NO-Norway                             
## [5649] NO-Norway                             
## [5650] NO-Norway                             
## [5651] NO-Norway                             
## [5652] NO-Norway                             
## [5653] NO-Norway                             
## [5654] NO-Norway                             
## [5655] NO-Norway                             
## [5656] NO-Norway                             
## [5657] NO-Norway                             
## [5658] NO-Norway                             
## [5659] NO-Norway                             
## [5660] NO-Norway                             
## [5661] NO-Norway                             
## [5662] NO-Norway                             
## [5663] NO-Norway                             
## [5664] NO-Norway                             
## [5665] NO-Norway                             
## [5666] NO-Norway                             
## [5667] NO-Norway                             
## [5668] NO-Norway                             
## [5669] NO-Norway                             
## [5670] NO-Norway                             
## [5671] NO-Norway                             
## [5672] NO-Norway                             
## [5673] NO-Norway                             
## [5674] NO-Norway                             
## [5675] NO-Norway                             
## [5676] NO-Norway                             
## [5677] NO-Norway                             
## [5678] NO-Norway                             
## [5679] NO-Norway                             
## [5680] NO-Norway                             
## [5681] NO-Norway                             
## [5682] NO-Norway                             
## [5683] NO-Norway                             
## [5684] NO-Norway                             
## [5685] NO-Norway                             
## [5686] NO-Norway                             
## [5687] NO-Norway                             
## [5688] NO-Norway                             
## [5689] NO-Norway                             
## [5690] NO-Norway                             
## [5691] NO-Norway                             
## [5692] NO-Norway                             
## [5693] NO-Norway                             
## [5694] NO-Norway                             
## [5695] NO-Norway                             
## [5696] NO-Norway                             
## [5697] NO-Norway                             
## [5698] NO-Norway                             
## [5699] NO-Norway                             
## [5700] NO-Norway                             
## [5701] NO-Norway                             
## [5702] NO-Norway                             
## [5703] NO-Norway                             
## [5704] NO-Norway                             
## [5705] NO-Norway                             
## [5706] NO-Norway                             
## [5707] NO-Norway                             
## [5708] NO-Norway                             
## [5709] NO-Norway                             
## [5710] NO-Norway                             
## [5711] NO-Norway                             
## [5712] NO-Norway                             
## [5713] NO-Norway                             
## [5714] NO-Norway                             
## [5715] NO-Norway                             
## [5716] NO-Norway                             
## [5717] NO-Norway                             
## [5718] NO-Norway                             
## [5719] NO-Norway                             
## [5720] NO-Norway                             
## [5721] NO-Norway                             
## [5722] NO-Norway                             
## [5723] NO-Norway                             
## [5724] NO-Norway                             
## [5725] NO-Norway                             
## [5726] NO-Norway                             
## [5727] NO-Norway                             
## [5728] NO-Norway                             
## [5729] NO-Norway                             
## [5730] NO-Norway                             
## [5731] NO-Norway                             
## [5732] NO-Norway                             
## [5733] NO-Norway                             
## [5734] NO-Norway                             
## [5735] NO-Norway                             
## [5736] NO-Norway                             
## [5737] NO-Norway                             
## [5738] NO-Norway                             
## [5739] NO-Norway                             
## [5740] NO-Norway                             
## [5741] NO-Norway                             
## [5742] NO-Norway                             
## [5743] NO-Norway                             
## [5744] NO-Norway                             
## [5745] NO-Norway                             
## [5746] NO-Norway                             
## [5747] NO-Norway                             
## [5748] NO-Norway                             
## [5749] NO-Norway                             
## [5750] NO-Norway                             
## [5751] NO-Norway                             
## [5752] NO-Norway                             
## [5753] NO-Norway                             
## [5754] NO-Norway                             
## [5755] NO-Norway                             
## [5756] NO-Norway                             
## [5757] NO-Norway                             
## [5758] NO-Norway                             
## [5759] NO-Norway                             
## [5760] NO-Norway                             
## [5761] NO-Norway                             
## [5762] NO-Norway                             
## [5763] NO-Norway                             
## [5764] NO-Norway                             
## [5765] NO-Norway                             
## [5766] NO-Norway                             
## [5767] NO-Norway                             
## [5768] NO-Norway                             
## [5769] NO-Norway                             
## [5770] NO-Norway                             
## [5771] NO-Norway                             
## [5772] NO-Norway                             
## [5773] NO-Norway                             
## [5774] NO-Norway                             
## [5775] NO-Norway                             
## [5776] NO-Norway                             
## [5777] NO-Norway                             
## [5778] NO-Norway                             
## [5779] NO-Norway                             
## [5780] NO-Norway                             
## [5781] NO-Norway                             
## [5782] NO-Norway                             
## [5783] NO-Norway                             
## [5784] NO-Norway                             
## [5785] NO-Norway                             
## [5786] NO-Norway                             
## [5787] NO-Norway                             
## [5788] NO-Norway                             
## [5789] NO-Norway                             
## [5790] NO-Norway                             
## [5791] NO-Norway                             
## [5792] NO-Norway                             
## [5793] NO-Norway                             
## [5794] NO-Norway                             
## [5795] NO-Norway                             
## [5796] NO-Norway                             
## [5797] NO-Norway                             
## [5798] NO-Norway                             
## [5799] NO-Norway                             
## [5800] NO-Norway                             
## [5801] NO-Norway                             
## [5802] NO-Norway                             
## [5803] NO-Norway                             
## [5804] NO-Norway                             
## [5805] NO-Norway                             
## [5806] NO-Norway                             
## [5807] NO-Norway                             
## [5808] NO-Norway                             
## [5809] NO-Norway                             
## [5810] NO-Norway                             
## [5811] NO-Norway                             
## [5812] NO-Norway                             
## [5813] NO-Norway                             
## [5814] NO-Norway                             
## [5815] NO-Norway                             
## [5816] NO-Norway                             
## [5817] NO-Norway                             
## [5818] NO-Norway                             
## [5819] NO-Norway                             
## [5820] NO-Norway                             
## [5821] NO-Norway                             
## [5822] NO-Norway                             
## [5823] NO-Norway                             
## [5824] NO-Norway                             
## [5825] NO-Norway                             
## [5826] NO-Norway                             
## [5827] NO-Norway                             
## [5828] NO-Norway                             
## [5829] NO-Norway                             
## [5830] NO-Norway                             
## [5831] NO-Norway                             
## [5832] NO-Norway                             
## [5833] NO-Norway                             
## [5834] NO-Norway                             
## [5835] NO-Norway                             
## [5836] NO-Norway                             
## [5837] NO-Norway                             
## [5838] NO-Norway                             
## [5839] NO-Norway                             
## [5840] NO-Norway                             
## [5841] NO-Norway                             
## [5842] NO-Norway                             
## [5843] NO-Norway                             
## [5844] NO-Norway                             
## [5845] NO-Norway                             
## [5846] NO-Norway                             
## [5847] NO-Norway                             
## [5848] NO-Norway                             
## [5849] NO-Norway                             
## [5850] NO-Norway                             
## [5851] NO-Norway                             
## [5852] NO-Norway                             
## [5853] NO-Norway                             
## [5854] NO-Norway                             
## [5855] NO-Norway                             
## [5856] NO-Norway                             
## [5857] NO-Norway                             
## [5858] NO-Norway                             
## [5859] NO-Norway                             
## [5860] NO-Norway                             
## [5861] NO-Norway                             
## [5862] NO-Norway                             
## [5863] NO-Norway                             
## [5864] NO-Norway                             
## [5865] NO-Norway                             
## [5866] NO-Norway                             
## [5867] NO-Norway                             
## [5868] NO-Norway                             
## [5869] NO-Norway                             
## [5870] NO-Norway                             
## [5871] NO-Norway                             
## [5872] NO-Norway                             
## [5873] NO-Norway                             
## [5874] NO-Norway                             
## [5875] NO-Norway                             
## [5876] NO-Norway                             
## [5877] NO-Norway                             
## [5878] NO-Norway                             
## [5879] NO-Norway                             
## [5880] NO-Norway                             
## [5881] NO-Norway                             
## [5882] NO-Norway                             
## [5883] NO-Norway                             
## [5884] NO-Norway                             
## [5885] NO-Norway                             
## [5886] NO-Norway                             
## [5887] NO-Norway                             
## [5888] NO-Norway                             
## [5889] NO-Norway                             
## [5890] NO-Norway                             
## [5891] NO-Norway                             
## [5892] NO-Norway                             
## [5893] NO-Norway                             
## [5894] NO-Norway                             
## [5895] NO-Norway                             
## [5896] NO-Norway                             
## [5897] NO-Norway                             
## [5898] NO-Norway                             
## [5899] NO-Norway                             
## [5900] NO-Norway                             
## [5901] NO-Norway                             
## [5902] NO-Norway                             
## [5903] NO-Norway                             
## [5904] NO-Norway                             
## [5905] NO-Norway                             
## [5906] NO-Norway                             
## [5907] NO-Norway                             
## [5908] NO-Norway                             
## [5909] NO-Norway                             
## [5910] NO-Norway                             
## [5911] NO-Norway                             
## [5912] NO-Norway                             
## [5913] NO-Norway                             
## [5914] NO-Norway                             
## [5915] NO-Norway                             
## [5916] NO-Norway                             
## [5917] NO-Norway                             
## [5918] NO-Norway                             
## [5919] NO-Norway                             
## [5920] NO-Norway                             
## [5921] NO-Norway                             
## [5922] NO-Norway                             
## [5923] NO-Norway                             
## [5924] NO-Norway                             
## [5925] NO-Norway                             
## [5926] NO-Norway                             
## [5927] NO-Norway                             
## [5928] NO-Norway                             
## [5929] NO-Norway                             
## [5930] NO-Norway                             
## [5931] NO-Norway                             
## [5932] NO-Norway                             
## [5933] NO-Norway                             
## [5934] NO-Norway                             
## [5935] NO-Norway                             
## [5936] NO-Norway                             
## [5937] NO-Norway                             
## [5938] NO-Norway                             
## [5939] NO-Norway                             
## [5940] NO-Norway                             
## [5941] NO-Norway                             
## [5942] NO-Norway                             
## [5943] NO-Norway                             
## [5944] NO-Norway                             
## [5945] NO-Norway                             
## [5946] NO-Norway                             
## [5947] NO-Norway                             
## [5948] NO-Norway                             
## [5949] NO-Norway                             
## [5950] NO-Norway                             
## [5951] NO-Norway                             
## [5952] NO-Norway                             
## [5953] NO-Norway                             
## [5954] NO-Norway                             
## [5955] NO-Norway                             
## [5956] NO-Norway                             
## [5957] NO-Norway                             
## [5958] NO-Norway                             
## [5959] NO-Norway                             
## [5960] NO-Norway                             
## [5961] NO-Norway                             
## [5962] NO-Norway                             
## [5963] NO-Norway                             
## [5964] NO-Norway                             
## [5965] NO-Norway                             
## [5966] NO-Norway                             
## [5967] NO-Norway                             
## [5968] NO-Norway                             
## [5969] NO-Norway                             
## [5970] NO-Norway                             
## [5971] NO-Norway                             
## [5972] NO-Norway                             
## [5973] NO-Norway                             
## [5974] NO-Norway                             
## [5975] NO-Norway                             
## [5976] NO-Norway                             
## [5977] NO-Norway                             
## [5978] NO-Norway                             
## [5979] NO-Norway                             
## [5980] NO-Norway                             
## [5981] NO-Norway                             
## [5982] NO-Norway                             
## [5983] NO-Norway                             
## [5984] NO-Norway                             
## [5985] NO-Norway                             
## [5986] NO-Norway                             
## [5987] NO-Norway                             
## [5988] NO-Norway                             
## [5989] NO-Norway                             
## [5990] NO-Norway                             
## [5991] NO-Norway                             
## [5992] NO-Norway                             
## [5993] NO-Norway                             
## [5994] NO-Norway                             
## [5995] NO-Norway                             
## [5996] NO-Norway                             
## [5997] NO-Norway                             
## [5998] NO-Norway                             
## [5999] NO-Norway                             
## [6000] NO-Norway                             
## [6001] NO-Norway                             
## [6002] NO-Norway                             
## [6003] NO-Norway                             
## [6004] NO-Norway                             
## [6005] NO-Norway                             
## [6006] NO-Norway                             
## [6007] NO-Norway                             
## [6008] NO-Norway                             
## [6009] NO-Norway                             
## [6010] NO-Norway                             
## [6011] NO-Norway                             
## [6012] NO-Norway                             
## [6013] NO-Norway                             
## [6014] NO-Norway                             
## [6015] NO-Norway                             
## [6016] NO-Norway                             
## [6017] NO-Norway                             
## [6018] NO-Norway                             
## [6019] NO-Norway                             
## [6020] NO-Norway                             
## [6021] NO-Norway                             
## [6022] NO-Norway                             
## [6023] NO-Norway                             
## [6024] NO-Norway                             
## [6025] NO-Norway                             
## [6026] NO-Norway                             
## [6027] NO-Norway                             
## [6028] NO-Norway                             
## [6029] NO-Norway                             
## [6030] NO-Norway                             
## [6031] NO-Norway                             
## [6032] NO-Norway                             
## [6033] NO-Norway                             
## [6034] NO-Norway                             
## [6035] NO-Norway                             
## [6036] NO-Norway                             
## [6037] NO-Norway                             
## [6038] NO-Norway                             
## [6039] NO-Norway                             
## [6040] NO-Norway                             
## [6041] NO-Norway                             
## [6042] NO-Norway                             
## [6043] NO-Norway                             
## [6044] NO-Norway                             
## [6045] NO-Norway                             
## [6046] NO-Norway                             
## [6047] NO-Norway                             
## [6048] NO-Norway                             
## [6049] NO-Norway                             
## [6050] NO-Norway                             
## [6051] NO-Norway                             
## [6052] NO-Norway                             
## [6053] NO-Norway                             
## [6054] NO-Norway                             
## [6055] NO-Norway                             
## [6056] NO-Norway                             
## [6057] NO-Norway                             
## [6058] NO-Norway                             
## [6059] NO-Norway                             
## [6060] NO-Norway                             
## [6061] NO-Norway                             
## [6062] NO-Norway                             
## [6063] NO-Norway                             
## [6064] NO-Norway                             
## [6065] NO-Norway                             
## [6066] NO-Norway                             
## [6067] NO-Norway                             
## [6068] NO-Norway                             
## [6069] NO-Norway                             
## [6070] NO-Norway                             
## [6071] NO-Norway                             
## [6072] NO-Norway                             
## [6073] NO-Norway                             
## [6074] NO-Norway                             
## [6075] NO-Norway                             
## [6076] NO-Norway                             
## [6077] NO-Norway                             
## [6078] NO-Norway                             
## [6079] NO-Norway                             
## [6080] NO-Norway                             
## [6081] NO-Norway                             
## [6082] NO-Norway                             
## [6083] NO-Norway                             
## [6084] NO-Norway                             
## [6085] NO-Norway                             
## [6086] NO-Norway                             
## [6087] NO-Norway                             
## [6088] NO-Norway                             
## [6089] NO-Norway                             
## [6090] NO-Norway                             
## [6091] NO-Norway                             
## [6092] NO-Norway                             
## [6093] NO-Norway                             
## [6094] NO-Norway                             
## [6095] NO-Norway                             
## [6096] NO-Norway                             
## [6097] NO-Norway                             
## [6098] NO-Norway                             
## [6099] NO-Norway                             
## [6100] NO-Norway                             
## [6101] NO-Norway                             
## [6102] NO-Norway                             
## [6103] NO-Norway                             
## [6104] NO-Norway                             
## [6105] NO-Norway                             
## [6106] NO-Norway                             
## [6107] NO-Norway                             
## [6108] NO-Norway                             
## [6109] NO-Norway                             
## [6110] NO-Norway                             
## [6111] NO-Norway                             
## [6112] NO-Norway                             
## [6113] NO-Norway                             
## [6114] NO-Norway                             
## [6115] NO-Norway                             
## [6116] NO-Norway                             
## [6117] NO-Norway                             
## [6118] NO-Norway                             
## [6119] NO-Norway                             
## [6120] NO-Norway                             
## [6121] NO-Norway                             
## [6122] NO-Norway                             
## [6123] NO-Norway                             
## [6124] NO-Norway                             
## [6125] NO-Norway                             
## [6126] NO-Norway                             
## [6127] NO-Norway                             
## [6128] NO-Norway                             
## [6129] NO-Norway                             
## [6130] NO-Norway                             
## [6131] NO-Norway                             
## [6132] NO-Norway                             
## [6133] NO-Norway                             
## [6134] NO-Norway                             
## [6135] NO-Norway                             
## [6136] NO-Norway                             
## [6137] NO-Norway                             
## [6138] NO-Norway                             
## [6139] NO-Norway                             
## [6140] NO-Norway                             
## [6141] NO-Norway                             
## [6142] NO-Norway                             
## [6143] NO-Norway                             
## [6144] NO-Norway                             
## [6145] NO-Norway                             
## [6146] NO-Norway                             
## [6147] NO-Norway                             
## [6148] NO-Norway                             
## [6149] NO-Norway                             
## [6150] NO-Norway                             
## [6151] NO-Norway                             
## [6152] NO-Norway                             
## [6153] NO-Norway                             
## [6154] NO-Norway                             
## [6155] NO-Norway                             
## [6156] NO-Norway                             
## [6157] NO-Norway                             
## [6158] NO-Norway                             
## [6159] NO-Norway                             
## [6160] NO-Norway                             
## [6161] NO-Norway                             
## [6162] NO-Norway                             
## [6163] NO-Norway                             
## [6164] NO-Norway                             
## [6165] NO-Norway                             
## [6166] NO-Norway                             
## [6167] NO-Norway                             
## [6168] NO-Norway                             
## [6169] NO-Norway                             
## [6170] NO-Norway                             
## [6171] NO-Norway                             
## [6172] NO-Norway                             
## [6173] NO-Norway                             
## [6174] NO-Norway                             
## [6175] NO-Norway                             
## [6176] NO-Norway                             
## [6177] NO-Norway                             
## [6178] NO-Norway                             
## [6179] NO-Norway                             
## [6180] NO-Norway                             
## [6181] NO-Norway                             
## [6182] NO-Norway                             
## [6183] NO-Norway                             
## [6184] NO-Norway                             
## [6185] NO-Norway                             
## [6186] NO-Norway                             
## [6187] NO-Norway                             
## [6188] NO-Norway                             
## [6189] NO-Norway                             
## [6190] NO-Norway                             
## [6191] NO-Norway                             
## [6192] NO-Norway                             
## [6193] NO-Norway                             
## [6194] NO-Norway                             
## [6195] NO-Norway                             
## [6196] NO-Norway                             
## [6197] NO-Norway                             
## [6198] NO-Norway                             
## [6199] NO-Norway                             
## [6200] NO-Norway                             
## [6201] NO-Norway                             
## [6202] NO-Norway                             
## [6203] NO-Norway                             
## [6204] NO-Norway                             
## [6205] NO-Norway                             
## [6206] NO-Norway                             
## [6207] NO-Norway                             
## [6208] NO-Norway                             
## [6209] NO-Norway                             
## [6210] NO-Norway                             
## [6211] NO-Norway                             
## [6212] NO-Norway                             
## [6213] NO-Norway                             
## [6214] NO-Norway                             
## [6215] NO-Norway                             
## [6216] NO-Norway                             
## [6217] NO-Norway                             
## [6218] NO-Norway                             
## [6219] NO-Norway                             
## [6220] NO-Norway                             
## [6221] NO-Norway                             
## [6222] NO-Norway                             
## [6223] NO-Norway                             
## [6224] NO-Norway                             
## [6225] NO-Norway                             
## [6226] NO-Norway                             
## [6227] NO-Norway                             
## [6228] NO-Norway                             
## [6229] NO-Norway                             
## [6230] NO-Norway                             
## [6231] NO-Norway                             
## [6232] NO-Norway                             
## [6233] NO-Norway                             
## [6234] NO-Norway                             
## [6235] NO-Norway                             
## [6236] NO-Norway                             
## [6237] NO-Norway                             
## [6238] NO-Norway                             
## [6239] NO-Norway                             
## [6240] NO-Norway                             
## [6241] NO-Norway                             
## [6242] NO-Norway                             
## [6243] NO-Norway                             
## [6244] NO-Norway                             
## [6245] NO-Norway                             
## [6246] NO-Norway                             
## [6247] NO-Norway                             
## [6248] NO-Norway                             
## [6249] NO-Norway                             
## [6250] NO-Norway                             
## [6251] NO-Norway                             
## [6252] NO-Norway                             
## [6253] NO-Norway                             
## [6254] NO-Norway                             
## [6255] NO-Norway                             
## [6256] NO-Norway                             
## [6257] NO-Norway                             
## [6258] NO-Norway                             
## [6259] NO-Norway                             
## [6260] NO-Norway                             
## [6261] NO-Norway                             
## [6262] NO-Norway                             
## [6263] NO-Norway                             
## [6264] NO-Norway                             
## [6265] NO-Norway                             
## [6266] NO-Norway                             
## [6267] NO-Norway                             
## [6268] NO-Norway                             
## [6269] NO-Norway                             
## [6270] NO-Norway                             
## [6271] NO-Norway                             
## [6272] NO-Norway                             
## [6273] NO-Norway                             
## [6274] NO-Norway                             
## [6275] NO-Norway                             
## [6276] NO-Norway                             
## [6277] NO-Norway                             
## [6278] NO-Norway                             
## [6279] NO-Norway                             
## [6280] NO-Norway                             
## [6281] NO-Norway                             
## [6282] NO-Norway                             
## [6283] NO-Norway                             
## [6284] NO-Norway                             
## [6285] NO-Norway                             
## [6286] NO-Norway                             
## [6287] NO-Norway                             
## [6288] NO-Norway                             
## [6289] NO-Norway                             
## [6290] NO-Norway                             
## [6291] NO-Norway                             
## [6292] NO-Norway                             
## [6293] NO-Norway                             
## [6294] NO-Norway                             
## [6295] NO-Norway                             
## [6296] NO-Norway                             
## [6297] NO-Norway                             
## [6298] NO-Norway                             
## [6299] NO-Norway                             
## [6300] NO-Norway                             
## [6301] NO-Norway                             
## [6302] NO-Norway                             
## [6303] NO-Norway                             
## [6304] NO-Norway                             
## [6305] NO-Norway                             
## [6306] NO-Norway                             
## [6307] NO-Norway                             
## [6308] NO-Norway                             
## [6309] NO-Norway                             
## [6310] NO-Norway                             
## [6311] NO-Norway                             
## [6312] NO-Norway                             
## [6313] NO-Norway                             
## [6314] NO-Norway                             
## [6315] NO-Norway                             
## [6316] NO-Norway                             
## [6317] NO-Norway                             
## [6318] NO-Norway                             
## [6319] NO-Norway                             
## [6320] NO-Norway                             
## [6321] NO-Norway                             
## [6322] NO-Norway                             
## [6323] NO-Norway                             
## [6324] NO-Norway                             
## [6325] NO-Norway                             
## [6326] NO-Norway                             
## [6327] NO-Norway                             
## [6328] NO-Norway                             
## [6329] NO-Norway                             
## [6330] NO-Norway                             
## [6331] NO-Norway                             
## [6332] NO-Norway                             
## [6333] NO-Norway                             
## [6334] NO-Norway                             
## [6335] NO-Norway                             
## [6336] NO-Norway                             
## [6337] NO-Norway                             
## [6338] NO-Norway                             
## [6339] NO-Norway                             
## [6340] NO-Norway                             
## [6341] NO-Norway                             
## [6342] NO-Norway                             
## [6343] NO-Norway                             
## [6344] NO-Norway                             
## [6345] NO-Norway                             
## [6346] NO-Norway                             
## [6347] NO-Norway                             
## [6348] NO-Norway                             
## [6349] NO-Norway                             
## [6350] NO-Norway                             
## [6351] NO-Norway                             
## [6352] NO-Norway                             
## [6353] NO-Norway                             
## [6354] NO-Norway                             
## [6355] NO-Norway                             
## [6356] NO-Norway                             
## [6357] NO-Norway                             
## [6358] NO-Norway                             
## [6359] NO-Norway                             
## [6360] NO-Norway                             
## [6361] NO-Norway                             
## [6362] NO-Norway                             
## [6363] NO-Norway                             
## [6364] NO-Norway                             
## [6365] NO-Norway                             
## [6366] NO-Norway                             
## [6367] NO-Norway                             
## [6368] NO-Norway                             
## [6369] NO-Norway                             
## [6370] NO-Norway                             
## [6371] NO-Norway                             
## [6372] NO-Norway                             
## [6373] NO-Norway                             
## [6374] NO-Norway                             
## [6375] NO-Norway                             
## [6376] NO-Norway                             
## [6377] NO-Norway                             
## [6378] NO-Norway                             
## [6379] NO-Norway                             
## [6380] NO-Norway                             
## [6381] NO-Norway                             
## [6382] NO-Norway                             
## [6383] NO-Norway                             
## [6384] NO-Norway                             
## [6385] NO-Norway                             
## [6386] NO-Norway                             
## [6387] NO-Norway                             
## [6388] NO-Norway                             
## [6389] NO-Norway                             
## [6390] NO-Norway                             
## [6391] NO-Norway                             
## [6392] NO-Norway                             
## [6393] NO-Norway                             
## [6394] NO-Norway                             
## [6395] NO-Norway                             
## [6396] NO-Norway                             
## [6397] NO-Norway                             
## [6398] NO-Norway                             
## [6399] NO-Norway                             
## [6400] NO-Norway                             
## [6401] NO-Norway                             
## [6402] NO-Norway                             
## [6403] NO-Norway                             
## [6404] NO-Norway                             
## [6405] NO-Norway                             
## [6406] NO-Norway                             
## [6407] NO-Norway                             
## [6408] NO-Norway                             
## [6409] NO-Norway                             
## [6410] NO-Norway                             
## [6411] NO-Norway                             
## [6412] NO-Norway                             
## [6413] NO-Norway                             
## [6414] NO-Norway                             
## [6415] NO-Norway                             
## [6416] NO-Norway                             
## [6417] NO-Norway                             
## [6418] NO-Norway                             
## [6419] NO-Norway                             
## [6420] NO-Norway                             
## [6421] NO-Norway                             
## [6422] NO-Norway                             
## [6423] NO-Norway                             
## [6424] NO-Norway                             
## [6425] NO-Norway                             
## [6426] SI-Slovenia                           
## [6427] ES-Spain                              
## [6428] ES-Spain                              
## [6429] ES-Spain                              
## [6430] ES-Spain                              
## [6431] ES-Spain                              
## [6432] ES-Spain                              
## [6433] ES-Spain                              
## [6434] ES-Spain                              
## [6435] ES-Spain                              
## [6436] ES-Spain                              
## [6437] ES-Spain                              
## [6438] ES-Spain                              
## [6439] ES-Spain                              
## [6440] GB-Great Britain and/or United Kingdom
## [6441] GB-Great Britain and/or United Kingdom
## [6442] GB-Great Britain and/or United Kingdom
## [6443] GB-Great Britain and/or United Kingdom
## [6444] GB-Great Britain and/or United Kingdom
## [6445] GB-Great Britain and/or United Kingdom
## [6446] GB-Great Britain and/or United Kingdom
## [6447] GB-Great Britain and/or United Kingdom
## [6448] GB-Great Britain and/or United Kingdom
## [6449] GB-Great Britain and/or United Kingdom
## [6450] GB-Great Britain and/or United Kingdom
## [6451] GB-Great Britain and/or United Kingdom
## [6452] GB-Great Britain and/or United Kingdom
## [6453] GB-Great Britain and/or United Kingdom
## [6454] GB-Great Britain and/or United Kingdom
## [6455] GB-Great Britain and/or United Kingdom
## [6456] GB-Great Britain and/or United Kingdom
## [6457] GB-Great Britain and/or United Kingdom
## [6458] GB-Great Britain and/or United Kingdom
## [6459] GB-Great Britain and/or United Kingdom
## [6460] GB-Great Britain and/or United Kingdom
## [6461] GB-Great Britain and/or United Kingdom
## [6462] GB-Great Britain and/or United Kingdom
## [6463] GB-Great Britain and/or United Kingdom
## [6464] GB-Great Britain and/or United Kingdom
## [6465] GB-Great Britain and/or United Kingdom
## [6466] GB-Great Britain and/or United Kingdom
## [6467] GB-Great Britain and/or United Kingdom
## [6468] GB-Great Britain and/or United Kingdom
## [6469] GB-Great Britain and/or United Kingdom
## [6470] GB-Great Britain and/or United Kingdom
## [6471] GB-Great Britain and/or United Kingdom
## [6472] GB-Great Britain and/or United Kingdom
## [6473] GB-Great Britain and/or United Kingdom
## [6474] GB-Great Britain and/or United Kingdom
## [6475] GB-Great Britain and/or United Kingdom
## [6476] GB-Great Britain and/or United Kingdom
## [6477] GB-Great Britain and/or United Kingdom
## [6478] GB-Great Britain and/or United Kingdom
## [6479] GB-Great Britain and/or United Kingdom
## [6480] GB-Great Britain and/or United Kingdom
## [6481] US-United States                      
## 33 Levels: AR-Argentina AU-Australia AT-Austria BE-Belgium ... US-United States
```


It is important to know which countries contribute information to which period of time.


```r
library(ggplot2)
fukushima <- strptime("2011-03-13", format = "%Y-%m-%d")
ggplot(issp, aes(x = date, y = V4)) + geom_point() + geom_vline(xintercept = as.numeric(fukushima), 
    color = "red")
```

```
## Warning: Removed 6481 rows containing missing values (geom_point).
```

![plot of chunk timelines](figure/timelines.png) 


## Method

### Regression Discontinuity Design (RDD)

RDD's causal identification strategy is to compare cases which are very close to a certain cutoff point of threshold which assigns data into a control and treatment group. Concerning the Fukushima accident, the assignment variable is time and the threshold is the day of the accident: 13th March 2011. Whether the interview was conducted before the 13th or after appears to be random so comparing individuals surveyed just before and just after the accident does control for most unobserved heterogeneity. RDD is illustrated in the following figure:


```r
library(ggplot2)
library(grid)  #for function arrow()
x <- 1:100
set.seed(123)
y <- rnorm(100)
y[50:100] <- y[50:100] + 2
group <- c(rep(0, 50), rep(1, 50))
qplot(x, y, group = group) + stat_smooth(se = FALSE) + geom_text(show_guide = FALSE, 
    x = 50, y = 1.2, label = "Treatment \neffect", color = "black") + geom_segment(aes(x = 50, 
    y = 1.5, xend = 50, yend = 2.2), arrow = arrow(), color = "black") + geom_segment(aes(x = 50, 
    y = 0.8, xend = 50, yend = 0.45), arrow = arrow(), color = "black") + ggtitle("RDD Visualization") + 
    theme_bw()
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using
## loess. Use 'method = x' to change the smoothing method.
```

![plot of chunk rddillustration](figure/rddillustration.png) 


### Difference-in-Difference Estimator (DiD)

DiD's causal identification strategy is to get rid of multiple sources of unobserved heterogeneity by controlling for country-characteristics (country dummies) and period-characteristics (period dummies) which in our case is accomplished by estimating a Country-Fixed-Effects Modell including a "before/after-Fukushima-Dummy". There are two key assumptions to mention. First, DiD assumes the timetrend of $y$, i.e. attitudes towards nuclear energy to be stable over time. Second, as there are countries which are completely before or completely after the accident, the covariance of all variables needs to be stable accross countries. (das müssen wir noch diskutieren). DiD is illustrated in the next figure:


```r
time <- rep(c(0, 1), 3)
observed <- c(30, 15)
control <- c(20, 35)
counterfactual <- c(30, 45)
group <- factor(c(1, 1, 2, 2, 3, 3), labels = c("observed", "control", "counterfactual"))
y <- c(observed, control, counterfactual)
qplot(time, y, group = group, colour = group, ylim = c(0, 50), xlim = c(0, 1.06)) + 
    geom_line(size = 1) + ggtitle("DiD Visualization") + geom_segment(aes(x = 1, 
    y = 27, xend = 1, yend = 15.5), arrow = arrow(), color = "black") + geom_segment(aes(x = 1, 
    y = 33, xend = 1, yend = 44.5), arrow = arrow(), color = "black") + geom_text(show_guide = FALSE, 
    x = 1, y = 30, label = "Treatment \neffect", color = "black") + theme_bw()
```

![plot of chunk didillustration](figure/didillustration.png) 



## Models

### RDD


```r
library(rdd)
# Efficiency gains can be made by including covariates. I.e. country
# dummies. Good to think about a lot more...
RDestimate(y ~ x | cov, cutpoint = fukushimadatu, )
plot(RDestimate(y ~ x | cov))
```


### DiD


```r
# We need ordered logit regression here. Fixed effects can be achieved by
# including country-dummies.
library(MASS)
polr(y ~ after + country, data = issp)
```


