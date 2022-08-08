module PhpUpper (phpUpper) where

phpUpper :: Char -> Char
phpUpper 'a' = 'A'
phpUpper 'b' = 'B'
phpUpper 'c' = 'C'
phpUpper 'd' = 'D'
phpUpper 'e' = 'E'
phpUpper 'f' = 'F'
phpUpper 'g' = 'G'
phpUpper 'h' = 'H'
phpUpper 'i' = 'I'
phpUpper 'j' = 'J'
phpUpper 'k' = 'K'
phpUpper 'l' = 'L'
phpUpper 'm' = 'M'
phpUpper 'n' = 'N'
phpUpper 'o' = 'O'
phpUpper 'p' = 'P'
phpUpper 'q' = 'Q'
phpUpper 'r' = 'R'
phpUpper 's' = 'S'
phpUpper 't' = 'T'
phpUpper 'u' = 'U'
phpUpper 'v' = 'V'
phpUpper 'w' = 'W'
phpUpper 'x' = 'X'
phpUpper 'y' = 'Y'
phpUpper 'z' = 'Z'
-- à -> À -- LATIN CAPITAL LETTER A GRAVE
phpUpper '\224' = '\192'
-- á -> Á -- LATIN CAPITAL LETTER A ACUTE
phpUpper '\225' = '\193'
-- â -> Â -- LATIN CAPITAL LETTER A CIRCUMFLEX
phpUpper '\226' = '\194'
-- ã -> Ã -- LATIN CAPITAL LETTER A TILDE
phpUpper '\227' = '\195'
-- ä -> Ä -- LATIN CAPITAL LETTER A DIAERESIS
phpUpper '\228' = '\196'
-- å -> Å -- LATIN CAPITAL LETTER A RING
phpUpper '\229' = '\197'
-- æ -> Æ -- LATIN CAPITAL LETTER A E
phpUpper '\230' = '\198'
-- ç -> Ç -- LATIN CAPITAL LETTER C CEDILLA
phpUpper '\231' = '\199'
-- è -> È -- LATIN CAPITAL LETTER E GRAVE
phpUpper '\232' = '\200'
-- é -> É -- LATIN CAPITAL LETTER E ACUTE
phpUpper '\233' = '\201'
-- ê -> Ê -- LATIN CAPITAL LETTER E CIRCUMFLEX
phpUpper '\234' = '\202'
-- ë -> Ë -- LATIN CAPITAL LETTER E DIAERESIS
phpUpper '\235' = '\203'
-- ì -> Ì -- LATIN CAPITAL LETTER I GRAVE
phpUpper '\236' = '\204'
-- í -> Í -- LATIN CAPITAL LETTER I ACUTE
phpUpper '\237' = '\205'
-- î -> Î -- LATIN CAPITAL LETTER I CIRCUMFLEX
phpUpper '\238' = '\206'
-- ï -> Ï -- LATIN CAPITAL LETTER I DIAERESIS
phpUpper '\239' = '\207'
-- ð -> Ð -- LATIN CAPITAL LETTER ETH
phpUpper '\240' = '\208'
-- ñ -> Ñ -- LATIN CAPITAL LETTER N TILDE
phpUpper '\241' = '\209'
-- ò -> Ò -- LATIN CAPITAL LETTER O GRAVE
phpUpper '\242' = '\210'
-- ó -> Ó -- LATIN CAPITAL LETTER O ACUTE
phpUpper '\243' = '\211'
-- ô -> Ô -- LATIN CAPITAL LETTER O CIRCUMFLEX
phpUpper '\244' = '\212'
-- õ -> Õ -- LATIN CAPITAL LETTER O TILDE
phpUpper '\245' = '\213'
-- ö -> Ö -- LATIN CAPITAL LETTER O DIAERESIS
phpUpper '\246' = '\214'
-- ø -> Ø -- LATIN CAPITAL LETTER O SLASH
phpUpper '\248' = '\216'
-- ù -> Ù -- LATIN CAPITAL LETTER U GRAVE
phpUpper '\249' = '\217'
-- ú -> Ú -- LATIN CAPITAL LETTER U ACUTE
phpUpper '\250' = '\218'
-- û -> Û -- LATIN CAPITAL LETTER U CIRCUMFLEX
phpUpper '\251' = '\219'
-- ü -> Ü -- LATIN CAPITAL LETTER U DIAERESIS
phpUpper '\252' = '\220'
-- ý -> Ý -- LATIN CAPITAL LETTER Y ACUTE
phpUpper '\253' = '\221'
-- þ -> Þ -- LATIN CAPITAL LETTER THORN
phpUpper '\254' = '\222'
-- ÿ -> Ÿ -- LATIN SMALL LETTER Y DIAERESIS
phpUpper '\255' = '\376'
-- ā -> Ā -- LATIN CAPITAL LETTER A MACRON
phpUpper '\257' = '\256'
-- ă -> Ă -- LATIN CAPITAL LETTER A BREVE
phpUpper '\259' = '\258'
-- ą -> Ą -- LATIN CAPITAL LETTER A OGONEK
phpUpper '\261' = '\260'
-- ć -> Ć -- LATIN CAPITAL LETTER C ACUTE
phpUpper '\263' = '\262'
-- ĉ -> Ĉ -- LATIN CAPITAL LETTER C CIRCUMFLEX
phpUpper '\265' = '\264'
-- ċ -> Ċ -- LATIN CAPITAL LETTER C DOT
phpUpper '\267' = '\266'
-- č -> Č -- LATIN CAPITAL LETTER C HACEK
phpUpper '\269' = '\268'
-- ď -> Ď -- LATIN CAPITAL LETTER D HACEK
phpUpper '\271' = '\270'
-- đ -> Đ -- LATIN CAPITAL LETTER D BAR
phpUpper '\273' = '\272'
-- ē -> Ē -- LATIN CAPITAL LETTER E MACRON
phpUpper '\275' = '\274'
-- ĕ -> Ĕ -- LATIN CAPITAL LETTER E BREVE
phpUpper '\277' = '\276'
-- ė -> Ė -- LATIN CAPITAL LETTER E DOT
phpUpper '\279' = '\278'
-- ę -> Ę -- LATIN CAPITAL LETTER E OGONEK
phpUpper '\281' = '\280'
-- ě -> Ě -- LATIN CAPITAL LETTER E HACEK
phpUpper '\283' = '\282'
-- ĝ -> Ĝ -- LATIN CAPITAL LETTER G CIRCUMFLEX
phpUpper '\285' = '\284'
-- ğ -> Ğ -- LATIN CAPITAL LETTER G BREVE
phpUpper '\287' = '\286'
-- ġ -> Ġ -- LATIN CAPITAL LETTER G DOT
phpUpper '\289' = '\288'
-- ģ -> Ģ -- LATIN CAPITAL LETTER G CEDILLA
phpUpper '\291' = '\290'
-- ĥ -> Ĥ -- LATIN CAPITAL LETTER H CIRCUMFLEX
phpUpper '\293' = '\292'
-- ħ -> Ħ -- LATIN CAPITAL LETTER H BAR
phpUpper '\295' = '\294'
-- ĩ -> Ĩ -- LATIN CAPITAL LETTER I TILDE
phpUpper '\297' = '\296'
-- ī -> Ī -- LATIN CAPITAL LETTER I MACRON
phpUpper '\299' = '\298'
-- ĭ -> Ĭ -- LATIN CAPITAL LETTER I BREVE
phpUpper '\301' = '\300'
-- į -> Į -- LATIN CAPITAL LETTER I OGONEK
phpUpper '\303' = '\302'
-- ĳ -> Ĳ -- LATIN CAPITAL LETTER I J
phpUpper '\307' = '\306'
-- ĵ -> Ĵ -- LATIN CAPITAL LETTER J CIRCUMFLEX
phpUpper '\309' = '\308'
-- ķ -> Ķ -- LATIN CAPITAL LETTER K CEDILLA
phpUpper '\311' = '\310'
-- ĺ -> Ĺ -- LATIN CAPITAL LETTER L ACUTE
phpUpper '\314' = '\313'
-- ļ -> Ļ -- LATIN CAPITAL LETTER L CEDILLA
phpUpper '\316' = '\315'
-- ľ -> Ľ -- LATIN CAPITAL LETTER L HACEK
phpUpper '\318' = '\317'
-- ŀ -> Ŀ -- LATIN CAPITAL LETTER L WITH MIDDLE DOT
phpUpper '\320' = '\319'
-- ł -> Ł -- LATIN CAPITAL LETTER L SLASH
phpUpper '\322' = '\321'
-- ń -> Ń -- LATIN CAPITAL LETTER N ACUTE
phpUpper '\324' = '\323'
-- ņ -> Ņ -- LATIN CAPITAL LETTER N CEDILLA
phpUpper '\326' = '\325'
-- ň -> Ň -- LATIN CAPITAL LETTER N HACEK
phpUpper '\328' = '\327'
-- ŋ -> Ŋ -- LATIN CAPITAL LETTER ENG
phpUpper '\331' = '\330'
-- ō -> Ō -- LATIN CAPITAL LETTER O MACRON
phpUpper '\333' = '\332'
-- ŏ -> Ŏ -- LATIN CAPITAL LETTER O BREVE
phpUpper '\335' = '\334'
-- ő -> Ő -- LATIN CAPITAL LETTER O DOUBLE ACUTE
phpUpper '\337' = '\336'
-- œ -> Œ -- LATIN CAPITAL LETTER O E
phpUpper '\339' = '\338'
-- ŕ -> Ŕ -- LATIN CAPITAL LETTER R ACUTE
phpUpper '\341' = '\340'
-- ŗ -> Ŗ -- LATIN CAPITAL LETTER R CEDILLA
phpUpper '\343' = '\342'
-- ř -> Ř -- LATIN CAPITAL LETTER R HACEK
phpUpper '\345' = '\344'
-- ś -> Ś -- LATIN CAPITAL LETTER S ACUTE
phpUpper '\347' = '\346'
-- ŝ -> Ŝ -- LATIN CAPITAL LETTER S CIRCUMFLEX
phpUpper '\349' = '\348'
-- ş -> Ş -- LATIN CAPITAL LETTER S CEDILLA
phpUpper '\351' = '\350'
-- š -> Š -- LATIN CAPITAL LETTER S HACEK
phpUpper '\353' = '\352'
-- ţ -> Ţ -- LATIN CAPITAL LETTER T CEDILLA
phpUpper '\355' = '\354'
-- ť -> Ť -- LATIN CAPITAL LETTER T HACEK
phpUpper '\357' = '\356'
-- ŧ -> Ŧ -- LATIN CAPITAL LETTER T BAR
phpUpper '\359' = '\358'
-- ũ -> Ũ -- LATIN CAPITAL LETTER U TILDE
phpUpper '\361' = '\360'
-- ū -> Ū -- LATIN CAPITAL LETTER U MACRON
phpUpper '\363' = '\362'
-- ŭ -> Ŭ -- LATIN CAPITAL LETTER U BREVE
phpUpper '\365' = '\364'
-- ů -> Ů -- LATIN CAPITAL LETTER U RING
phpUpper '\367' = '\366'
-- ű -> Ű -- LATIN CAPITAL LETTER U DOUBLE ACUTE
phpUpper '\369' = '\368'
-- ų -> Ų -- LATIN CAPITAL LETTER U OGONEK
phpUpper '\371' = '\370'
-- ŵ -> Ŵ -- LATIN CAPITAL LETTER W CIRCUMFLEX
phpUpper '\373' = '\372'
-- ŷ -> Ŷ -- LATIN CAPITAL LETTER Y CIRCUMFLEX
phpUpper '\375' = '\374'
-- ź -> Ź -- LATIN CAPITAL LETTER Z ACUTE
phpUpper '\378' = '\377'
-- ż -> Ż -- LATIN CAPITAL LETTER Z DOT
phpUpper '\380' = '\379'
-- ž -> Ž -- LATIN CAPITAL LETTER Z HACEK
phpUpper '\382' = '\381'
-- ƀ -> Ƀ -- LATIN SMALL LETTER B BAR
phpUpper '\384' = '\579'
-- ɓ -> Ɓ -- LATIN CAPITAL LETTER B HOOK
phpUpper '\595' = '\385'
-- ƃ -> Ƃ -- LATIN CAPITAL LETTER B TOPBAR
phpUpper '\387' = '\386'
-- ƅ -> Ƅ -- LATIN CAPITAL LETTER TONE SIX
phpUpper '\389' = '\388'
-- ɔ -> Ɔ -- LATIN CAPITAL LETTER OPEN O
phpUpper '\596' = '\390'
-- ƈ -> Ƈ -- LATIN CAPITAL LETTER C HOOK
phpUpper '\392' = '\391'
-- ɖ -> Ɖ -- LATIN CAPITAL LETTER AFRICAN D
phpUpper '\598' = '\393'
-- ɗ -> Ɗ -- LATIN CAPITAL LETTER D HOOK
phpUpper '\599' = '\394'
-- ƌ -> Ƌ -- LATIN CAPITAL LETTER D TOPBAR
phpUpper '\396' = '\395'
-- ǝ -> Ǝ -- LATIN CAPITAL LETTER TURNED E
phpUpper '\477' = '\398'
-- ə -> Ə -- LATIN CAPITAL LETTER SCHWA
phpUpper '\601' = '\399'
-- ɛ -> Ɛ -- LATIN CAPITAL LETTER EPSILON
phpUpper '\603' = '\400'
-- ƒ -> Ƒ -- LATIN CAPITAL LETTER F HOOK
phpUpper '\402' = '\401'
-- ɠ -> Ɠ -- LATIN CAPITAL LETTER G HOOK
phpUpper '\608' = '\403'
-- ɣ -> Ɣ -- LATIN CAPITAL LETTER GAMMA
phpUpper '\611' = '\404'
-- ƕ -> Ƕ -- LATIN SMALL LETTER H V
phpUpper '\405' = '\502'
-- ɩ -> Ɩ -- LATIN CAPITAL LETTER IOTA
phpUpper '\617' = '\406'
-- ɨ -> Ɨ -- LATIN CAPITAL LETTER BARRED I
phpUpper '\616' = '\407'
-- ƙ -> Ƙ -- LATIN CAPITAL LETTER K HOOK
phpUpper '\409' = '\408'
-- ƚ -> Ƚ -- LATIN SMALL LETTER BARRED L
phpUpper '\410' = '\573'
-- ɯ -> Ɯ -- LATIN CAPITAL LETTER TURNED M
phpUpper '\623' = '\412'
-- ɲ -> Ɲ -- LATIN CAPITAL LETTER N HOOK
phpUpper '\626' = '\413'
-- ƞ -> Ƞ -- LATIN SMALL LETTER N WITH LONG RIGHT LEG
phpUpper '\414' = '\544'
-- ɵ -> Ɵ -- LATIN CAPITAL LETTER BARRED O
phpUpper '\629' = '\415'
-- ơ -> Ơ -- LATIN CAPITAL LETTER O HORN
phpUpper '\417' = '\416'
-- ƣ -> Ƣ -- LATIN CAPITAL LETTER O I
phpUpper '\419' = '\418'
-- ƥ -> Ƥ -- LATIN CAPITAL LETTER P HOOK
phpUpper '\421' = '\420'
-- ʀ -> Ʀ -- LATIN LETTER Y R
phpUpper '\640' = '\422'
-- ƨ -> Ƨ -- LATIN CAPITAL LETTER TONE TWO
phpUpper '\424' = '\423'
-- ʃ -> Ʃ -- LATIN CAPITAL LETTER ESH
phpUpper '\643' = '\425'
-- ƭ -> Ƭ -- LATIN CAPITAL LETTER T HOOK
phpUpper '\429' = '\428'
-- ʈ -> Ʈ -- LATIN CAPITAL LETTER T RETROFLEX HOOK
phpUpper '\648' = '\430'
-- ư -> Ư -- LATIN CAPITAL LETTER U HORN
phpUpper '\432' = '\431'
-- ʊ -> Ʊ -- LATIN CAPITAL LETTER UPSILON
phpUpper '\650' = '\433'
-- ʋ -> Ʋ -- LATIN CAPITAL LETTER SCRIPT V
phpUpper '\651' = '\434'
-- ƴ -> Ƴ -- LATIN CAPITAL LETTER Y HOOK
phpUpper '\436' = '\435'
-- ƶ -> Ƶ -- LATIN CAPITAL LETTER Z BAR
phpUpper '\438' = '\437'
-- ʒ -> Ʒ -- LATIN CAPITAL LETTER YOGH
phpUpper '\658' = '\439'
-- ƹ -> Ƹ -- LATIN CAPITAL LETTER REVERSED YOGH
phpUpper '\441' = '\440'
-- ƽ -> Ƽ -- LATIN CAPITAL LETTER TONE FIVE
phpUpper '\445' = '\444'
-- ƿ -> Ƿ -- LATIN LETTER WYNN
phpUpper '\447' = '\503'
-- ǆ -> Ǆ -- LATIN CAPITAL LETTER D Z HACEK
phpUpper '\454' = '\452'
-- ǉ -> Ǉ -- LATIN CAPITAL LETTER L J
phpUpper '\457' = '\455'
-- ǌ -> Ǌ -- LATIN CAPITAL LETTER N J
phpUpper '\460' = '\458'
-- ǎ -> Ǎ -- LATIN CAPITAL LETTER A HACEK
phpUpper '\462' = '\461'
-- ǐ -> Ǐ -- LATIN CAPITAL LETTER I HACEK
phpUpper '\464' = '\463'
-- ǒ -> Ǒ -- LATIN CAPITAL LETTER O HACEK
phpUpper '\466' = '\465'
-- ǔ -> Ǔ -- LATIN CAPITAL LETTER U HACEK
phpUpper '\468' = '\467'
-- ǖ -> Ǖ -- LATIN CAPITAL LETTER U DIAERESIS MACRON
phpUpper '\470' = '\469'
-- ǘ -> Ǘ -- LATIN CAPITAL LETTER U DIAERESIS ACUTE
phpUpper '\472' = '\471'
-- ǚ -> Ǚ -- LATIN CAPITAL LETTER U DIAERESIS HACEK
phpUpper '\474' = '\473'
-- ǜ -> Ǜ -- LATIN CAPITAL LETTER U DIAERESIS GRAVE
phpUpper '\476' = '\475'
-- ǟ -> Ǟ -- LATIN CAPITAL LETTER A DIAERESIS MACRON
phpUpper '\479' = '\478'
-- ǡ -> Ǡ -- LATIN CAPITAL LETTER A DOT MACRON
phpUpper '\481' = '\480'
-- ǣ -> Ǣ -- LATIN CAPITAL LETTER A E MACRON
phpUpper '\483' = '\482'
-- ǥ -> Ǥ -- LATIN CAPITAL LETTER G BAR
phpUpper '\485' = '\484'
-- ǧ -> Ǧ -- LATIN CAPITAL LETTER G HACEK
phpUpper '\487' = '\486'
-- ǩ -> Ǩ -- LATIN CAPITAL LETTER K HACEK
phpUpper '\489' = '\488'
-- ǫ -> Ǫ -- LATIN CAPITAL LETTER O OGONEK
phpUpper '\491' = '\490'
-- ǭ -> Ǭ -- LATIN CAPITAL LETTER O OGONEK MACRON
phpUpper '\493' = '\492'
-- ǯ -> Ǯ -- LATIN CAPITAL LETTER YOGH HACEK
phpUpper '\495' = '\494'
-- ǳ -> Ǳ -- LATIN CAPITAL LETTER DZ
phpUpper '\499' = '\497'
-- ǵ -> Ǵ -- LATIN CAPITAL LETTER G WITH ACUTE
phpUpper '\501' = '\500'
-- ǹ -> Ǹ -- LATIN CAPITAL LETTER N WITH GRAVE
phpUpper '\505' = '\504'
-- ǻ -> Ǻ -- LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
phpUpper '\507' = '\506'
-- ǽ -> Ǽ -- LATIN CAPITAL LETTER AE WITH ACUTE
phpUpper '\509' = '\508'
-- ǿ -> Ǿ -- LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
phpUpper '\511' = '\510'
-- ȁ -> Ȁ -- LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
phpUpper '\513' = '\512'
-- ȃ -> Ȃ -- LATIN CAPITAL LETTER A WITH INVERTED BREVE
phpUpper '\515' = '\514'
-- ȅ -> Ȅ -- LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
phpUpper '\517' = '\516'
-- ȇ -> Ȇ -- LATIN CAPITAL LETTER E WITH INVERTED BREVE
phpUpper '\519' = '\518'
-- ȉ -> Ȉ -- LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
phpUpper '\521' = '\520'
-- ȋ -> Ȋ -- LATIN CAPITAL LETTER I WITH INVERTED BREVE
phpUpper '\523' = '\522'
-- ȍ -> Ȍ -- LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
phpUpper '\525' = '\524'
-- ȏ -> Ȏ -- LATIN CAPITAL LETTER O WITH INVERTED BREVE
phpUpper '\527' = '\526'
-- ȑ -> Ȑ -- LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
phpUpper '\529' = '\528'
-- ȓ -> Ȓ -- LATIN CAPITAL LETTER R WITH INVERTED BREVE
phpUpper '\531' = '\530'
-- ȕ -> Ȕ -- LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
phpUpper '\533' = '\532'
-- ȗ -> Ȗ -- LATIN CAPITAL LETTER U WITH INVERTED BREVE
phpUpper '\535' = '\534'
-- ș -> Ș -- LATIN CAPITAL LETTER S WITH COMMA BELOW
phpUpper '\537' = '\536'
-- ț -> Ț -- LATIN CAPITAL LETTER T WITH COMMA BELOW
phpUpper '\539' = '\538'
-- ȝ -> Ȝ -- LATIN CAPITAL LETTER YOGH
phpUpper '\541' = '\540'
-- ȟ -> Ȟ -- LATIN CAPITAL LETTER H WITH CARON
phpUpper '\543' = '\542'
-- ȣ -> Ȣ -- LATIN CAPITAL LETTER OU
phpUpper '\547' = '\546'
-- ȥ -> Ȥ -- LATIN CAPITAL LETTER Z WITH HOOK
phpUpper '\549' = '\548'
-- ȧ -> Ȧ -- LATIN CAPITAL LETTER A WITH DOT ABOVE
phpUpper '\551' = '\550'
-- ȩ -> Ȩ -- LATIN CAPITAL LETTER E WITH CEDILLA
phpUpper '\553' = '\552'
-- ȫ -> Ȫ -- LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
phpUpper '\555' = '\554'
-- ȭ -> Ȭ -- LATIN CAPITAL LETTER O WITH TILDE AND MACRON
phpUpper '\557' = '\556'
-- ȯ -> Ȯ -- LATIN CAPITAL LETTER O WITH DOT ABOVE
phpUpper '\559' = '\558'
-- ȱ -> Ȱ -- LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
phpUpper '\561' = '\560'
-- ȳ -> Ȳ -- LATIN CAPITAL LETTER Y WITH MACRON
phpUpper '\563' = '\562'
-- ⱥ -> Ⱥ -- LATIN CAPITAL LETTER A WITH STROKE
phpUpper '\11365' = '\570'
-- ȼ -> Ȼ -- LATIN CAPITAL LETTER C WITH STROKE
phpUpper '\572' = '\571'
-- ⱦ -> Ⱦ -- LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
phpUpper '\11366' = '\574'
-- ȿ -> Ȿ -- LATIN SMALL LETTER S WITH SWASH TAIL
phpUpper '\575' = '\11390'
-- ɀ -> Ɀ -- LATIN SMALL LETTER Z WITH SWASH TAIL
phpUpper '\576' = '\11391'
-- ɂ -> Ɂ -- LATIN CAPITAL LETTER GLOTTAL STOP
phpUpper '\578' = '\577'
-- ʉ -> Ʉ -- LATIN CAPITAL LETTER U BAR
phpUpper '\649' = '\580'
-- ʌ -> Ʌ -- LATIN CAPITAL LETTER TURNED V
phpUpper '\652' = '\581'
-- ɇ -> Ɇ -- LATIN CAPITAL LETTER E WITH STROKE
phpUpper '\583' = '\582'
-- ɉ -> Ɉ -- LATIN CAPITAL LETTER J WITH STROKE
phpUpper '\585' = '\584'
-- ɋ -> Ɋ -- LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
phpUpper '\587' = '\586'
-- ɍ -> Ɍ -- LATIN CAPITAL LETTER R WITH STROKE
phpUpper '\589' = '\588'
-- ɏ -> Ɏ -- LATIN CAPITAL LETTER Y WITH STROKE
phpUpper '\591' = '\590'
-- ɐ -> Ɐ -- LATIN SMALL LETTER TURNED A
phpUpper '\592' = '\11375'
-- ɑ -> Ɑ -- LATIN SMALL LETTER SCRIPT A
phpUpper '\593' = '\11373'
-- ɒ -> Ɒ -- LATIN SMALL LETTER TURNED SCRIPT A
phpUpper '\594' = '\11376'
-- ɥ -> Ɥ -- LATIN SMALL LETTER TURNED H
phpUpper '\613' = '\42893'
-- ɦ -> Ɦ -- LATIN SMALL LETTER H HOOK
phpUpper '\614' = '\42922'
-- ɫ -> Ɫ -- LATIN SMALL LETTER L WITH MIDDLE TILDE
phpUpper '\619' = '\11362'
-- ɱ -> Ɱ -- LATIN SMALL LETTER M HOOK
phpUpper '\625' = '\11374'
-- ɽ -> Ɽ -- LATIN SMALL LETTER R HOOK
phpUpper '\637' = '\11364'
-- ͱ -> Ͱ -- GREEK CAPITAL LETTER HETA
phpUpper '\881' = '\880'
-- ͳ -> Ͳ -- GREEK CAPITAL LETTER ARCHAIC SAMPI
phpUpper '\883' = '\882'
-- ͷ -> Ͷ -- GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
phpUpper '\887' = '\886'
-- ͻ -> Ͻ -- GREEK SMALL REVERSED LUNATE SIGMA SYMBOL
phpUpper '\891' = '\1021'
-- ͼ -> Ͼ -- GREEK SMALL DOTTED LUNATE SIGMA SYMBOL
phpUpper '\892' = '\1022'
-- ͽ -> Ͽ -- GREEK SMALL REVERSED DOTTED LUNATE SIGMA SYMBOL
phpUpper '\893' = '\1023'
-- ά -> Ά -- GREEK CAPITAL LETTER ALPHA TONOS
phpUpper '\940' = '\902'
-- έ -> Έ -- GREEK CAPITAL LETTER EPSILON TONOS
phpUpper '\941' = '\904'
-- ή -> Ή -- GREEK CAPITAL LETTER ETA TONOS
phpUpper '\942' = '\905'
-- ί -> Ί -- GREEK CAPITAL LETTER IOTA TONOS
phpUpper '\943' = '\906'
-- ό -> Ό -- GREEK CAPITAL LETTER OMICRON TONOS
phpUpper '\972' = '\908'
-- ύ -> Ύ -- GREEK CAPITAL LETTER UPSILON TONOS
phpUpper '\973' = '\910'
-- ώ -> Ώ -- GREEK CAPITAL LETTER OMEGA TONOS
phpUpper '\974' = '\911'
-- α -> Α -- GREEK CAPITAL LETTER ALPHA
phpUpper '\945' = '\913'
-- β -> Β -- GREEK CAPITAL LETTER BETA
phpUpper '\946' = '\914'
-- γ -> Γ -- GREEK CAPITAL LETTER GAMMA
phpUpper '\947' = '\915'
-- δ -> Δ -- GREEK CAPITAL LETTER DELTA
phpUpper '\948' = '\916'
-- ε -> Ε -- GREEK CAPITAL LETTER EPSILON
phpUpper '\949' = '\917'
-- ζ -> Ζ -- GREEK CAPITAL LETTER ZETA
phpUpper '\950' = '\918'
-- η -> Η -- GREEK CAPITAL LETTER ETA
phpUpper '\951' = '\919'
-- θ -> Θ -- GREEK CAPITAL LETTER THETA
phpUpper '\952' = '\920'
-- κ -> Κ -- GREEK CAPITAL LETTER KAPPA
phpUpper '\954' = '\922'
-- λ -> Λ -- GREEK CAPITAL LETTER LAMBDA
phpUpper '\955' = '\923'
-- ν -> Ν -- GREEK CAPITAL LETTER NU
phpUpper '\957' = '\925'
-- ξ -> Ξ -- GREEK CAPITAL LETTER XI
phpUpper '\958' = '\926'
-- ο -> Ο -- GREEK CAPITAL LETTER OMICRON
phpUpper '\959' = '\927'
-- π -> Π -- GREEK CAPITAL LETTER PI
phpUpper '\960' = '\928'
-- ρ -> Ρ -- GREEK CAPITAL LETTER RHO
phpUpper '\961' = '\929'
-- σ -> Σ -- GREEK CAPITAL LETTER SIGMA
phpUpper '\963' = '\931'
-- τ -> Τ -- GREEK CAPITAL LETTER TAU
phpUpper '\964' = '\932'
-- υ -> Υ -- GREEK CAPITAL LETTER UPSILON
phpUpper '\965' = '\933'
-- φ -> Φ -- GREEK CAPITAL LETTER PHI
phpUpper '\966' = '\934'
-- χ -> Χ -- GREEK CAPITAL LETTER CHI
phpUpper '\967' = '\935'
-- ψ -> Ψ -- GREEK CAPITAL LETTER PSI
phpUpper '\968' = '\936'
-- ω -> Ω -- GREEK CAPITAL LETTER OMEGA
phpUpper '\969' = '\937'
-- ϊ -> Ϊ -- GREEK CAPITAL LETTER IOTA DIAERESIS
phpUpper '\970' = '\938'
-- ϋ -> Ϋ -- GREEK CAPITAL LETTER UPSILON DIAERESIS
phpUpper '\971' = '\939'
-- ϗ -> Ϗ -- GREEK CAPITAL KAI SYMBOL
phpUpper '\983' = '\975'
-- ϙ -> Ϙ -- GREEK LETTER ARCHAIC KOPPA
phpUpper '\985' = '\984'
-- ϛ -> Ϛ -- GREEK CAPITAL LETTER STIGMA
phpUpper '\987' = '\986'
-- ϝ -> Ϝ -- GREEK CAPITAL LETTER DIGAMMA
phpUpper '\989' = '\988'
-- ϟ -> Ϟ -- GREEK CAPITAL LETTER KOPPA
phpUpper '\991' = '\990'
-- ϡ -> Ϡ -- GREEK CAPITAL LETTER SAMPI
phpUpper '\993' = '\992'
-- ϣ -> Ϣ -- GREEK CAPITAL LETTER SHEI
phpUpper '\995' = '\994'
-- ϥ -> Ϥ -- GREEK CAPITAL LETTER FEI
phpUpper '\997' = '\996'
-- ϧ -> Ϧ -- GREEK CAPITAL LETTER KHEI
phpUpper '\999' = '\998'
-- ϩ -> Ϩ -- GREEK CAPITAL LETTER HORI
phpUpper '\1001' = '\1000'
-- ϫ -> Ϫ -- GREEK CAPITAL LETTER GANGIA
phpUpper '\1003' = '\1002'
-- ϭ -> Ϭ -- GREEK CAPITAL LETTER SHIMA
phpUpper '\1005' = '\1004'
-- ϯ -> Ϯ -- GREEK CAPITAL LETTER DEI
phpUpper '\1007' = '\1006'
-- ϲ -> Ϲ -- GREEK SMALL LETTER LUNATE SIGMA
phpUpper '\1010' = '\1017'
-- ϸ -> Ϸ -- GREEK CAPITAL LETTER SHO
phpUpper '\1016' = '\1015'
-- ϻ -> Ϻ -- GREEK CAPITAL LETTER SAN
phpUpper '\1019' = '\1018'
-- ѐ -> Ѐ -- CYRILLIC CAPITAL LETTER IE WITH GRAVE
phpUpper '\1104' = '\1024'
-- ё -> Ё -- CYRILLIC CAPITAL LETTER IO
phpUpper '\1105' = '\1025'
-- ђ -> Ђ -- CYRILLIC CAPITAL LETTER DJE
phpUpper '\1106' = '\1026'
-- ѓ -> Ѓ -- CYRILLIC CAPITAL LETTER GJE
phpUpper '\1107' = '\1027'
-- є -> Є -- CYRILLIC CAPITAL LETTER E
phpUpper '\1108' = '\1028'
-- ѕ -> Ѕ -- CYRILLIC CAPITAL LETTER DZE
phpUpper '\1109' = '\1029'
-- і -> І -- CYRILLIC CAPITAL LETTER I
phpUpper '\1110' = '\1030'
-- ї -> Ї -- CYRILLIC CAPITAL LETTER YI
phpUpper '\1111' = '\1031'
-- ј -> Ј -- CYRILLIC CAPITAL LETTER JE
phpUpper '\1112' = '\1032'
-- љ -> Љ -- CYRILLIC CAPITAL LETTER LJE
phpUpper '\1113' = '\1033'
-- њ -> Њ -- CYRILLIC CAPITAL LETTER NJE
phpUpper '\1114' = '\1034'
-- ћ -> Ћ -- CYRILLIC CAPITAL LETTER TSHE
phpUpper '\1115' = '\1035'
-- ќ -> Ќ -- CYRILLIC CAPITAL LETTER KJE
phpUpper '\1116' = '\1036'
-- ѝ -> Ѝ -- CYRILLIC CAPITAL LETTER I WITH GRAVE
phpUpper '\1117' = '\1037'
-- ў -> Ў -- CYRILLIC CAPITAL LETTER SHORT U
phpUpper '\1118' = '\1038'
-- џ -> Џ -- CYRILLIC CAPITAL LETTER DZHE
phpUpper '\1119' = '\1039'
-- а -> А -- CYRILLIC CAPITAL LETTER A
phpUpper '\1072' = '\1040'
-- б -> Б -- CYRILLIC CAPITAL LETTER BE
phpUpper '\1073' = '\1041'
-- в -> В -- CYRILLIC CAPITAL LETTER VE
phpUpper '\1074' = '\1042'
-- г -> Г -- CYRILLIC CAPITAL LETTER GE
phpUpper '\1075' = '\1043'
-- д -> Д -- CYRILLIC CAPITAL LETTER DE
phpUpper '\1076' = '\1044'
-- е -> Е -- CYRILLIC CAPITAL LETTER IE
phpUpper '\1077' = '\1045'
-- ж -> Ж -- CYRILLIC CAPITAL LETTER ZHE
phpUpper '\1078' = '\1046'
-- з -> З -- CYRILLIC CAPITAL LETTER ZE
phpUpper '\1079' = '\1047'
-- и -> И -- CYRILLIC CAPITAL LETTER II
phpUpper '\1080' = '\1048'
-- й -> Й -- CYRILLIC CAPITAL LETTER SHORT II
phpUpper '\1081' = '\1049'
-- к -> К -- CYRILLIC CAPITAL LETTER KA
phpUpper '\1082' = '\1050'
-- л -> Л -- CYRILLIC CAPITAL LETTER EL
phpUpper '\1083' = '\1051'
-- м -> М -- CYRILLIC CAPITAL LETTER EM
phpUpper '\1084' = '\1052'
-- н -> Н -- CYRILLIC CAPITAL LETTER EN
phpUpper '\1085' = '\1053'
-- о -> О -- CYRILLIC CAPITAL LETTER O
phpUpper '\1086' = '\1054'
-- п -> П -- CYRILLIC CAPITAL LETTER PE
phpUpper '\1087' = '\1055'
-- р -> Р -- CYRILLIC CAPITAL LETTER ER
phpUpper '\1088' = '\1056'
-- с -> С -- CYRILLIC CAPITAL LETTER ES
phpUpper '\1089' = '\1057'
-- т -> Т -- CYRILLIC CAPITAL LETTER TE
phpUpper '\1090' = '\1058'
-- у -> У -- CYRILLIC CAPITAL LETTER U
phpUpper '\1091' = '\1059'
-- ф -> Ф -- CYRILLIC CAPITAL LETTER EF
phpUpper '\1092' = '\1060'
-- х -> Х -- CYRILLIC CAPITAL LETTER KHA
phpUpper '\1093' = '\1061'
-- ц -> Ц -- CYRILLIC CAPITAL LETTER TSE
phpUpper '\1094' = '\1062'
-- ч -> Ч -- CYRILLIC CAPITAL LETTER CHE
phpUpper '\1095' = '\1063'
-- ш -> Ш -- CYRILLIC CAPITAL LETTER SHA
phpUpper '\1096' = '\1064'
-- щ -> Щ -- CYRILLIC CAPITAL LETTER SHCHA
phpUpper '\1097' = '\1065'
-- ъ -> Ъ -- CYRILLIC CAPITAL LETTER HARD SIGN
phpUpper '\1098' = '\1066'
-- ы -> Ы -- CYRILLIC CAPITAL LETTER YERI
phpUpper '\1099' = '\1067'
-- ь -> Ь -- CYRILLIC CAPITAL LETTER SOFT SIGN
phpUpper '\1100' = '\1068'
-- э -> Э -- CYRILLIC CAPITAL LETTER REVERSED E
phpUpper '\1101' = '\1069'
-- ю -> Ю -- CYRILLIC CAPITAL LETTER IU
phpUpper '\1102' = '\1070'
-- я -> Я -- CYRILLIC CAPITAL LETTER IA
phpUpper '\1103' = '\1071'
-- ѡ -> Ѡ -- CYRILLIC CAPITAL LETTER OMEGA
phpUpper '\1121' = '\1120'
-- ѣ -> Ѣ -- CYRILLIC CAPITAL LETTER YAT
phpUpper '\1123' = '\1122'
-- ѥ -> Ѥ -- CYRILLIC CAPITAL LETTER IOTIFIED E
phpUpper '\1125' = '\1124'
-- ѧ -> Ѧ -- CYRILLIC CAPITAL LETTER LITTLE YUS
phpUpper '\1127' = '\1126'
-- ѩ -> Ѩ -- CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
phpUpper '\1129' = '\1128'
-- ѫ -> Ѫ -- CYRILLIC CAPITAL LETTER BIG YUS
phpUpper '\1131' = '\1130'
-- ѭ -> Ѭ -- CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
phpUpper '\1133' = '\1132'
-- ѯ -> Ѯ -- CYRILLIC CAPITAL LETTER KSI
phpUpper '\1135' = '\1134'
-- ѱ -> Ѱ -- CYRILLIC CAPITAL LETTER PSI
phpUpper '\1137' = '\1136'
-- ѳ -> Ѳ -- CYRILLIC CAPITAL LETTER FITA
phpUpper '\1139' = '\1138'
-- ѵ -> Ѵ -- CYRILLIC CAPITAL LETTER IZHITSA
phpUpper '\1141' = '\1140'
-- ѷ -> Ѷ -- CYRILLIC CAPITAL LETTER IZHITSA DOUBLE GRAVE
phpUpper '\1143' = '\1142'
-- ѹ -> Ѹ -- CYRILLIC CAPITAL LETTER UK DIGRAPH
phpUpper '\1145' = '\1144'
-- ѻ -> Ѻ -- CYRILLIC CAPITAL LETTER ROUND OMEGA
phpUpper '\1147' = '\1146'
-- ѽ -> Ѽ -- CYRILLIC CAPITAL LETTER OMEGA TITLO
phpUpper '\1149' = '\1148'
-- ѿ -> Ѿ -- CYRILLIC CAPITAL LETTER OT
phpUpper '\1151' = '\1150'
-- ҁ -> Ҁ -- CYRILLIC CAPITAL LETTER KOPPA
phpUpper '\1153' = '\1152'
-- ҋ -> Ҋ -- CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
phpUpper '\1163' = '\1162'
-- ҍ -> Ҍ -- CYRILLIC CAPITAL LETTER SEMISOFT SIGN
phpUpper '\1165' = '\1164'
-- ҏ -> Ҏ -- CYRILLIC CAPITAL LETTER ER WITH TICK
phpUpper '\1167' = '\1166'
-- ґ -> Ґ -- CYRILLIC CAPITAL LETTER GE WITH UPTURN
phpUpper '\1169' = '\1168'
-- ғ -> Ғ -- CYRILLIC CAPITAL LETTER GE BAR
phpUpper '\1171' = '\1170'
-- ҕ -> Ҕ -- CYRILLIC CAPITAL LETTER GE HOOK
phpUpper '\1173' = '\1172'
-- җ -> Җ -- CYRILLIC CAPITAL LETTER ZHE WITH RIGHT DESCENDER
phpUpper '\1175' = '\1174'
-- ҙ -> Ҙ -- CYRILLIC CAPITAL LETTER ZE CEDILLA
phpUpper '\1177' = '\1176'
-- қ -> Қ -- CYRILLIC CAPITAL LETTER KA WITH RIGHT DESCENDER
phpUpper '\1179' = '\1178'
-- ҝ -> Ҝ -- CYRILLIC CAPITAL LETTER KA VERTICAL BAR
phpUpper '\1181' = '\1180'
-- ҟ -> Ҟ -- CYRILLIC CAPITAL LETTER KA BAR
phpUpper '\1183' = '\1182'
-- ҡ -> Ҡ -- CYRILLIC CAPITAL LETTER REVERSED GE KA
phpUpper '\1185' = '\1184'
-- ң -> Ң -- CYRILLIC CAPITAL LETTER EN WITH RIGHT DESCENDER
phpUpper '\1187' = '\1186'
-- ҥ -> Ҥ -- CYRILLIC CAPITAL LETTER EN GE
phpUpper '\1189' = '\1188'
-- ҧ -> Ҧ -- CYRILLIC CAPITAL LETTER PE HOOK
phpUpper '\1191' = '\1190'
-- ҩ -> Ҩ -- CYRILLIC CAPITAL LETTER O HOOK
phpUpper '\1193' = '\1192'
-- ҫ -> Ҫ -- CYRILLIC CAPITAL LETTER ES CEDILLA
phpUpper '\1195' = '\1194'
-- ҭ -> Ҭ -- CYRILLIC CAPITAL LETTER TE WITH RIGHT DESCENDER
phpUpper '\1197' = '\1196'
-- ү -> Ү -- CYRILLIC CAPITAL LETTER STRAIGHT U
phpUpper '\1199' = '\1198'
-- ұ -> Ұ -- CYRILLIC CAPITAL LETTER STRAIGHT U BAR
phpUpper '\1201' = '\1200'
-- ҳ -> Ҳ -- CYRILLIC CAPITAL LETTER KHA WITH RIGHT DESCENDER
phpUpper '\1203' = '\1202'
-- ҵ -> Ҵ -- CYRILLIC CAPITAL LETTER TE TSE
phpUpper '\1205' = '\1204'
-- ҷ -> Ҷ -- CYRILLIC CAPITAL LETTER CHE WITH RIGHT DESCENDER
phpUpper '\1207' = '\1206'
-- ҹ -> Ҹ -- CYRILLIC CAPITAL LETTER CHE VERTICAL BAR
phpUpper '\1209' = '\1208'
-- һ -> Һ -- CYRILLIC CAPITAL LETTER H
phpUpper '\1211' = '\1210'
-- ҽ -> Ҽ -- CYRILLIC CAPITAL LETTER IE HOOK
phpUpper '\1213' = '\1212'
-- ҿ -> Ҿ -- CYRILLIC CAPITAL LETTER IE HOOK OGONEK
phpUpper '\1215' = '\1214'
-- ӏ -> Ӏ -- CYRILLIC LETTER I
phpUpper '\1231' = '\1216'
-- ӂ -> Ӂ -- CYRILLIC CAPITAL LETTER SHORT ZHE
phpUpper '\1218' = '\1217'
-- ӄ -> Ӄ -- CYRILLIC CAPITAL LETTER KA HOOK
phpUpper '\1220' = '\1219'
-- ӆ -> Ӆ -- CYRILLIC CAPITAL LETTER EL WITH TAIL
phpUpper '\1222' = '\1221'
-- ӈ -> Ӈ -- CYRILLIC CAPITAL LETTER EN HOOK
phpUpper '\1224' = '\1223'
-- ӊ -> Ӊ -- CYRILLIC CAPITAL LETTER EN WITH TAIL
phpUpper '\1226' = '\1225'
-- ӌ -> Ӌ -- CYRILLIC CAPITAL LETTER CHE WITH LEFT DESCENDER
phpUpper '\1228' = '\1227'
-- ӎ -> Ӎ -- CYRILLIC CAPITAL LETTER EM WITH TAIL
phpUpper '\1230' = '\1229'
-- ӑ -> Ӑ -- CYRILLIC CAPITAL LETTER A WITH BREVE
phpUpper '\1233' = '\1232'
-- ӓ -> Ӓ -- CYRILLIC CAPITAL LETTER A WITH DIAERESIS
phpUpper '\1235' = '\1234'
-- ӕ -> Ӕ -- CYRILLIC CAPITAL LIGATURE A IE
phpUpper '\1237' = '\1236'
-- ӗ -> Ӗ -- CYRILLIC CAPITAL LETTER IE WITH BREVE
phpUpper '\1239' = '\1238'
-- ә -> Ә -- CYRILLIC CAPITAL LETTER SCHWA
phpUpper '\1241' = '\1240'
-- ӛ -> Ӛ -- CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
phpUpper '\1243' = '\1242'
-- ӝ -> Ӝ -- CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
phpUpper '\1245' = '\1244'
-- ӟ -> Ӟ -- CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
phpUpper '\1247' = '\1246'
-- ӡ -> Ӡ -- CYRILLIC CAPITAL LETTER ABKHASIAN DZE
phpUpper '\1249' = '\1248'
-- ӣ -> Ӣ -- CYRILLIC CAPITAL LETTER I WITH MACRON
phpUpper '\1251' = '\1250'
-- ӥ -> Ӥ -- CYRILLIC CAPITAL LETTER I WITH DIAERESIS
phpUpper '\1253' = '\1252'
-- ӧ -> Ӧ -- CYRILLIC CAPITAL LETTER O WITH DIAERESIS
phpUpper '\1255' = '\1254'
-- ө -> Ө -- CYRILLIC CAPITAL LETTER BARRED O
phpUpper '\1257' = '\1256'
-- ӫ -> Ӫ -- CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
phpUpper '\1259' = '\1258'
-- ӭ -> Ӭ -- CYRILLIC CAPITAL LETTER E WITH DIAERESIS
phpUpper '\1261' = '\1260'
-- ӯ -> Ӯ -- CYRILLIC CAPITAL LETTER U WITH MACRON
phpUpper '\1263' = '\1262'
-- ӱ -> Ӱ -- CYRILLIC CAPITAL LETTER U WITH DIAERESIS
phpUpper '\1265' = '\1264'
-- ӳ -> Ӳ -- CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
phpUpper '\1267' = '\1266'
-- ӵ -> Ӵ -- CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
phpUpper '\1269' = '\1268'
-- ӷ -> Ӷ -- CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
phpUpper '\1271' = '\1270'
-- ӹ -> Ӹ -- CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
phpUpper '\1273' = '\1272'
-- ӻ -> Ӻ -- CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
phpUpper '\1275' = '\1274'
-- ӽ -> Ӽ -- CYRILLIC CAPITAL LETTER HA WITH HOOK
phpUpper '\1277' = '\1276'
-- ӿ -> Ӿ -- CYRILLIC CAPITAL LETTER HA WITH STROKE
phpUpper '\1279' = '\1278'
-- ԁ -> Ԁ -- CYRILLIC CAPITAL LETTER KOMI DE
phpUpper '\1281' = '\1280'
-- ԃ -> Ԃ -- CYRILLIC CAPITAL LETTER KOMI DJE
phpUpper '\1283' = '\1282'
-- ԅ -> Ԅ -- CYRILLIC CAPITAL LETTER KOMI ZJE
phpUpper '\1285' = '\1284'
-- ԇ -> Ԇ -- CYRILLIC CAPITAL LETTER KOMI DZJE
phpUpper '\1287' = '\1286'
-- ԉ -> Ԉ -- CYRILLIC CAPITAL LETTER KOMI LJE
phpUpper '\1289' = '\1288'
-- ԋ -> Ԋ -- CYRILLIC CAPITAL LETTER KOMI NJE
phpUpper '\1291' = '\1290'
-- ԍ -> Ԍ -- CYRILLIC CAPITAL LETTER KOMI SJE
phpUpper '\1293' = '\1292'
-- ԏ -> Ԏ -- CYRILLIC CAPITAL LETTER KOMI TJE
phpUpper '\1295' = '\1294'
-- ԑ -> Ԑ -- CYRILLIC CAPITAL LETTER REVERSED ZE
phpUpper '\1297' = '\1296'
-- ԓ -> Ԓ -- CYRILLIC CAPITAL LETTER EL WITH HOOK
phpUpper '\1299' = '\1298'
-- ԕ -> Ԕ -- CYRILLIC CAPITAL LETTER LHA
phpUpper '\1301' = '\1300'
-- ԗ -> Ԗ -- CYRILLIC CAPITAL LETTER RHA
phpUpper '\1303' = '\1302'
-- ԙ -> Ԙ -- CYRILLIC CAPITAL LETTER YAE
phpUpper '\1305' = '\1304'
-- ԛ -> Ԛ -- CYRILLIC CAPITAL LETTER QA
phpUpper '\1307' = '\1306'
-- ԝ -> Ԝ -- CYRILLIC CAPITAL LETTER WE
phpUpper '\1309' = '\1308'
-- ԟ -> Ԟ -- CYRILLIC CAPITAL LETTER ALEUT KA
phpUpper '\1311' = '\1310'
-- ԡ -> Ԡ -- CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
phpUpper '\1313' = '\1312'
-- ԣ -> Ԣ -- CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
phpUpper '\1315' = '\1314'
-- ԥ -> Ԥ -- CYRILLIC CAPITAL LETTER PE WITH DESCENDER
phpUpper '\1317' = '\1316'
-- ԧ -> Ԧ -- CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
phpUpper '\1319' = '\1318'
-- ա -> Ա -- ARMENIAN CAPITAL LETTER AYB
phpUpper '\1377' = '\1329'
-- բ -> Բ -- ARMENIAN CAPITAL LETTER BEN
phpUpper '\1378' = '\1330'
-- գ -> Գ -- ARMENIAN CAPITAL LETTER GIM
phpUpper '\1379' = '\1331'
-- դ -> Դ -- ARMENIAN CAPITAL LETTER DA
phpUpper '\1380' = '\1332'
-- ե -> Ե -- ARMENIAN CAPITAL LETTER ECH
phpUpper '\1381' = '\1333'
-- զ -> Զ -- ARMENIAN CAPITAL LETTER ZA
phpUpper '\1382' = '\1334'
-- է -> Է -- ARMENIAN CAPITAL LETTER EH
phpUpper '\1383' = '\1335'
-- ը -> Ը -- ARMENIAN CAPITAL LETTER ET
phpUpper '\1384' = '\1336'
-- թ -> Թ -- ARMENIAN CAPITAL LETTER TO
phpUpper '\1385' = '\1337'
-- ժ -> Ժ -- ARMENIAN CAPITAL LETTER ZHE
phpUpper '\1386' = '\1338'
-- ի -> Ի -- ARMENIAN CAPITAL LETTER INI
phpUpper '\1387' = '\1339'
-- լ -> Լ -- ARMENIAN CAPITAL LETTER LIWN
phpUpper '\1388' = '\1340'
-- խ -> Խ -- ARMENIAN CAPITAL LETTER XEH
phpUpper '\1389' = '\1341'
-- ծ -> Ծ -- ARMENIAN CAPITAL LETTER CA
phpUpper '\1390' = '\1342'
-- կ -> Կ -- ARMENIAN CAPITAL LETTER KEN
phpUpper '\1391' = '\1343'
-- հ -> Հ -- ARMENIAN CAPITAL LETTER HO
phpUpper '\1392' = '\1344'
-- ձ -> Ձ -- ARMENIAN CAPITAL LETTER JA
phpUpper '\1393' = '\1345'
-- ղ -> Ղ -- ARMENIAN CAPITAL LETTER LAD
phpUpper '\1394' = '\1346'
-- ճ -> Ճ -- ARMENIAN CAPITAL LETTER CHEH
phpUpper '\1395' = '\1347'
-- մ -> Մ -- ARMENIAN CAPITAL LETTER MEN
phpUpper '\1396' = '\1348'
-- յ -> Յ -- ARMENIAN CAPITAL LETTER YI
phpUpper '\1397' = '\1349'
-- ն -> Ն -- ARMENIAN CAPITAL LETTER NOW
phpUpper '\1398' = '\1350'
-- շ -> Շ -- ARMENIAN CAPITAL LETTER SHA
phpUpper '\1399' = '\1351'
-- ո -> Ո -- ARMENIAN CAPITAL LETTER VO
phpUpper '\1400' = '\1352'
-- չ -> Չ -- ARMENIAN CAPITAL LETTER CHA
phpUpper '\1401' = '\1353'
-- պ -> Պ -- ARMENIAN CAPITAL LETTER PEH
phpUpper '\1402' = '\1354'
-- ջ -> Ջ -- ARMENIAN CAPITAL LETTER JHEH
phpUpper '\1403' = '\1355'
-- ռ -> Ռ -- ARMENIAN CAPITAL LETTER RA
phpUpper '\1404' = '\1356'
-- ս -> Ս -- ARMENIAN CAPITAL LETTER SEH
phpUpper '\1405' = '\1357'
-- վ -> Վ -- ARMENIAN CAPITAL LETTER VEW
phpUpper '\1406' = '\1358'
-- տ -> Տ -- ARMENIAN CAPITAL LETTER TIWN
phpUpper '\1407' = '\1359'
-- ր -> Ր -- ARMENIAN CAPITAL LETTER REH
phpUpper '\1408' = '\1360'
-- ց -> Ց -- ARMENIAN CAPITAL LETTER CO
phpUpper '\1409' = '\1361'
-- ւ -> Ւ -- ARMENIAN CAPITAL LETTER YIWN
phpUpper '\1410' = '\1362'
-- փ -> Փ -- ARMENIAN CAPITAL LETTER PIWR
phpUpper '\1411' = '\1363'
-- ք -> Ք -- ARMENIAN CAPITAL LETTER KEH
phpUpper '\1412' = '\1364'
-- օ -> Օ -- ARMENIAN CAPITAL LETTER OH
phpUpper '\1413' = '\1365'
-- ֆ -> Ֆ -- ARMENIAN CAPITAL LETTER FEH
phpUpper '\1414' = '\1366'
-- ⴀ -> Ⴀ -- GEORGIAN CAPITAL LETTER AN
phpUpper '\11520' = '\4256'
-- ⴁ -> Ⴁ -- GEORGIAN CAPITAL LETTER BAN
phpUpper '\11521' = '\4257'
-- ⴂ -> Ⴂ -- GEORGIAN CAPITAL LETTER GAN
phpUpper '\11522' = '\4258'
-- ⴃ -> Ⴃ -- GEORGIAN CAPITAL LETTER DON
phpUpper '\11523' = '\4259'
-- ⴄ -> Ⴄ -- GEORGIAN CAPITAL LETTER EN
phpUpper '\11524' = '\4260'
-- ⴅ -> Ⴅ -- GEORGIAN CAPITAL LETTER VIN
phpUpper '\11525' = '\4261'
-- ⴆ -> Ⴆ -- GEORGIAN CAPITAL LETTER ZEN
phpUpper '\11526' = '\4262'
-- ⴇ -> Ⴇ -- GEORGIAN CAPITAL LETTER TAN
phpUpper '\11527' = '\4263'
-- ⴈ -> Ⴈ -- GEORGIAN CAPITAL LETTER IN
phpUpper '\11528' = '\4264'
-- ⴉ -> Ⴉ -- GEORGIAN CAPITAL LETTER KAN
phpUpper '\11529' = '\4265'
-- ⴊ -> Ⴊ -- GEORGIAN CAPITAL LETTER LAS
phpUpper '\11530' = '\4266'
-- ⴋ -> Ⴋ -- GEORGIAN CAPITAL LETTER MAN
phpUpper '\11531' = '\4267'
-- ⴌ -> Ⴌ -- GEORGIAN CAPITAL LETTER NAR
phpUpper '\11532' = '\4268'
-- ⴍ -> Ⴍ -- GEORGIAN CAPITAL LETTER ON
phpUpper '\11533' = '\4269'
-- ⴎ -> Ⴎ -- GEORGIAN CAPITAL LETTER PAR
phpUpper '\11534' = '\4270'
-- ⴏ -> Ⴏ -- GEORGIAN CAPITAL LETTER ZHAR
phpUpper '\11535' = '\4271'
-- ⴐ -> Ⴐ -- GEORGIAN CAPITAL LETTER RAE
phpUpper '\11536' = '\4272'
-- ⴑ -> Ⴑ -- GEORGIAN CAPITAL LETTER SAN
phpUpper '\11537' = '\4273'
-- ⴒ -> Ⴒ -- GEORGIAN CAPITAL LETTER TAR
phpUpper '\11538' = '\4274'
-- ⴓ -> Ⴓ -- GEORGIAN CAPITAL LETTER UN
phpUpper '\11539' = '\4275'
-- ⴔ -> Ⴔ -- GEORGIAN CAPITAL LETTER PHAR
phpUpper '\11540' = '\4276'
-- ⴕ -> Ⴕ -- GEORGIAN CAPITAL LETTER KHAR
phpUpper '\11541' = '\4277'
-- ⴖ -> Ⴖ -- GEORGIAN CAPITAL LETTER GHAN
phpUpper '\11542' = '\4278'
-- ⴗ -> Ⴗ -- GEORGIAN CAPITAL LETTER QAR
phpUpper '\11543' = '\4279'
-- ⴘ -> Ⴘ -- GEORGIAN CAPITAL LETTER SHIN
phpUpper '\11544' = '\4280'
-- ⴙ -> Ⴙ -- GEORGIAN CAPITAL LETTER CHIN
phpUpper '\11545' = '\4281'
-- ⴚ -> Ⴚ -- GEORGIAN CAPITAL LETTER CAN
phpUpper '\11546' = '\4282'
-- ⴛ -> Ⴛ -- GEORGIAN CAPITAL LETTER JIL
phpUpper '\11547' = '\4283'
-- ⴜ -> Ⴜ -- GEORGIAN CAPITAL LETTER CIL
phpUpper '\11548' = '\4284'
-- ⴝ -> Ⴝ -- GEORGIAN CAPITAL LETTER CHAR
phpUpper '\11549' = '\4285'
-- ⴞ -> Ⴞ -- GEORGIAN CAPITAL LETTER XAN
phpUpper '\11550' = '\4286'
-- ⴟ -> Ⴟ -- GEORGIAN CAPITAL LETTER JHAN
phpUpper '\11551' = '\4287'
-- ⴠ -> Ⴠ -- GEORGIAN CAPITAL LETTER HAE
phpUpper '\11552' = '\4288'
-- ⴡ -> Ⴡ -- GEORGIAN CAPITAL LETTER HE
phpUpper '\11553' = '\4289'
-- ⴢ -> Ⴢ -- GEORGIAN CAPITAL LETTER HIE
phpUpper '\11554' = '\4290'
-- ⴣ -> Ⴣ -- GEORGIAN CAPITAL LETTER WE
phpUpper '\11555' = '\4291'
-- ⴤ -> Ⴤ -- GEORGIAN CAPITAL LETTER HAR
phpUpper '\11556' = '\4292'
-- ⴥ -> Ⴥ -- GEORGIAN CAPITAL LETTER HOE
phpUpper '\11557' = '\4293'
-- ⴧ -> Ⴧ -- GEORGIAN CAPITAL LETTER YN
phpUpper '\11559' = '\4295'
-- ⴭ -> Ⴭ -- GEORGIAN CAPITAL LETTER AEN
phpUpper '\11565' = '\4301'
-- ᵹ -> Ᵹ -- LATIN SMALL LETTER INSULAR G
phpUpper '\7545' = '\42877'
-- ᵽ -> Ᵽ -- LATIN SMALL LETTER P WITH STROKE
phpUpper '\7549' = '\11363'
-- ḁ -> Ḁ -- LATIN CAPITAL LETTER A WITH RING BELOW
phpUpper '\7681' = '\7680'
-- ḃ -> Ḃ -- LATIN CAPITAL LETTER B WITH DOT ABOVE
phpUpper '\7683' = '\7682'
-- ḅ -> Ḅ -- LATIN CAPITAL LETTER B WITH DOT BELOW
phpUpper '\7685' = '\7684'
-- ḇ -> Ḇ -- LATIN CAPITAL LETTER B WITH LINE BELOW
phpUpper '\7687' = '\7686'
-- ḉ -> Ḉ -- LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
phpUpper '\7689' = '\7688'
-- ḋ -> Ḋ -- LATIN CAPITAL LETTER D WITH DOT ABOVE
phpUpper '\7691' = '\7690'
-- ḍ -> Ḍ -- LATIN CAPITAL LETTER D WITH DOT BELOW
phpUpper '\7693' = '\7692'
-- ḏ -> Ḏ -- LATIN CAPITAL LETTER D WITH LINE BELOW
phpUpper '\7695' = '\7694'
-- ḑ -> Ḑ -- LATIN CAPITAL LETTER D WITH CEDILLA
phpUpper '\7697' = '\7696'
-- ḓ -> Ḓ -- LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
phpUpper '\7699' = '\7698'
-- ḕ -> Ḕ -- LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
phpUpper '\7701' = '\7700'
-- ḗ -> Ḗ -- LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
phpUpper '\7703' = '\7702'
-- ḙ -> Ḙ -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
phpUpper '\7705' = '\7704'
-- ḛ -> Ḛ -- LATIN CAPITAL LETTER E WITH TILDE BELOW
phpUpper '\7707' = '\7706'
-- ḝ -> Ḝ -- LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
phpUpper '\7709' = '\7708'
-- ḟ -> Ḟ -- LATIN CAPITAL LETTER F WITH DOT ABOVE
phpUpper '\7711' = '\7710'
-- ḡ -> Ḡ -- LATIN CAPITAL LETTER G WITH MACRON
phpUpper '\7713' = '\7712'
-- ḣ -> Ḣ -- LATIN CAPITAL LETTER H WITH DOT ABOVE
phpUpper '\7715' = '\7714'
-- ḥ -> Ḥ -- LATIN CAPITAL LETTER H WITH DOT BELOW
phpUpper '\7717' = '\7716'
-- ḧ -> Ḧ -- LATIN CAPITAL LETTER H WITH DIAERESIS
phpUpper '\7719' = '\7718'
-- ḩ -> Ḩ -- LATIN CAPITAL LETTER H WITH CEDILLA
phpUpper '\7721' = '\7720'
-- ḫ -> Ḫ -- LATIN CAPITAL LETTER H WITH BREVE BELOW
phpUpper '\7723' = '\7722'
-- ḭ -> Ḭ -- LATIN CAPITAL LETTER I WITH TILDE BELOW
phpUpper '\7725' = '\7724'
-- ḯ -> Ḯ -- LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
phpUpper '\7727' = '\7726'
-- ḱ -> Ḱ -- LATIN CAPITAL LETTER K WITH ACUTE
phpUpper '\7729' = '\7728'
-- ḳ -> Ḳ -- LATIN CAPITAL LETTER K WITH DOT BELOW
phpUpper '\7731' = '\7730'
-- ḵ -> Ḵ -- LATIN CAPITAL LETTER K WITH LINE BELOW
phpUpper '\7733' = '\7732'
-- ḷ -> Ḷ -- LATIN CAPITAL LETTER L WITH DOT BELOW
phpUpper '\7735' = '\7734'
-- ḹ -> Ḹ -- LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
phpUpper '\7737' = '\7736'
-- ḻ -> Ḻ -- LATIN CAPITAL LETTER L WITH LINE BELOW
phpUpper '\7739' = '\7738'
-- ḽ -> Ḽ -- LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
phpUpper '\7741' = '\7740'
-- ḿ -> Ḿ -- LATIN CAPITAL LETTER M WITH ACUTE
phpUpper '\7743' = '\7742'
-- ṁ -> Ṁ -- LATIN CAPITAL LETTER M WITH DOT ABOVE
phpUpper '\7745' = '\7744'
-- ṃ -> Ṃ -- LATIN CAPITAL LETTER M WITH DOT BELOW
phpUpper '\7747' = '\7746'
-- ṅ -> Ṅ -- LATIN CAPITAL LETTER N WITH DOT ABOVE
phpUpper '\7749' = '\7748'
-- ṇ -> Ṇ -- LATIN CAPITAL LETTER N WITH DOT BELOW
phpUpper '\7751' = '\7750'
-- ṉ -> Ṉ -- LATIN CAPITAL LETTER N WITH LINE BELOW
phpUpper '\7753' = '\7752'
-- ṋ -> Ṋ -- LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
phpUpper '\7755' = '\7754'
-- ṍ -> Ṍ -- LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
phpUpper '\7757' = '\7756'
-- ṏ -> Ṏ -- LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
phpUpper '\7759' = '\7758'
-- ṑ -> Ṑ -- LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
phpUpper '\7761' = '\7760'
-- ṓ -> Ṓ -- LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
phpUpper '\7763' = '\7762'
-- ṕ -> Ṕ -- LATIN CAPITAL LETTER P WITH ACUTE
phpUpper '\7765' = '\7764'
-- ṗ -> Ṗ -- LATIN CAPITAL LETTER P WITH DOT ABOVE
phpUpper '\7767' = '\7766'
-- ṙ -> Ṙ -- LATIN CAPITAL LETTER R WITH DOT ABOVE
phpUpper '\7769' = '\7768'
-- ṛ -> Ṛ -- LATIN CAPITAL LETTER R WITH DOT BELOW
phpUpper '\7771' = '\7770'
-- ṝ -> Ṝ -- LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
phpUpper '\7773' = '\7772'
-- ṟ -> Ṟ -- LATIN CAPITAL LETTER R WITH LINE BELOW
phpUpper '\7775' = '\7774'
-- ṡ -> Ṡ -- LATIN CAPITAL LETTER S WITH DOT ABOVE
phpUpper '\7777' = '\7776'
-- ṣ -> Ṣ -- LATIN CAPITAL LETTER S WITH DOT BELOW
phpUpper '\7779' = '\7778'
-- ṥ -> Ṥ -- LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
phpUpper '\7781' = '\7780'
-- ṧ -> Ṧ -- LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
phpUpper '\7783' = '\7782'
-- ṩ -> Ṩ -- LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
phpUpper '\7785' = '\7784'
-- ṫ -> Ṫ -- LATIN CAPITAL LETTER T WITH DOT ABOVE
phpUpper '\7787' = '\7786'
-- ṭ -> Ṭ -- LATIN CAPITAL LETTER T WITH DOT BELOW
phpUpper '\7789' = '\7788'
-- ṯ -> Ṯ -- LATIN CAPITAL LETTER T WITH LINE BELOW
phpUpper '\7791' = '\7790'
-- ṱ -> Ṱ -- LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
phpUpper '\7793' = '\7792'
-- ṳ -> Ṳ -- LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
phpUpper '\7795' = '\7794'
-- ṵ -> Ṵ -- LATIN CAPITAL LETTER U WITH TILDE BELOW
phpUpper '\7797' = '\7796'
-- ṷ -> Ṷ -- LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
phpUpper '\7799' = '\7798'
-- ṹ -> Ṹ -- LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
phpUpper '\7801' = '\7800'
-- ṻ -> Ṻ -- LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
phpUpper '\7803' = '\7802'
-- ṽ -> Ṽ -- LATIN CAPITAL LETTER V WITH TILDE
phpUpper '\7805' = '\7804'
-- ṿ -> Ṿ -- LATIN CAPITAL LETTER V WITH DOT BELOW
phpUpper '\7807' = '\7806'
-- ẁ -> Ẁ -- LATIN CAPITAL LETTER W WITH GRAVE
phpUpper '\7809' = '\7808'
-- ẃ -> Ẃ -- LATIN CAPITAL LETTER W WITH ACUTE
phpUpper '\7811' = '\7810'
-- ẅ -> Ẅ -- LATIN CAPITAL LETTER W WITH DIAERESIS
phpUpper '\7813' = '\7812'
-- ẇ -> Ẇ -- LATIN CAPITAL LETTER W WITH DOT ABOVE
phpUpper '\7815' = '\7814'
-- ẉ -> Ẉ -- LATIN CAPITAL LETTER W WITH DOT BELOW
phpUpper '\7817' = '\7816'
-- ẋ -> Ẋ -- LATIN CAPITAL LETTER X WITH DOT ABOVE
phpUpper '\7819' = '\7818'
-- ẍ -> Ẍ -- LATIN CAPITAL LETTER X WITH DIAERESIS
phpUpper '\7821' = '\7820'
-- ẏ -> Ẏ -- LATIN CAPITAL LETTER Y WITH DOT ABOVE
phpUpper '\7823' = '\7822'
-- ẑ -> Ẑ -- LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
phpUpper '\7825' = '\7824'
-- ẓ -> Ẓ -- LATIN CAPITAL LETTER Z WITH DOT BELOW
phpUpper '\7827' = '\7826'
-- ẕ -> Ẕ -- LATIN CAPITAL LETTER Z WITH LINE BELOW
phpUpper '\7829' = '\7828'
-- ạ -> Ạ -- LATIN CAPITAL LETTER A WITH DOT BELOW
phpUpper '\7841' = '\7840'
-- ả -> Ả -- LATIN CAPITAL LETTER A WITH HOOK ABOVE
phpUpper '\7843' = '\7842'
-- ấ -> Ấ -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
phpUpper '\7845' = '\7844'
-- ầ -> Ầ -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
phpUpper '\7847' = '\7846'
-- ẩ -> Ẩ -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
phpUpper '\7849' = '\7848'
-- ẫ -> Ẫ -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
phpUpper '\7851' = '\7850'
-- ậ -> Ậ -- LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
phpUpper '\7853' = '\7852'
-- ắ -> Ắ -- LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
phpUpper '\7855' = '\7854'
-- ằ -> Ằ -- LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
phpUpper '\7857' = '\7856'
-- ẳ -> Ẳ -- LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
phpUpper '\7859' = '\7858'
-- ẵ -> Ẵ -- LATIN CAPITAL LETTER A WITH BREVE AND TILDE
phpUpper '\7861' = '\7860'
-- ặ -> Ặ -- LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
phpUpper '\7863' = '\7862'
-- ẹ -> Ẹ -- LATIN CAPITAL LETTER E WITH DOT BELOW
phpUpper '\7865' = '\7864'
-- ẻ -> Ẻ -- LATIN CAPITAL LETTER E WITH HOOK ABOVE
phpUpper '\7867' = '\7866'
-- ẽ -> Ẽ -- LATIN CAPITAL LETTER E WITH TILDE
phpUpper '\7869' = '\7868'
-- ế -> Ế -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
phpUpper '\7871' = '\7870'
-- ề -> Ề -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
phpUpper '\7873' = '\7872'
-- ể -> Ể -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
phpUpper '\7875' = '\7874'
-- ễ -> Ễ -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
phpUpper '\7877' = '\7876'
-- ệ -> Ệ -- LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
phpUpper '\7879' = '\7878'
-- ỉ -> Ỉ -- LATIN CAPITAL LETTER I WITH HOOK ABOVE
phpUpper '\7881' = '\7880'
-- ị -> Ị -- LATIN CAPITAL LETTER I WITH DOT BELOW
phpUpper '\7883' = '\7882'
-- ọ -> Ọ -- LATIN CAPITAL LETTER O WITH DOT BELOW
phpUpper '\7885' = '\7884'
-- ỏ -> Ỏ -- LATIN CAPITAL LETTER O WITH HOOK ABOVE
phpUpper '\7887' = '\7886'
-- ố -> Ố -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
phpUpper '\7889' = '\7888'
-- ồ -> Ồ -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
phpUpper '\7891' = '\7890'
-- ổ -> Ổ -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
phpUpper '\7893' = '\7892'
-- ỗ -> Ỗ -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
phpUpper '\7895' = '\7894'
-- ộ -> Ộ -- LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
phpUpper '\7897' = '\7896'
-- ớ -> Ớ -- LATIN CAPITAL LETTER O WITH HORN AND ACUTE
phpUpper '\7899' = '\7898'
-- ờ -> Ờ -- LATIN CAPITAL LETTER O WITH HORN AND GRAVE
phpUpper '\7901' = '\7900'
-- ở -> Ở -- LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
phpUpper '\7903' = '\7902'
-- ỡ -> Ỡ -- LATIN CAPITAL LETTER O WITH HORN AND TILDE
phpUpper '\7905' = '\7904'
-- ợ -> Ợ -- LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
phpUpper '\7907' = '\7906'
-- ụ -> Ụ -- LATIN CAPITAL LETTER U WITH DOT BELOW
phpUpper '\7909' = '\7908'
-- ủ -> Ủ -- LATIN CAPITAL LETTER U WITH HOOK ABOVE
phpUpper '\7911' = '\7910'
-- ứ -> Ứ -- LATIN CAPITAL LETTER U WITH HORN AND ACUTE
phpUpper '\7913' = '\7912'
-- ừ -> Ừ -- LATIN CAPITAL LETTER U WITH HORN AND GRAVE
phpUpper '\7915' = '\7914'
-- ử -> Ử -- LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
phpUpper '\7917' = '\7916'
-- ữ -> Ữ -- LATIN CAPITAL LETTER U WITH HORN AND TILDE
phpUpper '\7919' = '\7918'
-- ự -> Ự -- LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
phpUpper '\7921' = '\7920'
-- ỳ -> Ỳ -- LATIN CAPITAL LETTER Y WITH GRAVE
phpUpper '\7923' = '\7922'
-- ỵ -> Ỵ -- LATIN CAPITAL LETTER Y WITH DOT BELOW
phpUpper '\7925' = '\7924'
-- ỷ -> Ỷ -- LATIN CAPITAL LETTER Y WITH HOOK ABOVE
phpUpper '\7927' = '\7926'
-- ỹ -> Ỹ -- LATIN CAPITAL LETTER Y WITH TILDE
phpUpper '\7929' = '\7928'
-- ỻ -> Ỻ -- LATIN CAPITAL LETTER MIDDLE-WELSH LL
phpUpper '\7931' = '\7930'
-- ỽ -> Ỽ -- LATIN CAPITAL LETTER MIDDLE-WELSH V
phpUpper '\7933' = '\7932'
-- ỿ -> Ỿ -- LATIN CAPITAL LETTER Y WITH LOOP
phpUpper '\7935' = '\7934'
-- ἀ -> Ἀ -- GREEK SMALL LETTER ALPHA WITH PSILI
phpUpper '\7936' = '\7944'
-- ἁ -> Ἁ -- GREEK SMALL LETTER ALPHA WITH DASIA
phpUpper '\7937' = '\7945'
-- ἂ -> Ἂ -- GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
phpUpper '\7938' = '\7946'
-- ἃ -> Ἃ -- GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
phpUpper '\7939' = '\7947'
-- ἄ -> Ἄ -- GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
phpUpper '\7940' = '\7948'
-- ἅ -> Ἅ -- GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
phpUpper '\7941' = '\7949'
-- ἆ -> Ἆ -- GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
phpUpper '\7942' = '\7950'
-- ἇ -> Ἇ -- GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
phpUpper '\7943' = '\7951'
-- ἐ -> Ἐ -- GREEK SMALL LETTER EPSILON WITH PSILI
phpUpper '\7952' = '\7960'
-- ἑ -> Ἑ -- GREEK SMALL LETTER EPSILON WITH DASIA
phpUpper '\7953' = '\7961'
-- ἒ -> Ἒ -- GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
phpUpper '\7954' = '\7962'
-- ἓ -> Ἓ -- GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
phpUpper '\7955' = '\7963'
-- ἔ -> Ἔ -- GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
phpUpper '\7956' = '\7964'
-- ἕ -> Ἕ -- GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
phpUpper '\7957' = '\7965'
-- ἠ -> Ἠ -- GREEK SMALL LETTER ETA WITH PSILI
phpUpper '\7968' = '\7976'
-- ἡ -> Ἡ -- GREEK SMALL LETTER ETA WITH DASIA
phpUpper '\7969' = '\7977'
-- ἢ -> Ἢ -- GREEK SMALL LETTER ETA WITH PSILI AND VARIA
phpUpper '\7970' = '\7978'
-- ἣ -> Ἣ -- GREEK SMALL LETTER ETA WITH DASIA AND VARIA
phpUpper '\7971' = '\7979'
-- ἤ -> Ἤ -- GREEK SMALL LETTER ETA WITH PSILI AND OXIA
phpUpper '\7972' = '\7980'
-- ἥ -> Ἥ -- GREEK SMALL LETTER ETA WITH DASIA AND OXIA
phpUpper '\7973' = '\7981'
-- ἦ -> Ἦ -- GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
phpUpper '\7974' = '\7982'
-- ἧ -> Ἧ -- GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
phpUpper '\7975' = '\7983'
-- ἰ -> Ἰ -- GREEK SMALL LETTER IOTA WITH PSILI
phpUpper '\7984' = '\7992'
-- ἱ -> Ἱ -- GREEK SMALL LETTER IOTA WITH DASIA
phpUpper '\7985' = '\7993'
-- ἲ -> Ἲ -- GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
phpUpper '\7986' = '\7994'
-- ἳ -> Ἳ -- GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
phpUpper '\7987' = '\7995'
-- ἴ -> Ἴ -- GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
phpUpper '\7988' = '\7996'
-- ἵ -> Ἵ -- GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
phpUpper '\7989' = '\7997'
-- ἶ -> Ἶ -- GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
phpUpper '\7990' = '\7998'
-- ἷ -> Ἷ -- GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
phpUpper '\7991' = '\7999'
-- ὀ -> Ὀ -- GREEK SMALL LETTER OMICRON WITH PSILI
phpUpper '\8000' = '\8008'
-- ὁ -> Ὁ -- GREEK SMALL LETTER OMICRON WITH DASIA
phpUpper '\8001' = '\8009'
-- ὂ -> Ὂ -- GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
phpUpper '\8002' = '\8010'
-- ὃ -> Ὃ -- GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
phpUpper '\8003' = '\8011'
-- ὄ -> Ὄ -- GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
phpUpper '\8004' = '\8012'
-- ὅ -> Ὅ -- GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
phpUpper '\8005' = '\8013'
-- ὑ -> Ὑ -- GREEK SMALL LETTER UPSILON WITH DASIA
phpUpper '\8017' = '\8025'
-- ὓ -> Ὓ -- GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
phpUpper '\8019' = '\8027'
-- ὕ -> Ὕ -- GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
phpUpper '\8021' = '\8029'
-- ὗ -> Ὗ -- GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
phpUpper '\8023' = '\8031'
-- ὠ -> Ὠ -- GREEK SMALL LETTER OMEGA WITH PSILI
phpUpper '\8032' = '\8040'
-- ὡ -> Ὡ -- GREEK SMALL LETTER OMEGA WITH DASIA
phpUpper '\8033' = '\8041'
-- ὢ -> Ὢ -- GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
phpUpper '\8034' = '\8042'
-- ὣ -> Ὣ -- GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
phpUpper '\8035' = '\8043'
-- ὤ -> Ὤ -- GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
phpUpper '\8036' = '\8044'
-- ὥ -> Ὥ -- GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
phpUpper '\8037' = '\8045'
-- ὦ -> Ὦ -- GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
phpUpper '\8038' = '\8046'
-- ὧ -> Ὧ -- GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
phpUpper '\8039' = '\8047'
-- ὰ -> Ὰ -- GREEK SMALL LETTER ALPHA WITH VARIA
phpUpper '\8048' = '\8122'
-- ά -> Ά -- GREEK SMALL LETTER ALPHA WITH OXIA
phpUpper '\8049' = '\8123'
-- ὲ -> Ὲ -- GREEK SMALL LETTER EPSILON WITH VARIA
phpUpper '\8050' = '\8136'
-- έ -> Έ -- GREEK SMALL LETTER EPSILON WITH OXIA
phpUpper '\8051' = '\8137'
-- ὴ -> Ὴ -- GREEK SMALL LETTER ETA WITH VARIA
phpUpper '\8052' = '\8138'
-- ή -> Ή -- GREEK SMALL LETTER ETA WITH OXIA
phpUpper '\8053' = '\8139'
-- ὶ -> Ὶ -- GREEK SMALL LETTER IOTA WITH VARIA
phpUpper '\8054' = '\8154'
-- ί -> Ί -- GREEK SMALL LETTER IOTA WITH OXIA
phpUpper '\8055' = '\8155'
-- ὸ -> Ὸ -- GREEK SMALL LETTER OMICRON WITH VARIA
phpUpper '\8056' = '\8184'
-- ό -> Ό -- GREEK SMALL LETTER OMICRON WITH OXIA
phpUpper '\8057' = '\8185'
-- ὺ -> Ὺ -- GREEK SMALL LETTER UPSILON WITH VARIA
phpUpper '\8058' = '\8170'
-- ύ -> Ύ -- GREEK SMALL LETTER UPSILON WITH OXIA
phpUpper '\8059' = '\8171'
-- ὼ -> Ὼ -- GREEK SMALL LETTER OMEGA WITH VARIA
phpUpper '\8060' = '\8186'
-- ώ -> Ώ -- GREEK SMALL LETTER OMEGA WITH OXIA
phpUpper '\8061' = '\8187'
-- ᾀ -> ᾈ -- GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
phpUpper '\8064' = '\8072'
-- ᾁ -> ᾉ -- GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
phpUpper '\8065' = '\8073'
-- ᾂ -> ᾊ -- GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
phpUpper '\8066' = '\8074'
-- ᾃ -> ᾋ -- GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
phpUpper '\8067' = '\8075'
-- ᾄ -> ᾌ -- GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
phpUpper '\8068' = '\8076'
-- ᾅ -> ᾍ -- GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
phpUpper '\8069' = '\8077'
-- ᾆ -> ᾎ -- GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
phpUpper '\8070' = '\8078'
-- ᾇ -> ᾏ -- GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
phpUpper '\8071' = '\8079'
-- ᾐ -> ᾘ -- GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
phpUpper '\8080' = '\8088'
-- ᾑ -> ᾙ -- GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
phpUpper '\8081' = '\8089'
-- ᾒ -> ᾚ -- GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
phpUpper '\8082' = '\8090'
-- ᾓ -> ᾛ -- GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
phpUpper '\8083' = '\8091'
-- ᾔ -> ᾜ -- GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
phpUpper '\8084' = '\8092'
-- ᾕ -> ᾝ -- GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
phpUpper '\8085' = '\8093'
-- ᾖ -> ᾞ -- GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
phpUpper '\8086' = '\8094'
-- ᾗ -> ᾟ -- GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
phpUpper '\8087' = '\8095'
-- ᾠ -> ᾨ -- GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
phpUpper '\8096' = '\8104'
-- ᾡ -> ᾩ -- GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
phpUpper '\8097' = '\8105'
-- ᾢ -> ᾪ -- GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
phpUpper '\8098' = '\8106'
-- ᾣ -> ᾫ -- GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
phpUpper '\8099' = '\8107'
-- ᾤ -> ᾬ -- GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
phpUpper '\8100' = '\8108'
-- ᾥ -> ᾭ -- GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
phpUpper '\8101' = '\8109'
-- ᾦ -> ᾮ -- GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
phpUpper '\8102' = '\8110'
-- ᾧ -> ᾯ -- GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
phpUpper '\8103' = '\8111'
-- ᾰ -> Ᾰ -- GREEK SMALL LETTER ALPHA WITH VRACHY
phpUpper '\8112' = '\8120'
-- ᾱ -> Ᾱ -- GREEK SMALL LETTER ALPHA WITH MACRON
phpUpper '\8113' = '\8121'
-- ᾳ -> ᾼ -- GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
phpUpper '\8115' = '\8124'
-- ῃ -> ῌ -- GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
phpUpper '\8131' = '\8140'
-- ῐ -> Ῐ -- GREEK SMALL LETTER IOTA WITH VRACHY
phpUpper '\8144' = '\8152'
-- ῑ -> Ῑ -- GREEK SMALL LETTER IOTA WITH MACRON
phpUpper '\8145' = '\8153'
-- ῠ -> Ῠ -- GREEK SMALL LETTER UPSILON WITH VRACHY
phpUpper '\8160' = '\8168'
-- ῡ -> Ῡ -- GREEK SMALL LETTER UPSILON WITH MACRON
phpUpper '\8161' = '\8169'
-- ῥ -> Ῥ -- GREEK SMALL LETTER RHO WITH DASIA
phpUpper '\8165' = '\8172'
-- ῳ -> ῼ -- GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
phpUpper '\8179' = '\8188'
-- ⅎ -> Ⅎ -- TURNED F
-- '\8560': '\8544',
-- -- ⅰ -> Ⅰ -- ROMAN NUMERAL ONE
-- '\8561': '\8545',
-- -- ⅱ -> Ⅱ -- ROMAN NUMERAL TWO
-- '\8562': '\8546',
-- -- ⅲ -> Ⅲ -- ROMAN NUMERAL THREE
-- '\8563': '\8547',
-- -- ⅳ -> Ⅳ -- ROMAN NUMERAL FOUR
-- '\8564': '\8548',
-- -- ⅴ -> Ⅴ -- ROMAN NUMERAL FIVE
-- '\8565': '\8549',
-- -- ⅵ -> Ⅵ -- ROMAN NUMERAL SIX
-- '\8566': '\8550',
-- -- ⅶ -> Ⅶ -- ROMAN NUMERAL SEVEN
-- '\8567': '\8551',
-- -- ⅷ -> Ⅷ -- ROMAN NUMERAL EIGHT
-- '\8568': '\8552',
-- -- ⅸ -> Ⅸ -- ROMAN NUMERAL NINE
-- '\8569': '\8553',
-- -- ⅹ -> Ⅹ -- ROMAN NUMERAL TEN
-- '\8570': '\8554',
-- -- ⅺ -> Ⅺ -- ROMAN NUMERAL ELEVEN
-- '\8571': '\8555',
-- -- ⅻ -> Ⅻ -- ROMAN NUMERAL TWELVE
-- '\8572': '\8556',
-- -- ⅼ -> Ⅼ -- ROMAN NUMERAL FIFTY
-- '\8573': '\8557',
-- -- ⅽ -> Ⅽ -- ROMAN NUMERAL ONE HUNDRED
-- '\8574': '\8558',
-- -- ⅾ -> Ⅾ -- ROMAN NUMERAL FIVE HUNDRED
-- '\8575': '\8559',
-- -- ⅿ -> Ⅿ -- ROMAN NUMERAL ONE THOUSAND
-- '\8580': '\8579',
-- -- ↄ -> Ↄ -- ROMAN NUMERAL REVERSED ONE HUNDRED
-- phpUpper '\8526' = '\8498'
-- ⓐ -> Ⓐ -- CIRCLED LATIN CAPITAL LETTER A
-- phpUpper '\9424' = '\9398'
-- ⓑ -> Ⓑ -- CIRCLED LATIN CAPITAL LETTER B
-- phpUpper '\9425' = '\9399'
-- ⓒ -> Ⓒ -- CIRCLED LATIN CAPITAL LETTER C
-- phpUpper '\9426' = '\9400'
-- ⓓ -> Ⓓ -- CIRCLED LATIN CAPITAL LETTER D
-- phpUpper '\9427' = '\9401'
-- ⓔ -> Ⓔ -- CIRCLED LATIN CAPITAL LETTER E
-- phpUpper '\9428' = '\9402'
-- ⓕ -> Ⓕ -- CIRCLED LATIN CAPITAL LETTER F
-- phpUpper '\9429' = '\9403'
-- ⓖ -> Ⓖ -- CIRCLED LATIN CAPITAL LETTER G
-- phpUpper '\9430' = '\9404'
-- ⓗ -> Ⓗ -- CIRCLED LATIN CAPITAL LETTER H
-- phpUpper '\9431' = '\9405'
-- ⓘ -> Ⓘ -- CIRCLED LATIN CAPITAL LETTER I
-- phpUpper '\9432' = '\9406'
-- ⓙ -> Ⓙ -- CIRCLED LATIN CAPITAL LETTER J
-- phpUpper '\9433' = '\9407'
-- ⓚ -> Ⓚ -- CIRCLED LATIN CAPITAL LETTER K
-- phpUpper '\9434' = '\9408'
-- ⓛ -> Ⓛ -- CIRCLED LATIN CAPITAL LETTER L
-- phpUpper '\9435' = '\9409'
-- ⓜ -> Ⓜ -- CIRCLED LATIN CAPITAL LETTER M
-- phpUpper '\9436' = '\9410'
-- ⓝ -> Ⓝ -- CIRCLED LATIN CAPITAL LETTER N
-- phpUpper '\9437' = '\9411'
-- ⓞ -> Ⓞ -- CIRCLED LATIN CAPITAL LETTER O
-- phpUpper '\9438' = '\9412'
-- ⓟ -> Ⓟ -- CIRCLED LATIN CAPITAL LETTER P
-- phpUpper '\9439' = '\9413'
-- ⓠ -> Ⓠ -- CIRCLED LATIN CAPITAL LETTER Q
-- phpUpper '\9440' = '\9414'
-- ⓡ -> Ⓡ -- CIRCLED LATIN CAPITAL LETTER R
-- phpUpper '\9441' = '\9415'
-- ⓢ -> Ⓢ -- CIRCLED LATIN CAPITAL LETTER S
-- phpUpper '\9442' = '\9416'
-- ⓣ -> Ⓣ -- CIRCLED LATIN CAPITAL LETTER T
-- phpUpper '\9443' = '\9417'
-- ⓤ -> Ⓤ -- CIRCLED LATIN CAPITAL LETTER U
-- phpUpper '\9444' = '\9418'
-- ⓥ -> Ⓥ -- CIRCLED LATIN CAPITAL LETTER V
-- phpUpper '\9445' = '\9419'
-- ⓦ -> Ⓦ -- CIRCLED LATIN CAPITAL LETTER W
-- phpUpper '\9446' = '\9420'
-- ⓧ -> Ⓧ -- CIRCLED LATIN CAPITAL LETTER X
-- phpUpper '\9447' = '\9421'
-- ⓨ -> Ⓨ -- CIRCLED LATIN CAPITAL LETTER Y
-- phpUpper '\9448' = '\9422'
-- ⓩ -> Ⓩ -- CIRCLED LATIN CAPITAL LETTER Z
-- phpUpper '\9449' = '\9423'
-- ⰰ -> Ⰰ -- GLAGOLITIC CAPITAL LETTER AZU
phpUpper '\11312' = '\11264'
-- ⰱ -> Ⰱ -- GLAGOLITIC CAPITAL LETTER BUKY
phpUpper '\11313' = '\11265'
-- ⰲ -> Ⰲ -- GLAGOLITIC CAPITAL LETTER VEDE
phpUpper '\11314' = '\11266'
-- ⰳ -> Ⰳ -- GLAGOLITIC CAPITAL LETTER GLAGOLI
phpUpper '\11315' = '\11267'
-- ⰴ -> Ⰴ -- GLAGOLITIC CAPITAL LETTER DOBRO
phpUpper '\11316' = '\11268'
-- ⰵ -> Ⰵ -- GLAGOLITIC CAPITAL LETTER YESTU
phpUpper '\11317' = '\11269'
-- ⰶ -> Ⰶ -- GLAGOLITIC CAPITAL LETTER ZHIVETE
phpUpper '\11318' = '\11270'
-- ⰷ -> Ⰷ -- GLAGOLITIC CAPITAL LETTER DZELO
phpUpper '\11319' = '\11271'
-- ⰸ -> Ⰸ -- GLAGOLITIC CAPITAL LETTER ZEMLJA
phpUpper '\11320' = '\11272'
-- ⰹ -> Ⰹ -- GLAGOLITIC CAPITAL LETTER IZHE
phpUpper '\11321' = '\11273'
-- ⰺ -> Ⰺ -- GLAGOLITIC CAPITAL LETTER INITIAL IZHE
phpUpper '\11322' = '\11274'
-- ⰻ -> Ⰻ -- GLAGOLITIC CAPITAL LETTER I
phpUpper '\11323' = '\11275'
-- ⰼ -> Ⰼ -- GLAGOLITIC CAPITAL LETTER DJERVI
phpUpper '\11324' = '\11276'
-- ⰽ -> Ⰽ -- GLAGOLITIC CAPITAL LETTER KAKO
phpUpper '\11325' = '\11277'
-- ⰾ -> Ⰾ -- GLAGOLITIC CAPITAL LETTER LJUDIJE
phpUpper '\11326' = '\11278'
-- ⰿ -> Ⰿ -- GLAGOLITIC CAPITAL LETTER MYSLITE
phpUpper '\11327' = '\11279'
-- ⱀ -> Ⱀ -- GLAGOLITIC CAPITAL LETTER NASHI
phpUpper '\11328' = '\11280'
-- ⱁ -> Ⱁ -- GLAGOLITIC CAPITAL LETTER ONU
phpUpper '\11329' = '\11281'
-- ⱂ -> Ⱂ -- GLAGOLITIC CAPITAL LETTER POKOJI
phpUpper '\11330' = '\11282'
-- ⱃ -> Ⱃ -- GLAGOLITIC CAPITAL LETTER RITSI
phpUpper '\11331' = '\11283'
-- ⱄ -> Ⱄ -- GLAGOLITIC CAPITAL LETTER SLOVO
phpUpper '\11332' = '\11284'
-- ⱅ -> Ⱅ -- GLAGOLITIC CAPITAL LETTER TVRIDO
phpUpper '\11333' = '\11285'
-- ⱆ -> Ⱆ -- GLAGOLITIC CAPITAL LETTER UKU
phpUpper '\11334' = '\11286'
-- ⱇ -> Ⱇ -- GLAGOLITIC CAPITAL LETTER FRITU
phpUpper '\11335' = '\11287'
-- ⱈ -> Ⱈ -- GLAGOLITIC CAPITAL LETTER HERU
phpUpper '\11336' = '\11288'
-- ⱉ -> Ⱉ -- GLAGOLITIC CAPITAL LETTER OTU
phpUpper '\11337' = '\11289'
-- ⱊ -> Ⱊ -- GLAGOLITIC CAPITAL LETTER PE
phpUpper '\11338' = '\11290'
-- ⱋ -> Ⱋ -- GLAGOLITIC CAPITAL LETTER SHTA
phpUpper '\11339' = '\11291'
-- ⱌ -> Ⱌ -- GLAGOLITIC CAPITAL LETTER TSI
phpUpper '\11340' = '\11292'
-- ⱍ -> Ⱍ -- GLAGOLITIC CAPITAL LETTER CHRIVI
phpUpper '\11341' = '\11293'
-- ⱎ -> Ⱎ -- GLAGOLITIC CAPITAL LETTER SHA
phpUpper '\11342' = '\11294'
-- ⱏ -> Ⱏ -- GLAGOLITIC CAPITAL LETTER YERU
phpUpper '\11343' = '\11295'
-- ⱐ -> Ⱐ -- GLAGOLITIC CAPITAL LETTER YERI
phpUpper '\11344' = '\11296'
-- ⱑ -> Ⱑ -- GLAGOLITIC CAPITAL LETTER YATI
phpUpper '\11345' = '\11297'
-- ⱒ -> Ⱒ -- GLAGOLITIC CAPITAL LETTER SPIDERY HA
phpUpper '\11346' = '\11298'
-- ⱓ -> Ⱓ -- GLAGOLITIC CAPITAL LETTER YU
phpUpper '\11347' = '\11299'
-- ⱔ -> Ⱔ -- GLAGOLITIC CAPITAL LETTER SMALL YUS
phpUpper '\11348' = '\11300'
-- ⱕ -> Ⱕ -- GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
phpUpper '\11349' = '\11301'
-- ⱖ -> Ⱖ -- GLAGOLITIC CAPITAL LETTER YO
phpUpper '\11350' = '\11302'
-- ⱗ -> Ⱗ -- GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
phpUpper '\11351' = '\11303'
-- ⱘ -> Ⱘ -- GLAGOLITIC CAPITAL LETTER BIG YUS
phpUpper '\11352' = '\11304'
-- ⱙ -> Ⱙ -- GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
phpUpper '\11353' = '\11305'
-- ⱚ -> Ⱚ -- GLAGOLITIC CAPITAL LETTER FITA
phpUpper '\11354' = '\11306'
-- ⱛ -> Ⱛ -- GLAGOLITIC CAPITAL LETTER IZHITSA
phpUpper '\11355' = '\11307'
-- ⱜ -> Ⱜ -- GLAGOLITIC CAPITAL LETTER SHTAPIC
phpUpper '\11356' = '\11308'
-- ⱝ -> Ⱝ -- GLAGOLITIC CAPITAL LETTER TROKUTASTI A
phpUpper '\11357' = '\11309'
-- ⱞ -> Ⱞ -- GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
phpUpper '\11358' = '\11310'
-- ⱡ -> Ⱡ -- LATIN CAPITAL LETTER L WITH DOUBLE BAR
phpUpper '\11361' = '\11360'
-- ⱨ -> Ⱨ -- LATIN CAPITAL LETTER H WITH DESCENDER
phpUpper '\11368' = '\11367'
-- ⱪ -> Ⱪ -- LATIN CAPITAL LETTER K WITH DESCENDER
phpUpper '\11370' = '\11369'
-- ⱬ -> Ⱬ -- LATIN CAPITAL LETTER Z WITH DESCENDER
phpUpper '\11372' = '\11371'
-- ⱳ -> Ⱳ -- LATIN CAPITAL LETTER W WITH HOOK
phpUpper '\11379' = '\11378'
-- ⱶ -> Ⱶ -- LATIN CAPITAL LETTER HALF H
phpUpper '\11382' = '\11381'
-- ⲁ -> Ⲁ -- COPTIC CAPITAL LETTER ALFA
phpUpper '\11393' = '\11392'
-- ⲃ -> Ⲃ -- COPTIC CAPITAL LETTER VIDA
phpUpper '\11395' = '\11394'
-- ⲅ -> Ⲅ -- COPTIC CAPITAL LETTER GAMMA
phpUpper '\11397' = '\11396'
-- ⲇ -> Ⲇ -- COPTIC CAPITAL LETTER DALDA
phpUpper '\11399' = '\11398'
-- ⲉ -> Ⲉ -- COPTIC CAPITAL LETTER EIE
phpUpper '\11401' = '\11400'
-- ⲋ -> Ⲋ -- COPTIC CAPITAL LETTER SOU
phpUpper '\11403' = '\11402'
-- ⲍ -> Ⲍ -- COPTIC CAPITAL LETTER ZATA
phpUpper '\11405' = '\11404'
-- ⲏ -> Ⲏ -- COPTIC CAPITAL LETTER HATE
phpUpper '\11407' = '\11406'
-- ⲑ -> Ⲑ -- COPTIC CAPITAL LETTER THETHE
phpUpper '\11409' = '\11408'
-- ⲓ -> Ⲓ -- COPTIC CAPITAL LETTER IAUDA
phpUpper '\11411' = '\11410'
-- ⲕ -> Ⲕ -- COPTIC CAPITAL LETTER KAPA
phpUpper '\11413' = '\11412'
-- ⲗ -> Ⲗ -- COPTIC CAPITAL LETTER LAULA
phpUpper '\11415' = '\11414'
-- ⲙ -> Ⲙ -- COPTIC CAPITAL LETTER MI
phpUpper '\11417' = '\11416'
-- ⲛ -> Ⲛ -- COPTIC CAPITAL LETTER NI
phpUpper '\11419' = '\11418'
-- ⲝ -> Ⲝ -- COPTIC CAPITAL LETTER KSI
phpUpper '\11421' = '\11420'
-- ⲟ -> Ⲟ -- COPTIC CAPITAL LETTER O
phpUpper '\11423' = '\11422'
-- ⲡ -> Ⲡ -- COPTIC CAPITAL LETTER PI
phpUpper '\11425' = '\11424'
-- ⲣ -> Ⲣ -- COPTIC CAPITAL LETTER RO
phpUpper '\11427' = '\11426'
-- ⲥ -> Ⲥ -- COPTIC CAPITAL LETTER SIMA
phpUpper '\11429' = '\11428'
-- ⲧ -> Ⲧ -- COPTIC CAPITAL LETTER TAU
phpUpper '\11431' = '\11430'
-- ⲩ -> Ⲩ -- COPTIC CAPITAL LETTER UA
phpUpper '\11433' = '\11432'
-- ⲫ -> Ⲫ -- COPTIC CAPITAL LETTER FI
phpUpper '\11435' = '\11434'
-- ⲭ -> Ⲭ -- COPTIC CAPITAL LETTER KHI
phpUpper '\11437' = '\11436'
-- ⲯ -> Ⲯ -- COPTIC CAPITAL LETTER PSI
phpUpper '\11439' = '\11438'
-- ⲱ -> Ⲱ -- COPTIC CAPITAL LETTER OOU
phpUpper '\11441' = '\11440'
-- ⲳ -> Ⲳ -- COPTIC CAPITAL LETTER DIALECT-P ALEF
phpUpper '\11443' = '\11442'
-- ⲵ -> Ⲵ -- COPTIC CAPITAL LETTER OLD COPTIC AIN
phpUpper '\11445' = '\11444'
-- ⲷ -> Ⲷ -- COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
phpUpper '\11447' = '\11446'
-- ⲹ -> Ⲹ -- COPTIC CAPITAL LETTER DIALECT-P KAPA
phpUpper '\11449' = '\11448'
-- ⲻ -> Ⲻ -- COPTIC CAPITAL LETTER DIALECT-P NI
phpUpper '\11451' = '\11450'
-- ⲽ -> Ⲽ -- COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
phpUpper '\11453' = '\11452'
-- ⲿ -> Ⲿ -- COPTIC CAPITAL LETTER OLD COPTIC OOU
phpUpper '\11455' = '\11454'
-- ⳁ -> Ⳁ -- COPTIC CAPITAL LETTER SAMPI
phpUpper '\11457' = '\11456'
-- ⳃ -> Ⳃ -- COPTIC CAPITAL LETTER CROSSED SHEI
phpUpper '\11459' = '\11458'
-- ⳅ -> Ⳅ -- COPTIC CAPITAL LETTER OLD COPTIC SHEI
phpUpper '\11461' = '\11460'
-- ⳇ -> Ⳇ -- COPTIC CAPITAL LETTER OLD COPTIC ESH
phpUpper '\11463' = '\11462'
-- ⳉ -> Ⳉ -- COPTIC CAPITAL LETTER AKHMIMIC KHEI
phpUpper '\11465' = '\11464'
-- ⳋ -> Ⳋ -- COPTIC CAPITAL LETTER DIALECT-P HORI
phpUpper '\11467' = '\11466'
-- ⳍ -> Ⳍ -- COPTIC CAPITAL LETTER OLD COPTIC HORI
phpUpper '\11469' = '\11468'
-- ⳏ -> Ⳏ -- COPTIC CAPITAL LETTER OLD COPTIC HA
phpUpper '\11471' = '\11470'
-- ⳑ -> Ⳑ -- COPTIC CAPITAL LETTER L-SHAPED HA
phpUpper '\11473' = '\11472'
-- ⳓ -> Ⳓ -- COPTIC CAPITAL LETTER OLD COPTIC HEI
phpUpper '\11475' = '\11474'
-- ⳕ -> Ⳕ -- COPTIC CAPITAL LETTER OLD COPTIC HAT
phpUpper '\11477' = '\11476'
-- ⳗ -> Ⳗ -- COPTIC CAPITAL LETTER OLD COPTIC GANGIA
phpUpper '\11479' = '\11478'
-- ⳙ -> Ⳙ -- COPTIC CAPITAL LETTER OLD COPTIC DJA
phpUpper '\11481' = '\11480'
-- ⳛ -> Ⳛ -- COPTIC CAPITAL LETTER OLD COPTIC SHIMA
phpUpper '\11483' = '\11482'
-- ⳝ -> Ⳝ -- COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
phpUpper '\11485' = '\11484'
-- ⳟ -> Ⳟ -- COPTIC CAPITAL LETTER OLD NUBIAN NGI
phpUpper '\11487' = '\11486'
-- ⳡ -> Ⳡ -- COPTIC CAPITAL LETTER OLD NUBIAN NYI
phpUpper '\11489' = '\11488'
-- ⳣ -> Ⳣ -- COPTIC CAPITAL LETTER OLD NUBIAN WAU
phpUpper '\11491' = '\11490'
-- ⳬ -> Ⳬ -- COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
phpUpper '\11500' = '\11499'
-- ⳮ -> Ⳮ -- COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
phpUpper '\11502' = '\11501'
-- ⳳ -> Ⳳ -- COPTIC CAPITAL LETTER BOHAIRIC KHEI
phpUpper '\11507' = '\11506'
-- ꙁ -> Ꙁ -- CYRILLIC CAPITAL LETTER ZEMLYA
phpUpper '\42561' = '\42560'
-- ꙃ -> Ꙃ -- CYRILLIC CAPITAL LETTER DZELO
phpUpper '\42563' = '\42562'
-- ꙅ -> Ꙅ -- CYRILLIC CAPITAL LETTER REVERSED DZE
phpUpper '\42565' = '\42564'
-- ꙇ -> Ꙇ -- CYRILLIC CAPITAL LETTER IOTA
phpUpper '\42567' = '\42566'
-- ꙉ -> Ꙉ -- CYRILLIC CAPITAL LETTER DJERV
phpUpper '\42569' = '\42568'
-- ꙋ -> Ꙋ -- CYRILLIC CAPITAL LETTER MONOGRAPH UK
phpUpper '\42571' = '\42570'
-- ꙍ -> Ꙍ -- CYRILLIC CAPITAL LETTER BROAD OMEGA
phpUpper '\42573' = '\42572'
-- ꙏ -> Ꙏ -- CYRILLIC CAPITAL LETTER NEUTRAL YER
phpUpper '\42575' = '\42574'
-- ꙑ -> Ꙑ -- CYRILLIC CAPITAL LETTER YERU WITH BACK YER
phpUpper '\42577' = '\42576'
-- ꙓ -> Ꙓ -- CYRILLIC CAPITAL LETTER IOTIFIED YAT
phpUpper '\42579' = '\42578'
-- ꙕ -> Ꙕ -- CYRILLIC CAPITAL LETTER REVERSED YU
phpUpper '\42581' = '\42580'
-- ꙗ -> Ꙗ -- CYRILLIC CAPITAL LETTER IOTIFIED A
phpUpper '\42583' = '\42582'
-- ꙙ -> Ꙙ -- CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
phpUpper '\42585' = '\42584'
-- ꙛ -> Ꙛ -- CYRILLIC CAPITAL LETTER BLENDED YUS
phpUpper '\42587' = '\42586'
-- ꙝ -> Ꙝ -- CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
phpUpper '\42589' = '\42588'
-- ꙟ -> Ꙟ -- CYRILLIC CAPITAL LETTER YN
phpUpper '\42591' = '\42590'
-- ꙡ -> Ꙡ -- CYRILLIC CAPITAL LETTER REVERSED TSE
phpUpper '\42593' = '\42592'
-- ꙣ -> Ꙣ -- CYRILLIC CAPITAL LETTER SOFT DE
phpUpper '\42595' = '\42594'
-- ꙥ -> Ꙥ -- CYRILLIC CAPITAL LETTER SOFT EL
phpUpper '\42597' = '\42596'
-- ꙧ -> Ꙧ -- CYRILLIC CAPITAL LETTER SOFT EM
phpUpper '\42599' = '\42598'
-- ꙩ -> Ꙩ -- CYRILLIC CAPITAL LETTER MONOCULAR O
phpUpper '\42601' = '\42600'
-- ꙫ -> Ꙫ -- CYRILLIC CAPITAL LETTER BINOCULAR O
phpUpper '\42603' = '\42602'
-- ꙭ -> Ꙭ -- CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
phpUpper '\42605' = '\42604'
-- ꚁ -> Ꚁ -- CYRILLIC CAPITAL LETTER DWE
phpUpper '\42625' = '\42624'
-- ꚃ -> Ꚃ -- CYRILLIC CAPITAL LETTER DZWE
phpUpper '\42627' = '\42626'
-- ꚅ -> Ꚅ -- CYRILLIC CAPITAL LETTER ZHWE
phpUpper '\42629' = '\42628'
-- ꚇ -> Ꚇ -- CYRILLIC CAPITAL LETTER CCHE
phpUpper '\42631' = '\42630'
-- ꚉ -> Ꚉ -- CYRILLIC CAPITAL LETTER DZZE
phpUpper '\42633' = '\42632'
-- ꚋ -> Ꚋ -- CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
phpUpper '\42635' = '\42634'
-- ꚍ -> Ꚍ -- CYRILLIC CAPITAL LETTER TWE
phpUpper '\42637' = '\42636'
-- ꚏ -> Ꚏ -- CYRILLIC CAPITAL LETTER TSWE
phpUpper '\42639' = '\42638'
-- ꚑ -> Ꚑ -- CYRILLIC CAPITAL LETTER TSSE
phpUpper '\42641' = '\42640'
-- ꚓ -> Ꚓ -- CYRILLIC CAPITAL LETTER TCHE
phpUpper '\42643' = '\42642'
-- ꚕ -> Ꚕ -- CYRILLIC CAPITAL LETTER HWE
phpUpper '\42645' = '\42644'
-- ꚗ -> Ꚗ -- CYRILLIC CAPITAL LETTER SHWE
phpUpper '\42647' = '\42646'
-- ꜣ -> Ꜣ -- LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
phpUpper '\42787' = '\42786'
-- ꜥ -> Ꜥ -- LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
phpUpper '\42789' = '\42788'
-- ꜧ -> Ꜧ -- LATIN CAPITAL LETTER HENG
phpUpper '\42791' = '\42790'
-- ꜩ -> Ꜩ -- LATIN CAPITAL LETTER TZ
phpUpper '\42793' = '\42792'
-- ꜫ -> Ꜫ -- LATIN CAPITAL LETTER TRESILLO
phpUpper '\42795' = '\42794'
-- ꜭ -> Ꜭ -- LATIN CAPITAL LETTER CUATRILLO
phpUpper '\42797' = '\42796'
-- ꜯ -> Ꜯ -- LATIN CAPITAL LETTER CUATRILLO WITH COMMA
phpUpper '\42799' = '\42798'
-- ꜳ -> Ꜳ -- LATIN CAPITAL LETTER AA
phpUpper '\42803' = '\42802'
-- ꜵ -> Ꜵ -- LATIN CAPITAL LETTER AO
phpUpper '\42805' = '\42804'
-- ꜷ -> Ꜷ -- LATIN CAPITAL LETTER AU
phpUpper '\42807' = '\42806'
-- ꜹ -> Ꜹ -- LATIN CAPITAL LETTER AV
phpUpper '\42809' = '\42808'
-- ꜻ -> Ꜻ -- LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
phpUpper '\42811' = '\42810'
-- ꜽ -> Ꜽ -- LATIN CAPITAL LETTER AY
phpUpper '\42813' = '\42812'
-- ꜿ -> Ꜿ -- LATIN CAPITAL LETTER REVERSED C WITH DOT
phpUpper '\42815' = '\42814'
-- ꝁ -> Ꝁ -- LATIN CAPITAL LETTER K WITH STROKE
phpUpper '\42817' = '\42816'
-- ꝃ -> Ꝃ -- LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
phpUpper '\42819' = '\42818'
-- ꝅ -> Ꝅ -- LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
phpUpper '\42821' = '\42820'
-- ꝇ -> Ꝇ -- LATIN CAPITAL LETTER BROKEN L
phpUpper '\42823' = '\42822'
-- ꝉ -> Ꝉ -- LATIN CAPITAL LETTER L WITH HIGH STROKE
phpUpper '\42825' = '\42824'
-- ꝋ -> Ꝋ -- LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
phpUpper '\42827' = '\42826'
-- ꝍ -> Ꝍ -- LATIN CAPITAL LETTER O WITH LOOP
phpUpper '\42829' = '\42828'
-- ꝏ -> Ꝏ -- LATIN CAPITAL LETTER OO
phpUpper '\42831' = '\42830'
-- ꝑ -> Ꝑ -- LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
phpUpper '\42833' = '\42832'
-- ꝓ -> Ꝓ -- LATIN CAPITAL LETTER P WITH FLOURISH
phpUpper '\42835' = '\42834'
-- ꝕ -> Ꝕ -- LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
phpUpper '\42837' = '\42836'
-- ꝗ -> Ꝗ -- LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
phpUpper '\42839' = '\42838'
-- ꝙ -> Ꝙ -- LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
phpUpper '\42841' = '\42840'
-- ꝛ -> Ꝛ -- LATIN CAPITAL LETTER R ROTUNDA
phpUpper '\42843' = '\42842'
-- ꝝ -> Ꝝ -- LATIN CAPITAL LETTER RUM ROTUNDA
phpUpper '\42845' = '\42844'
-- ꝟ -> Ꝟ -- LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
phpUpper '\42847' = '\42846'
-- ꝡ -> Ꝡ -- LATIN CAPITAL LETTER VY
phpUpper '\42849' = '\42848'
-- ꝣ -> Ꝣ -- LATIN CAPITAL LETTER VISIGOTHIC Z
phpUpper '\42851' = '\42850'
-- ꝥ -> Ꝥ -- LATIN CAPITAL LETTER THORN WITH STROKE
phpUpper '\42853' = '\42852'
-- ꝧ -> Ꝧ -- LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
phpUpper '\42855' = '\42854'
-- ꝩ -> Ꝩ -- LATIN CAPITAL LETTER VEND
phpUpper '\42857' = '\42856'
-- ꝫ -> Ꝫ -- LATIN CAPITAL LETTER ET
phpUpper '\42859' = '\42858'
-- ꝭ -> Ꝭ -- LATIN CAPITAL LETTER IS
phpUpper '\42861' = '\42860'
-- ꝯ -> Ꝯ -- LATIN CAPITAL LETTER CON
phpUpper '\42863' = '\42862'
-- ꝺ -> Ꝺ -- LATIN CAPITAL LETTER INSULAR D
phpUpper '\42874' = '\42873'
-- ꝼ -> Ꝼ -- LATIN CAPITAL LETTER INSULAR F
phpUpper '\42876' = '\42875'
-- ꝿ -> Ꝿ -- LATIN CAPITAL LETTER TURNED INSULAR G
phpUpper '\42879' = '\42878'
-- ꞁ -> Ꞁ -- LATIN CAPITAL LETTER TURNED L
phpUpper '\42881' = '\42880'
-- ꞃ -> Ꞃ -- LATIN CAPITAL LETTER INSULAR R
phpUpper '\42883' = '\42882'
-- ꞅ -> Ꞅ -- LATIN CAPITAL LETTER INSULAR S
phpUpper '\42885' = '\42884'
-- ꞇ -> Ꞇ -- LATIN CAPITAL LETTER INSULAR T
phpUpper '\42887' = '\42886'
-- ꞌ -> Ꞌ -- LATIN CAPITAL LETTER SALTILLO
phpUpper '\42892' = '\42891'
-- ꞑ -> Ꞑ -- LATIN CAPITAL LETTER N WITH DESCENDER
phpUpper '\42897' = '\42896'
-- ꞓ -> Ꞓ -- LATIN CAPITAL LETTER C WITH BAR
phpUpper '\42899' = '\42898'
-- ꞡ -> Ꞡ -- LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
phpUpper '\42913' = '\42912'
-- ꞣ -> Ꞣ -- LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
phpUpper '\42915' = '\42914'
-- ꞥ -> Ꞥ -- LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
phpUpper '\42917' = '\42916'
-- ꞧ -> Ꞧ -- LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
phpUpper '\42919' = '\42918'
-- ꞩ -> Ꞩ -- LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
phpUpper '\42921' = '\42920'
-- ａ -> Ａ -- FULLWIDTH LATIN CAPITAL LETTER A
phpUpper '\65345' = '\65313'
-- ｂ -> Ｂ -- FULLWIDTH LATIN CAPITAL LETTER B
phpUpper '\65346' = '\65314'
-- ｃ -> Ｃ -- FULLWIDTH LATIN CAPITAL LETTER C
phpUpper '\65347' = '\65315'
-- ｄ -> Ｄ -- FULLWIDTH LATIN CAPITAL LETTER D
phpUpper '\65348' = '\65316'
-- ｅ -> Ｅ -- FULLWIDTH LATIN CAPITAL LETTER E
phpUpper '\65349' = '\65317'
-- ｆ -> Ｆ -- FULLWIDTH LATIN CAPITAL LETTER F
phpUpper '\65350' = '\65318'
-- ｇ -> Ｇ -- FULLWIDTH LATIN CAPITAL LETTER G
phpUpper '\65351' = '\65319'
-- ｈ -> Ｈ -- FULLWIDTH LATIN CAPITAL LETTER H
phpUpper '\65352' = '\65320'
-- ｉ -> Ｉ -- FULLWIDTH LATIN CAPITAL LETTER I
phpUpper '\65353' = '\65321'
-- ｊ -> Ｊ -- FULLWIDTH LATIN CAPITAL LETTER J
phpUpper '\65354' = '\65322'
-- ｋ -> Ｋ -- FULLWIDTH LATIN CAPITAL LETTER K
phpUpper '\65355' = '\65323'
-- ｌ -> Ｌ -- FULLWIDTH LATIN CAPITAL LETTER L
phpUpper '\65356' = '\65324'
-- ｍ -> Ｍ -- FULLWIDTH LATIN CAPITAL LETTER M
phpUpper '\65357' = '\65325'
-- ｎ -> Ｎ -- FULLWIDTH LATIN CAPITAL LETTER N
phpUpper '\65358' = '\65326'
-- ｏ -> Ｏ -- FULLWIDTH LATIN CAPITAL LETTER O
phpUpper '\65359' = '\65327'
-- ｐ -> Ｐ -- FULLWIDTH LATIN CAPITAL LETTER P
phpUpper '\65360' = '\65328'
-- ｑ -> Ｑ -- FULLWIDTH LATIN CAPITAL LETTER Q
phpUpper '\65361' = '\65329'
-- ｒ -> Ｒ -- FULLWIDTH LATIN CAPITAL LETTER R
phpUpper '\65362' = '\65330'
-- ｓ -> Ｓ -- FULLWIDTH LATIN CAPITAL LETTER S
phpUpper '\65363' = '\65331'
-- ｔ -> Ｔ -- FULLWIDTH LATIN CAPITAL LETTER T
phpUpper '\65364' = '\65332'
-- ｕ -> Ｕ -- FULLWIDTH LATIN CAPITAL LETTER U
phpUpper '\65365' = '\65333'
-- ｖ -> Ｖ -- FULLWIDTH LATIN CAPITAL LETTER V
phpUpper '\65366' = '\65334'
-- ｗ -> Ｗ -- FULLWIDTH LATIN CAPITAL LETTER W
phpUpper '\65367' = '\65335'
-- ｘ -> Ｘ -- FULLWIDTH LATIN CAPITAL LETTER X
phpUpper '\65368' = '\65336'
-- ｙ -> Ｙ -- FULLWIDTH LATIN CAPITAL LETTER Y
phpUpper '\65369' = '\65337'
-- ｚ -> Ｚ -- FULLWIDTH LATIN CAPITAL LETTER Z
phpUpper '\65370' = '\65338'
-- 𐐨 -> 𐐀 -- DESERET CAPITAL LETTER LONG I
phpUpper '\66600' = '\66560'
-- 𐐩 -> 𐐁 -- DESERET CAPITAL LETTER LONG E
phpUpper '\66601' = '\66561'
-- 𐐪 -> 𐐂 -- DESERET CAPITAL LETTER LONG A
phpUpper '\66602' = '\66562'
-- 𐐫 -> 𐐃 -- DESERET CAPITAL LETTER LONG AH
phpUpper '\66603' = '\66563'
-- 𐐬 -> 𐐄 -- DESERET CAPITAL LETTER LONG O
phpUpper '\66604' = '\66564'
-- 𐐭 -> 𐐅 -- DESERET CAPITAL LETTER LONG OO
phpUpper '\66605' = '\66565'
-- 𐐮 -> 𐐆 -- DESERET CAPITAL LETTER SHORT I
phpUpper '\66606' = '\66566'
-- 𐐯 -> 𐐇 -- DESERET CAPITAL LETTER SHORT E
phpUpper '\66607' = '\66567'
-- 𐐰 -> 𐐈 -- DESERET CAPITAL LETTER SHORT A
phpUpper '\66608' = '\66568'
-- 𐐱 -> 𐐉 -- DESERET CAPITAL LETTER SHORT AH
phpUpper '\66609' = '\66569'
-- 𐐲 -> 𐐊 -- DESERET CAPITAL LETTER SHORT O
phpUpper '\66610' = '\66570'
-- 𐐳 -> 𐐋 -- DESERET CAPITAL LETTER SHORT OO
phpUpper '\66611' = '\66571'
-- 𐐴 -> 𐐌 -- DESERET CAPITAL LETTER AY
phpUpper '\66612' = '\66572'
-- 𐐵 -> 𐐍 -- DESERET CAPITAL LETTER OW
phpUpper '\66613' = '\66573'
-- 𐐶 -> 𐐎 -- DESERET CAPITAL LETTER WU
phpUpper '\66614' = '\66574'
-- 𐐷 -> 𐐏 -- DESERET CAPITAL LETTER YEE
phpUpper '\66615' = '\66575'
-- 𐐸 -> 𐐐 -- DESERET CAPITAL LETTER H
phpUpper '\66616' = '\66576'
-- 𐐹 -> 𐐑 -- DESERET CAPITAL LETTER PEE
phpUpper '\66617' = '\66577'
-- 𐐺 -> 𐐒 -- DESERET CAPITAL LETTER BEE
phpUpper '\66618' = '\66578'
-- 𐐻 -> 𐐓 -- DESERET CAPITAL LETTER TEE
phpUpper '\66619' = '\66579'
-- 𐐼 -> 𐐔 -- DESERET CAPITAL LETTER DEE
phpUpper '\66620' = '\66580'
-- 𐐽 -> 𐐕 -- DESERET CAPITAL LETTER CHEE
phpUpper '\66621' = '\66581'
-- 𐐾 -> 𐐖 -- DESERET CAPITAL LETTER JEE
phpUpper '\66622' = '\66582'
-- 𐐿 -> 𐐗 -- DESERET CAPITAL LETTER KAY
phpUpper '\66623' = '\66583'
-- 𐑀 -> 𐐘 -- DESERET CAPITAL LETTER GAY
phpUpper '\66624' = '\66584'
-- 𐑁 -> 𐐙 -- DESERET CAPITAL LETTER EF
phpUpper '\66625' = '\66585'
-- 𐑂 -> 𐐚 -- DESERET CAPITAL LETTER VEE
phpUpper '\66626' = '\66586'
-- 𐑃 -> 𐐛 -- DESERET CAPITAL LETTER ETH
phpUpper '\66627' = '\66587'
-- 𐑄 -> 𐐜 -- DESERET CAPITAL LETTER THEE
phpUpper '\66628' = '\66588'
-- 𐑅 -> 𐐝 -- DESERET CAPITAL LETTER ES
phpUpper '\66629' = '\66589'
-- 𐑆 -> 𐐞 -- DESERET CAPITAL LETTER ZEE
phpUpper '\66630' = '\66590'
-- 𐑇 -> 𐐟 -- DESERET CAPITAL LETTER ESH
phpUpper '\66631' = '\66591'
-- 𐑈 -> 𐐠 -- DESERET CAPITAL LETTER ZHEE
phpUpper '\66632' = '\66592'
-- 𐑉 -> 𐐡 -- DESERET CAPITAL LETTER ER
phpUpper '\66633' = '\66593'
-- 𐑊 -> 𐐢 -- DESERET CAPITAL LETTER EL
phpUpper '\66634' = '\66594'
-- 𐑋 -> 𐐣 -- DESERET CAPITAL LETTER EM
phpUpper '\66635' = '\66595'
-- 𐑌 -> 𐐤 -- DESERET CAPITAL LETTER EN
phpUpper '\66636' = '\66596'
-- 𐑍 -> 𐐥 -- DESERET CAPITAL LETTER ENG
phpUpper '\66637' = '\66597'
-- 𐑎 -> 𐐦 -- DESERET CAPITAL LETTER OI
phpUpper '\66638' = '\66598'
-- 𐑏 -> 𐐧 -- DESERET CAPITAL LETTER EW
phpUpper '\66639' = '\66599'
-- ı -> I -- LATIN SMALL LETTER DOTLESS I
phpUpper '\305' = 'I'
-- ſ -> S -- LATIN SMALL LETTER LONG S
phpUpper '\383' = 'S'
-- -> Ι -- GREEK NON-SPACING IOTA BELOW
-- phpUpper '\837' = '\921'
-- ι -> Ι -- GREEK SMALL LETTER IOTA
phpUpper '\953' = '\921'
-- ς -> Σ -- GREEK SMALL LETTER FINAL SIGMA
phpUpper '\962' = '\931'
-- ϐ -> Β -- GREEK SMALL LETTER CURLED BETA
phpUpper '\976' = '\914'
-- ϑ -> Θ -- GREEK SMALL LETTER SCRIPT THETA
phpUpper '\977' = '\920'
-- ϕ -> Φ -- GREEK SMALL LETTER SCRIPT PHI
phpUpper '\981' = '\934'
-- ϖ -> Π -- GREEK SMALL LETTER OMEGA PI
phpUpper '\982' = '\928'
-- ϰ -> Κ -- GREEK SMALL LETTER SCRIPT KAPPA
phpUpper '\1008' = '\922'
-- ϱ -> Ρ -- GREEK SMALL LETTER TAILED RHO
phpUpper '\1009' = '\929'
-- ϵ -> Ε -- GREEK LUNATE EPSILON SYMBOL
phpUpper '\1013' = '\917'
-- ẛ -> Ṡ -- LATIN SMALL LETTER LONG S WITH DOT ABOVE
phpUpper '\7835' = '\7776'
-- ι -> Ι -- GREEK PROSGEGRAMMENI
phpUpper '\8126' = '\921'

phpUpper c = c

