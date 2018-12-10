#!/usr/bin/env stack
{- stack --resolver=nightly-2018-11-14 script --compile
   --package "relude here megaparsec pretty-show"
-}

-- {-# Language NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

-- import Relude
import Data.List
import Data.String.Here
import Text.Show.Pretty

pp :: Show a => a -> IO ()
pp = putStrLn . ppShow

{-
You stop falling through time, catch your breath, and check the screen
on the device. "Destination reached. Current Year: 1518. Current
Location: North Pole Utility Closet 83N10." You made it! Now, to find
those anomalies.

Outside the utility closet, you hear footsteps and a voice. "...I'm
not sure either. But now that so many people have chimneys, maybe he
could sneak in that way?" Another voice responds, "Actually, we've
been working on a new kind of suit that would let him fit through
tight spaces like that. But, I heard that a few days ago, they lost
the prototype fabric, the design plans, everything! Nobody on the team
can even seem to remember important details of the project!"

"Wouldn't they have had enough fabric to fill several boxes in the
warehouse? They'd be stored together, so the box IDs should be
similar. Too bad it would take forever to search the warehouse for two
similar box IDs..." They walk too far away to hear any more.

Late at night, you sneak to the warehouse - who knows what kinds of
paradoxes you could cause if you were discovered - and use your fancy
wrist device to quickly scan every box and produce a list of the
likely candidates (your puzzle input).

To make sure you didn't miss any, you scan the likely candidate boxes
again, counting the number that have an ID containing exactly two of
any letter and then separately counting those with exactly three of
any letter. You can multiply those two counts together to get a
rudimentary checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

    abcdef contains no letters that appear exactly two or three times.
    bababc contains two a and three b, so it counts for both.
    abbcde contains two b, but no letter appears exactly three times.
    abcccd contains three c, but no letter appears exactly two times.
    aabcdd contains two a and two d, but it only counts once.
    abcdee contains two e.
    ababab contains three a and three b, but it only counts once.

Of these box IDs, four of them contain a letter which appears exactly
twice, and three of them contain a letter which appears exactly three
times. Multiplying these together produces a checksum of 4 * 3 = 12.

What is the checksum for your list of box IDs?
-}
doubleTriple s = (any ((==2).length) gs, any ((==3).length) gs)
  where gs = group $ sort s

a =
  let
    dts = map doubleTriple $ lines input
    ds = length $ filter (==True) $ map fst dts
    ts = length $ filter (==True) $ map snd dts
  in
    ds * ts
      
{-
Confident that your list of box IDs is complete, you're ready to find
the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the
same position in both strings. For example, given the following box
IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz

The IDs abcde and axcye are close, but they differ by two characters
(the second and fourth). However, the IDs fghij and fguij differ by
exactly one character, the third (h and u). Those must be the correct
boxes.

What letters are common between the two correct box IDs? (In the
example above, this is found by removing the differing character from
either ID, producing fgij.)
-}

ids = lines input
differ (a,b) = filter (\(x,y) -> x /= y) $ zip a b
common (a,b) = filter (\(x,y) -> x == y) $ zip a b
differBy1 (a,b) = length (differ (a,b)) == 1
pairs [] = []
pairs (_:[]) = []
pairs (x:ys) = map (x,) ys ++ pairs ys
commonpair = head $ filter ((==True).differBy1) $ pairs ids
b = map fst $ common commonpair
  
test = lines $ [here|
abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
|]

input = [here|
ymdrcyapvwfloiuktanxzjsieb
ymdrwhgznwfloiuktanxzjsqeb
ymdrchguvwfloiuktanxmjsleb
pmdrchgmvwfdoiuktanxzjsqeb
ymdrfegpvwfloiukjanxzjsqeb
ymdrchgpvwfloiukmanazjsdeb
ymdsnhgpvwflciuktanxzjsqeb
lmdrbhrpvwfloiuktanxzjsqeb
ymdrwhgpvwfloiukeanxzjsjeb
ymdrchgpvpfloihktanszjsqeb
omdrchgpvwflokuktanazjsqeb
kmsrchgpvwfloiuktanxqjsqeb
ymdrchopvwzloiustanxzjsqeb
omdrchgpvwfloiuktawxtjsqeb
ymdrchgpvwfroiukhanozjsqeb
ymdrchgpvwfloikktanyzosqeb
ymdrchgpvwfioiuktaexzjsqea
ymdrcngpvwfloiuktanxzjsamb
ymdrchgpqwfaoiuktanxxjsqeb
ymdrcmgpvwflziuktakxzjsqeb
ymdrchguvwsloiuktanxzjsqen
ymdrchppowfloiuvtanxzjsqeb
ymdrcngpvwflyiukkanxzjsqeb
ymdrcbgpvwfloiukjanxzjspeb
ymdrchgpvwflopuktanxzosseb
ygdrchgpvwfloiukxanxcjsqeb
ymdrchgpvwfloauktanuzjsqei
ymerchgpvwfloiumtanxzjsqet
ymdlcegpvwfloiuktbnxzjsqeb
ymdrclgpvwfloiukyanxzjlqeb
ymdrchgpvwhmoiuktanxijsqeb
ymdrchgpwrfloiuktanxdjsqeb
ymdbcwgpvwfloiuktanxzusqeb
ymgrchgphwfloiuktanxzjspeb
imdrchgpvwflmiuktanxzjsqib
ymdrihgpvwfloiiktanxzjsteb
ywdrchgpvwfloibkvanxzjsqeb
ymdrchgpxwfloiuktanezjsqes
ymdrchgpiwfloiukxanxzhsqeb
ymdrchgpveflokuktdnxzjsqeb
kmdrchgpvwfloviktanxzjsqeb
ymdrchgpvgfmoiuytanxzjsqeb
ymyrchgpvzfluiuktanxzjsqeb
ymdrchguvwfloiuktanxpjsqlb
ymerchgpvwfloiukthnxsjsqeb
hmdrchgpvwglfiuktanxzjsqeb
ymdrchgpvwfdoiuktanxzjsqaf
yudrchgdvwfloiuktaexzjsqeb
ymdbchgxvwfloiuktanxzjsqem
ymdrchgpvwfloiumjanxzjsqpb
ymdrchgpqwfloiuqtanxrjsqeb
ymdqchhpvwfloiuktanxzzsqeb
ymdryhgpfwfloiuktanxzjsyeb
xmdrchgpvwfloioitanxzjsqeb
ykdrchgpvwfloiuktcnxzisqeb
ymdrcpgprwfloiuktanqzjsqeb
yidrchgpvwfloiuktanxzjgleb
ymdrchgpxwfloiuktanxzjsxfb
ymdrchgfvwfloiuktanxzjiteb
ymdrchgvvwflqifktanxzjsqeb
ymdrchgplwfloiuktanizjnqeb
ymdrchgpvwfyfiuktafxzjsqeb
ymddchgpvwcloiuktanxzjsqeq
ymdrchgkvwflaiuktanxzjsqfb
yudrchgpvwfzoiuktanxzjsreb
ymdrdhgpvwfloiuktnnxqjsqeb
ymdrnhgpvwfloiuktauxzjdqeb
ymdrchgpvwflsiddtanxzjsqeb
ymdrchgpvwhloeuktanxzjsqek
ymdrchgpvjfioiuktawxzjsqeb
ycdrohgpvwfgoiuktanxzjsqeb
ymdrchgpvwflmifktanxzjsqel
yfdrchrpvwfloruktanxzjsqeb
ymdrchgjvwfloiuktanxzrsqeg
ymarchgpxwfloiukkanxzjsqeb
ymdrchgppwflghuktanxzjsqeb
ymdrchvpvwfloiuktanxpjrqeb
ymdlchgpqjfloiuktanxzjsqeb
ymdrchgpvwfofiuktandzjsqeb
ymdrcngpqwfloiuktanlzjsqeb
ymdrchgpvwfloiuiocnxzjsqeb
ymdrcogpvwfloizktanxzjcqeb
ymdrchgpvlfvoiuksanxzjsqeb
ymdrchgpvwflocpctanxzjsqeb
ymdrchgpvwfloiuktanlzjsejb
yndrchgpvwflzifktanxzjsqeb
ymdrcrgpvkfloiuktanxrjsqeb
ymdrchhpvwslocuktanxzjsqeb
ymdrxhgpvwfloiuwtazxzjsqeb
ymdrchgpvafloiuutanxzjsqxb
ymdrchppvhfloquktanxzjsqeb
ymprcugpvwtloiuktanxzjsqeb
ymdrchgpvvflyiuktanxzjsqvb
ymdrchgovwfloiuftanxzjwqeb
ymdrchrpvwflotyktanxzjsqeb
gmdrchgpvwfloauttanxzjsqeb
ymdrchmpvofloiukmanxzjsqeb
ymdrchgpvwflsiuktanxzjspkb
ymdrchgpvwfloluktajxijsqmb
ymdrcngpvwfloiukbanxzdsqeb
ymdrchgpvwploiuktnnxzmsqeb
ymdrcwgpvwfloiuktbnxhjsqeb
ymdrcngpvwfloiuktaaxbjsqeb
ykdrchgpvwfloiuktanxzgsqej
yuhrchgpvwfdoiuktanxzjsqeb
ymdrchgpvsfloiukbanxujsqeb
ymqrchgpvwfliiuktanxzjsteb
ysdqchgpvwfloiuktanxzjtqeb
ymdjchgpcwfloiuktanxzrsqeb
ymdkchgpvwfloiukfanlzjsqeb
ymdrchgpvxfloikktanxzjiqeb
smdrchgwewfloiuktanxzjsqeb
ymdrchgpvwfljiuktanxajsqer
ymdrchgpowflifuktanxzjsqeb
ymdrchgpvpzloiukoanxzjsqeb
yydrchgwvwfvoiuktanxzjsqeb
ymdgcdgpvwflobuktanxzjsqeb
ymdechgpvkfloiuktanxzjsjeb
ymdnchnpvwfloixktanxzjsqeb
ymdrchgpiefloiuktqnxzjsqeb
ymprchgpvwfloiuktjnxzjsxeb
ymdrjdgpzwfloiuktanxzjsqeb
ymsrchgpywfloiuktanxzjsueb
ymdrchgpvgoloiuktanxzcsqeb
ymdrphgpswflbiuktanxzjsqeb
ymqrchgpvnfloiumtanxzjsqeb
ymjrchgpvwyloiuktacxzjsqeb
ymdrchepvwmlqiuktanxzjsqeb
kmirchgpvwfloiuktanxzjsreb
ymdncygpvwfloiuktanuzjsqeb
ymdrzhgpvwploiuktanxzxsqeb
ymdrchkpvwfloiwkmanxzjsqeb
ywdrchgovwfloiuktanxzjsceb
amdrchgpvwfloiuktanrzjqqeb
ymdpshgpvwfloiuktanxzjyqeb
ymdrcegpvwfloijktcnxzjsqeb
ymdrcygpvwfloiuktanxztsqwb
ymdrchgpvufloiuvtabxzjsqeb
ymdrchgpvwflkiuktrnxzjsqmb
ymdrchgpvqfloiuktanxpjfqeb
ymdrclgpvkfloiyktanxzjsqeb
ybdxchgpvwfloiuktanxzjskeb
pmdrchgpvwfzoirktanxzjsqeb
ycdfchgpvwfloiuktanxzjtqeb
ymdrchgpdwfloiumtbnxzjsqeb
ymdrchgpqmfloiuktanxzjsqer
ymgrchgpvwfroiuktanxzjsqey
ymdrnhgpvwfloiuktanjzjsqlb
dmdrchgpvgfloiuktqnxzjsqeb
yudrchgnvwfloiukranxzjsqeb
ymdrxhgpvafloiuktanxzjsqeq
ymdrchgpvwfyofuktanxzjsueb
ymdrrhgpvwfloiuktavxzjsqpb
yvdrchgpvwfloiuktalxzhsqeb
ymdrchgpbwfloiuktanxzfnqeb
ymdrqhgpvwfloiuvtznxzjsqeb
ymdrchgpvbfloiuetanxzjsqeo
ymdrchjpvwfloiuktanxzjnqrb
ymdrchgpmwfqoiuknanxzjsqeb
ymdrchgpvwfuoiuktaqxzjtqeb
ymdrchgpvwfloiuktamxaosqeb
fmdrchgpvffloiuktanxzjsaeb
ymdrrhglvwfwoiuktanxzjsqeb
ymdrchgpvwflohuktanxzjcqei
ymdrcsgpvwfloiuktaexzjsqek
ymlrchfpvwfloiuktpnxzjsqeb
yxdrchgpvwfdoiuvtanxzjsqeb
ymdrchgrvwfloiuktadxzjsqew
ymdrchgpvwbloiyktandzjsqeb
ymdrchgpvsfloiyktanozjsqeb
ymdrchgpjwfloiuktanxibsqeb
ymdrchgjvyfloiuktanxzjsqeh
ymdrchgvvwfloiuktanzrjsqeb
ymdrchgpvwaloiuktynxzjsqev
ymdrccgpvwflonvktanxzjsqeb
ymdrchgqvffloiuktanxfjsqeb
ymdbchgpvwsloiudtanxzjsqeb
ymdachgpvwfloiuktanlzjsqwb
ymdrclgpvwwloiuktanxzjsjeb
ybdpchgpvwdloiuktanxzjsqeb
ymdtchgpvwfleijktanxzjsqeb
ymdrchgpvwfloiustanxzjsxep
ymdrcjypvwfloiuktanxnjsqeb
ymdrcdgpvwfloiuutanxkjsqeb
yhirchgpvufloiuktanxzjsqeb
ymdrlhgpvwfluigktanxzjsqeb
ywdrhhgpvwftoiuktanxzjsqeb
ymdrchgpvwflyiuktanozjsqtb
cmdrchgpuwfloiukmanxzjsqeb
ymdochgpvrfloiuktanvzjsqeb
ymdrcvgpvwfgoiuktfnxzjsqeb
ymdrchgpmufloiuktanxzssqeb
ymurchgrvwfloiuktanxzjsqep
bmdrchgpvwfloiukpanxzjsqmb
ymdrchgphwvloiuktanszjsqeb
ymdpkhgpvwfloiuktanxzjsqtb
ymdrchgpvwfloiuwtanxzjfqev
ymdrchgpvwfloguktqlxzjsqeb
ymkrshgpvwflgiuktanxzjsqeb
ymdrchgpzwfloizktanxznsqeb
ymdrchgpvxfloiuktegxzjsqeb
yydrchgpwwfloiuktanxzjsqqb
ymdrcngwvwfltiuktanxzjsqeb
ymdszhgwvwfloiuktanxzjsqeb
ymdrchguvwfjoiuktanxzxsqeb
ymdomhgpvwfloiuktanxgjsqeb
ymdrcvgpvwfloiuktanwzzsqeb
yydrchgpvwfloiuktanxzjmqtb
rmdrchgpvwfloiuktmnszjsqeb
ykdrchgpvwfloyuktmnxzjsqeb
ymcrchkpvwfloiuktanxzjsoeb
ymdrcrgpvwfloiukpanxzjsceb
yrdrchgpvwfloiukwanxzjsqhb
ymdrcfgpvwfloiurtanxojsqeb
ymdrchgpuwstoiuktanxzjsqeb
ymdrchgpvwflpxuktanxzjsqer
ymdrehgpvwfloiuktabxdjsqeb
yedrchgpvwfloiukqanxzjiqeb
ymdrthgpvyfloiuktanxzjsqen
cmdlchgpvwfloiuvtanxzjsqeb
ymdrchgpvwtloiuktanlpjsqeb
ymdrchgpvwfloiuktanyvjsqea
gmdrcogpvwfloiuktanxzjsqqb
ymmrchgpvwflosuktauxzjsqeb
ymgrchgjvwfloiuktavxzjsqeb
ymdbclgpvwfloeuktanxzjsqeb
ymdrchgpvwfloiuktaixzcsqfb
ymdrchgpvwflmiuktanxttsqeb
ymxrchgpvwfloiuktanxzfsqec
yqzrchgpcwfloiuktanxzjsqeb
yvdrchgpvwfloiukgvnxzjsqeb
ymdrchepvwfloiuktahxzosqeb
ymdlchgpvwfloiuktamizjsqeb
ymdrchgpcwflovuktanxzjsqzb
yvduchgpvwfloiukaanxzjsqeb
ymdrchgpvwfloiuktxmxzjsgeb
ymdrcrgpvwfloizktanbzjsqeb
amdrchgpvwfloiukhanxzjsqbb
ymdrchgpvwfloluktajxijsqeb
ymdrcfgpvwfloiubtanxznsqeb
ymdrchgpvwfleiuwtanxzjsweb
ymdrchgpvwfzdguktanxzjsqeb
ymdrchgwvwflosyktanxzjsqeb
ymrrchgpvwfloiultanxzjsqez
ymdpchgkvwfleiuktanxzjsqeb
ymdrchgpvwfloijktalxfjsqeb
ymdrchgpmwfloiuktanzzjsqfb
ymdrcsgpvwfljiukyanxzjsqeb
ymdrcarpvwfloiuktapxzjsqeb
ymdrchgpvwfloiuktanxzjcqvs
|]
