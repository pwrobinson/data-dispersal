# More Information

For detailed usage examples, complexity bounds, and more information see
[Information Dispersal and Secret Sharing in Haskell](https://blog.lowerbound.io/2020-05-10_Data_dispersal_in_Haskell.html)


# GHCi Example

The following is a simple example. That is, we split a ByteString into 15 fragments such that any 5 of them are sufficient to recover the original data. In the first example we don't use encryption, so another party may gain some knowledge of our data if they get their hands on some fragments.

## Without Security Guarantees

~~~ {.haskell}
❯ :m + Data.IDA
❯ let msg = Data.ByteString.Char8.pack "my really important data"
❯ let fragments = encode 5 15 msg
❯ -- Let's pretend that we lost the first 5 and the last 5 fragments:
❯ let frags' = drop 5 $ take 10 fragments
❯ -- These are the 10 fragments that we have left:
❯ mapM_ (Prelude.putStrLn . show)  frags'
(6,[273,771,899,737,285])
(7,[289,939,612,285,936])
(8,[424,781,1001,322,788])
(9,[143,657,790,157,423])
(10,[314,674,418,888,423])
❯ -- Space-efficiency: Note that the length of each of the 5 fragments is 5
❯ -- and our original message has length 24.
❯ decode frags'
"my really important data"
~~~

## With Information-Theoretic Security

~~~ {.haskell}
❯ :m + Crypto.IDA
❯ let msg = Data.ByteString.Lazy.Char8.pack "my really important data"
❯ fragments <- encode 5 15 msg
❯ -- Let's pretend that we lost the first 5 and the last 5 fragments:
❯ let frags' = drop 5 $ take 10 fragments
❯ -- These are the 5 fragments that we have left:
❯ mapM_ (Prelude.putStrLn . show)  frags'
((6,"x\SI\148p\216\151EE\179E7\191\DLE!\190p*C\236zS2\247P\160\167T\172\159\143P\186"),[384,957,737,857,415])
((7,"\t/T\224[\129t\226W\201\161\DC4\233\SI)\241\131P\SOt_5;\DC2*_\236<\210\187\152\221"),[126,193,443,775,713])
((8,"\147\226\161\&4\190\209CsS\FS@g\163\138\SOH\209nB1\248A\224A\172IK\176Y\195]W\175"),[735,312,773,379,596])
((9,"L]\168\153\129\&3\219\238\201G\248\173\&89\SO\162[\131\224\n\233\161I\169{zo-\237\146\EM\175"),[244,213,112,1,1012])
((10,"\228\229v\169v\CAN\192\246\134\168\191\&2\148t]\a\ESCb\189\&9V\230\&9B\192h\247\197\202d\194\244"),[491,619,275,244,867]

❯ decode frags'
"my really important data"
~~~


