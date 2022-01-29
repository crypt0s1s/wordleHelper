# wordle

Dependencies:

split
- stack install split

multiset
- stack install multiset

Usage:

The program takes up to 4 arguments detailing the following

1. letters that are in the word (e.g. abz)
2. letters that aren't in the word (e.g. ceh)
3. letters that are in the word at a specific location seperated by commas (e.g. r,e,t,r,y  - can only return the word retry)
4. letters that aren't in the word at a specific location seperated by commas (e.g. v,a,ca,n,t)

All arguments can be ommited. If there are no known letters in the word a '.' can be used as a replacement.


Full examples:

stack run . adieu
(no known letters and the letters 'a','d','i','e','u' are not in the word)

stack run zer wqjb z,,,, ,,,,er
(z is in the first position in the word. 'e' and 'r' are not in the last position in the word)