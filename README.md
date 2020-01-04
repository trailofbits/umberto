# Umberto

Umberto is a library designed for mutating generic structured data (e.g., JSON, XML, X.509) and a command line application wrapping this functionality. It can apply a variety of mutations, synthesize totally new values, and apply existing mutators (e.g. [Radamsa](https://gitlab.com/akihe/radamsa)) to structure components. If you're trying to perform grammar-based fuzzing, you might find it very useful. It's probably easiest to understand by example:

```
echo -n "{\"a\": 1, \"b\": 2, \"c\": \"test\"}" | umberto-mutate json knuth all

    {"a":2,"c":"tesb","t":1}

echo -n "{\"a\": 1, \"b\": 2, \"c\": \"test\"}" | umberto-mutate json newvals nums

    {"a":-2.8e-20,"b":-40000,"c":"test"}

echo -n "{\"a\": 1, \"b\": 2, \"c\": \"test\"}" | umberto-mutate json radamsa strings

    {"c\u0008^\u001f":"ó ¨teï»â¨¿st","1313131313131313â":1,"ô ó ó ¶Db":2}

echo -n "{\"a\": 1, \"b\": 2, \"c\": \"test\"}" | umberto-mutate string replacement all
    
    :"c2:tb{ :"{c"}t"as }c"e": "2
```

# Installation

Umberto uses [`stack`](https://docs.haskellstack.org/en/stable/README/). Install stack, clone this repo, then run `stack build && stack install`. If you want to use the Radamsa fuzzer, you will also need to [install it](https://gitlab.com/akihe/radamsa) separately.

# Command Line Usage

Umberto provides a simple executable, `umberto-mutate`, that wraps most of its functionality. Typical usage is as follows:

`echo -n mydata | umberto-mutate FORMAT MUTATOR TARGET`

FORMAT can be any of the following:

  * `json`
  
  * `string` (or essentially "no format")
  
  * `xml` (relatively untested)

  * `der` (X.509 DER, relatively untested)
  
MUTATOR can be any of the following:

  * `knuth`: Knuth shuffle targeted values
  
  * `replacement`: Replacement-shuffle targeted values
  
  * `newvals`: Synthesize new targeted values in place of old ones
  
  * `radamsa`: Shell out to radamsa for mutation (only works on strings)
  
TARGET can be any of the following:

  * `strings`: String types
  
  * `nums`: Numeric types
  
  * `all`: All types

# Library Usage

Most of the functionality of Umberto is available in [`lib/Umberto.hs`](lib/Umberto.hs). Mutators that make use of typeclass polymorphism require some usage of Template Haskell to violate the [open world assumption](http://book.realworldhaskell.org/read/using-typeclasses.html#id608052). This is available in [`Umberto.TH`](lib/Umberto/TH.hs) but should only be used in executable code, never in libraries.
