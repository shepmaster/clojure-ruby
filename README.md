# clojure-ruby

A Clojure program to interpret a **very** narrow subset of Ruby.

Written for the [Reddit daily programmer][rdp] challenge

[rdp]: http://www.reddit.com/r/dailyprogrammer/comments/210j6i/21314_challenge_153_hard_interpreting_interpreters/#

## Usage

```
lein run path-to-ruby-code.rb
```

Remember that it's unlikely that any arbitrary Ruby code will work. An
`example.rb` file is included that should work, but you should try the
Brainfuck interpreter from my other project, [ruby-brainfuck][rbbf].

There should be exceptions thrown for bad input, but they aren't very
easy to read!

[rbbf]: https://github.com/shepmaster/ruby-brainfuck

## Thanks

Many thanks to the [instaparse][instaparse] library, which took care
of all the heavy lifting of parsing!

[instaparse]: https://github.com/Engelberg/instaparse

## License

Copyright © 2014 Jake Goulding

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
