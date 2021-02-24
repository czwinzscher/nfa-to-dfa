# nfa-to-dfa
[![GitHub CI](https://github.com/czwinzscher/nfa-to-dfa/workflows/CI/badge.svg)](https://github.com/czwinzscher/nfa-to-dfa/actions)

This is a program to convert a NFA into a DFA. The CLI can read the definition of the NFA from JSON and output it either as JSON or in the DOT format.

## installation
```bash
git clone https://github.com/czwinzscher/nfa-to-dfa.git
cd nfa-to-dfa
stack install
```

## usage
```bash
# output as json and write to stdout
nfa-to-dfa -f example.json --output-json
# output as in dot format and use the dot program to create an image
nfa-to-dfa -f example.json --output-dot | dot -Tpng > out.png
# read from stdin and write to file
cat example.json | nfa-to-dfa --stdin --output-dot -o out.dot
```
