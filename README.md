# nfa-to-dfa
[![GitHub CI](https://github.com/czwinzscher/nfa-to-dfa/workflows/CI/badge.svg)](https://github.com/czwinzscher/nfa-to-dfa/actions)

This is a program to convert a NFA into a DFA. The CLI can read the definition of the NFA from JSON and output it either as JSON or as Text.

## installation
```bash
git clone https://github.com/czwinzscher/nfa-to-dfa.git
cd nfa-to-dfa
stack install
```

## usage
```bash
# output as json and write to file
nfa-to-dfa -f example.json --output-json -o out.json
# output as text and write to stdout
nfa-to-dfa -f example.json --output-text
# read from stdin
cat example.json | nfa-to-dfa --stdin --output-json
```
