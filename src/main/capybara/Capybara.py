import sys
from antlr4 import *
from grammar.CapybaraLexer import *
from grammar.CapybaraParser import *
from grammar.CapybaraListener import *
from compile_unit import *
import json

def main(argv):
    print("Start...")

    compile_units = list(map(parse, argv[1:]))

    print('---')
    print('Parsed compile units:')
    for unit in compile_units:
        print(json.dumps(unit, indent=4))

    print("End.")


if __name__ == '__main__':
    main(sys.argv)
