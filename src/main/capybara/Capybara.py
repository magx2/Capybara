import sys
from antlr4 import *
from grammar.CapybaraLexer import *
from grammar.CapybaraParser import *
from grammar.CapybaraListener import *


class CapybaraListenerImpl(CapybaraListener):
    pass


def main(argv):
    print("Start...")
    input_stream = FileStream(argv[1])
    lexer = CapybaraLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = CapybaraParser(stream)
    tree = parser.compileUnit()

    walker = ParseTreeWalker()
    listener = CapybaraListenerImpl()
    walker.walk(listener, tree)

    print("End.")


if __name__ == '__main__':
    main(sys.argv)
