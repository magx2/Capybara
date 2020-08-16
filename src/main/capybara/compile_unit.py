import sys
from antlr4 import *
from grammar.CapybaraLexer import *
from grammar.CapybaraParser import *
from grammar.CapybaraListener import *
import hashlib


class CapybaraListenerImpl(CapybaraListener):
    def __init__(self, compile_unit):
        self.compile_unit = compile_unit
        self.compile_unit['package_name'] = ''
        self.compile_unit['structs'] = []
        self.compile_unit['functions'] = []

    # Enter a parse tree produced by CapybaraParser#compileUnit.
    def enterCompileUnit(self, ctx: CapybaraParser.CompileUnitContext):
        print("enterCompileUnit")

    # Exit a parse tree produced by CapybaraParser#compileUnit.
    def exitCompileUnit(self, ctx: CapybaraParser.CompileUnitContext):
        print("exitCompileUnit")

    # Enter a parse tree produced by CapybaraParser#packageDeclaration.
    def enterPackageDeclaration(self, ctx: CapybaraParser.PackageDeclarationContext):
        print("enterPackageDeclaration")
        self.compile_unit['package_name'] += str(ctx.PACKAGE())

    # Exit a parse tree produced by CapybaraParser#packageDeclaration.
    def exitPackageDeclaration(self, ctx: CapybaraParser.PackageDeclarationContext):
        print("exitPackageDeclaration")

    # Enter a parse tree produced by CapybaraParser#code.
    def enterCode(self, ctx: CapybaraParser.CodeContext):
        print("enterCode")

    # Exit a parse tree produced by CapybaraParser#code.
    def exitCode(self, ctx: CapybaraParser.CodeContext):
        print("exitCode")

    # Enter a parse tree produced by CapybaraParser#struct.
    def enterStruct(self, ctx: CapybaraParser.StructContext):
        print("enterStruct " + str(ctx.ALPH_NUM_STARTING_WITH_CAPITAL()))
        self.compile_unit['structs'].append(
            {
                'package_name': self.compile_unit['package_name'],
                'name': str(ctx.ALPH_NUM_STARTING_WITH_CAPITAL()),
                'fields': []
            })

    # Exit a parse tree produced by CapybaraParser#struct.
    def exitStruct(self, ctx: CapybaraParser.StructContext):
        print("exitStruct")

    # Enter a parse tree produced by CapybaraParser#field.
    def enterField(self, ctx: CapybaraParser.FieldContext):
        print("enterField ")
        name = ctx.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL()
        self.compile_unit['structs'][-1]['fields'].append(
            {
                'package_name': "".join(list(map(str, ctx.fullyQualifiedType().children))),
                'name': str(name) if name is not None else None
            })

    # Exit a parse tree produced by CapybaraParser#field.
    def exitField(self, ctx: CapybaraParser.FieldContext):
        print("exitField")

    # Enter a parse tree produced by CapybaraParser#def_.
    def enterDef_(self, ctx: CapybaraParser.Def_Context):
        print("enterDef_ " + str(ctx.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL()))
        self.compile_unit['functions'].append({
            'name': str(ctx.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL()),
            'parameters': [],
            'body': [],
            'return_type': "".join(list(map(str, ctx.fullyQualifiedType().children)))
        })

    # Exit a parse tree produced by CapybaraParser#def_.
    def exitDef_(self, ctx: CapybaraParser.Def_Context):
        print("exitDef_ " + str(ctx.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL()))

    # Enter a parse tree produced by CapybaraParser#listOfParameters.
    def enterListOfParameters(self, ctx: CapybaraParser.ListOfParametersContext):
        print("enterListOfParameters")
        self.compile_unit['functions'][-1]['parameters'].append({
            'name': None,
            'parameter_type': None
        })

    # Exit a parse tree produced by CapybaraParser#listOfParameters.
    def exitListOfParameters(self, ctx: CapybaraParser.ListOfParametersContext):
        print("exitListOfParameters")

    # Enter a parse tree produced by CapybaraParser#parameter.
    def enterParameter(self, ctx: CapybaraParser.ParameterContext):
        print("enterParameter")
        self.compile_unit['functions'][-1]['parameters'][-1]["name"] = str(
            ctx.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL())
        self.compile_unit['functions'][-1]['parameters'][-1]["parameter_type"] = "".join(
            list(map(str, ctx.fullyQualifiedType().children)))

    # Exit a parse tree produced by CapybaraParser#parameter.
    def exitParameter(self, ctx: CapybaraParser.ParameterContext):
        print("exitParameter")

    # Enter a parse tree produced by CapybaraParser#defBody.
    def enterDefBody(self, ctx: CapybaraParser.DefBodyContext):
        print("enterDefBody")
        self.compile_unit['functions'][-1]['body'].append({
            'assign_to': str(ctx.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL()),
            'expression': str(self._build_expression(ctx.expression()))
        })

    def _build_expression(self, expression):
        if expression is not None:
            if expression.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL() is not None:
                return expression.SMALL_ALPH_NUM_DIGITS_STARTING_WITH_SMALL()
            elif expression.expression(0) is not None:
                return expression.op.__dict__
            else:
                return None
        else:
            return None

    # Exit a parse tree produced by CapybaraParser#defBody.
    def exitDefBody(self, ctx: CapybaraParser.DefBodyContext):
        print("exitDefBody")

    # Enter a parse tree produced by CapybaraParser#fullyQualifiedType.
    def enterFullyQualifiedType(self, ctx: CapybaraParser.FullyQualifiedTypeContext):
        print("enterFullyQualifiedType")

    # Exit a parse tree produced by CapybaraParser#fullyQualifiedType.
    def exitFullyQualifiedType(self, ctx: CapybaraParser.FullyQualifiedTypeContext):
        print("exitFullyQualifiedType")


def hash(fname):
    hash_md5 = hashlib.md5()
    with open(fname, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            hash_md5.update(chunk)
    return hash_md5.hexdigest()


def parse(file):
    print("Parsing `" + file + "`")
    input_stream = FileStream(file)
    lexer = CapybaraLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = CapybaraParser(stream)
    tree = parser.compileUnit()

    walker = ParseTreeWalker()
    unit = {'file_hash': hash(file)}
    listener = CapybaraListenerImpl(unit)
    walker.walk(listener, tree)
    return unit
