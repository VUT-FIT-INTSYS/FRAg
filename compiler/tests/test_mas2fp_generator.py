import unittest

from antlr4 import CommonTokenStream, FileStream, ParseTreeWalker

from compiler.agentspeak.mas2fp_generator import Mas2fpGenerator
from compiler.agentspeak.mas2j.MAS2JavaLexer import MAS2JavaLexer
from compiler.agentspeak.mas2j.MAS2JavaParser import MAS2JavaParser

from examples import get_example_file_path


def _compile(example_name: str) -> str:
    input_stream = FileStream(get_example_file_path(example_name, 'mas2j').as_posix())
    lexer = MAS2JavaLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = MAS2JavaParser(stream)
    tree = parser.mas()

    mas2f_generator = Mas2fpGenerator("<PATH>")
    walker = ParseTreeWalker()
    walker.walk(mas2f_generator, tree)

    return mas2f_generator.output


def _get_expected_output(example_name: str) -> str:
    return get_example_file_path(example_name, 'mas2fp').read_text()


def _get_example(example_name: str) -> tuple[str, str]:
    output = _compile(example_name)
    expected_output = _get_expected_output(example_name)

    return output, expected_output


_examples = ('do_it', 'call_loud', 'factorial', 'count', 'worker')


class TestMas2fpGenerator(unittest.TestCase):
    def test_examples(self):
        for example in _examples:
            output, expected_output = _get_example(example)

            self.assertEqual(output, expected_output)


if __name__ == '__main__':
    unittest.main()
