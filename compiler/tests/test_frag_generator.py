import unittest

from antlr4 import CommonTokenStream, FileStream, ParseTreeWalker

from compiler.agentspeak.frag_generator import FragGenerator
from compiler.agentspeak.asl.AgentSpeakLexer import AgentSpeakLexer
from compiler.agentspeak.asl.AgentSpeakParser import AgentSpeakParser

from .examples import get_example_file_path


def _compile(example_name: str, env_name: str | None) -> str:
    input_stream = FileStream(get_example_file_path(example_name, 'asl').as_posix())
    lexer = AgentSpeakLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = AgentSpeakParser(stream)
    tree = parser.agent()

    frag_generator = FragGenerator(env_name)
    walker = ParseTreeWalker()
    walker.walk(frag_generator, tree)

    return frag_generator.output


def _get_expected_output(example_name: str) -> str:
    return get_example_file_path(example_name, 'fap').read_text()


def _get_example(example_name: str, env_name: str | None) -> tuple[str, str]:
    output = _compile(example_name, env_name)
    expected_output = _get_expected_output(example_name)

    return output, expected_output


_examples = (('factorial', None), ('do_it', None), ('count', None), ('call_loud', None), ('worker', 'workshop'))

class TestFragGenerator(unittest.TestCase):
    def test_examples(self):
        for example_name, env_name in _examples:
            output, expected_output = _get_example(example_name, env_name)

            self.assertEqual(output, expected_output)


if __name__ == '__main__':
    unittest.main()
