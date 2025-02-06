from antlr4.CommonTokenStream import CommonTokenStream

from compiler.agentspeak.mas2j.MAS2JavaLexer import MAS2JavaLexer
from compiler.agentspeak.mas2j.MAS2JavaParser import MAS2JavaParser

from ..base_highlighter import BaseSyntaxHighlighter, create_format

class MAS2JSyntaxHighlighter(BaseSyntaxHighlighter):
    mas_sections = ["agents", "environment", "infrastructure"]
    platform_types = ["Centralised"]

    def __init__(self, document, error_callback=None):
        super().__init__(document, error_callback)

    def add_language_specific_rules(self):
        # Add MAS-specific highlighting

        self.add_rule(r"\b[a-zA-Z_][a-zA-Z0-9_]*\.asl\b", create_format("orange")) # ASL file
        self.add_rule(r"\bMAS", create_format("green", bold=True))  # MAS keyword

        # For an agent on the same line as 'agents:'
        agent_format = create_format("green")
        self.add_rule(
            r"(?<=agents:)[\s\n]+[a-zA-Z_][a-zA-Z0-9_]*",
            agent_format
        )
        # For the agent on a new line (starts with white characters and ends with either .asl, [ or ;)
        self.add_rule(
            r"^\s+([a-zA-Z_][a-zA-Z0-9_]*)(?=\s+(?:\w+\.asl|[\[\(]|\s*;))",
            agent_format
        )

        self.add_rule(r"#(?=\s*\d+)", create_format("cyan")) # '#'

        sections_format = create_format("blue", bold=True)
        platform_format = create_format("purple", bold=True)

        for section in self.mas_sections:
            self.add_rule(rf"(?:^|\n)\s*{section}\b", sections_format)

        for platform in self.platform_types:
            self.add_rule(rf"\b{platform}\b", platform_format)

    def get_parser(self, input_stream):
        lexer = MAS2JavaLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = MAS2JavaParser(token_stream)
        return parser

