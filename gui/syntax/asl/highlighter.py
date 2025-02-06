from antlr4.CommonTokenStream import CommonTokenStream

from compiler.agentspeak.asl.AgentSpeakLexer import AgentSpeakLexer
from compiler.agentspeak.asl.AgentSpeakParser import AgentSpeakParser

from ..base_highlighter import BaseSyntaxHighlighter, create_format


class ASLSyntaxHighlighter(BaseSyntaxHighlighter):
    logical_operators = ["true", "false"]
    control_structures = ["for"]
    agent_actions = [".print", ".println", ".my_name"]

    def __init__(self, document, error_callback=None):
        super().__init__(document, error_callback)

    def add_language_specific_rules(self):
        logical_operators_format = create_format("blue", bold=True)
        control_structures_format = create_format("blue", bold=True)
        agent_actions_format = create_format("darkorange")
        predicate_format = create_format("green")  # for predicate names
        variable_format = create_format("purple")  # for variables
        goal_format = create_format("darkblue")  # for goal names

        # Not
        self.add_rule(r"\bnot(?=\s*\(|\s*&)", create_format("black"))

        # Variables - capture variables in different contexts
        self.add_rule(
            r'\(([A-Z_][a-zA-Z0-9_]*)\)',  # single variable
            variable_format,
            1  # use capture group 1
        )
        self.add_rule(
            r',\s*([A-Z_][a-zA-Z0-9_]*)',  # variable after comma
            variable_format,
            1  # use capture group 1
        )
        self.add_rule(
            r'\(\s*([A-Z_][a-zA-Z0-9_]*)\s*,',  # first variable in list
            variable_format,
            1  # use capture group 1
        )

        # Achievement goals (!goal)
        self.add_rule(
            r'!([a-zA-Z_][a-zA-Z0-9_]*)',  # goal name
            goal_format
        )

        # Test goals (?goal)
        self.add_rule(
            r'\?([a-zA-Z_][a-zA-Z0-9_]*)',  # goal name
            goal_format
        )

        # Triggered achievement goals (+!goal)
        self.add_rule(
            r'\+!([a-zA-Z_][a-zA-Z0-9_]*)',  # goal name
            goal_format
        )

        # Base keywords and actions
        for op in self.logical_operators:
            self.add_rule(rf"\b{op}\b", logical_operators_format)

        for struct in self.control_structures:
            self.add_rule(rf"\b{struct}\b", control_structures_format)

        for action in self.agent_actions:
            # We need a special rule for actions starting with a period
            if action.startswith('.'):
                action_without_dot = action[1:]
                self.add_rule(rf"\.{action_without_dot}\b", agent_actions_format)
            else:
                action_escaped = action.replace(".", r"\.")
                self.add_rule(rf"\b{action_escaped}\b", agent_actions_format)

        # Variables (terms)
        self.add_rule(
            r',\s*([a-z][a-zA-Z0-9_]*)',  # term after any comma
            variable_format,
            1
        )
        self.add_rule(
            r'[\(\[]\s*([a-z][a-zA-Z0-9_]*)',  # first term after ( or [
            variable_format,
            1
        )

        # Predicates in belief base - capture only the name
        self.add_rule(
            r'\b([a-zA-Z_][a-zA-Z0-9_]*)(?=\s*\()',  # predicate name, without parenthesis
            predicate_format
        )

        # Plan triggers (+belief)
        self.add_rule(
            r'\+([a-zA-Z_][a-zA-Z0-9_]*)(?=\s*\()',  # +predicate, without parenthesis
            predicate_format
        )

    def get_parser(self, input_stream):
        lexer = AgentSpeakLexer(input_stream)
        token_stream = CommonTokenStream(lexer)
        parser = AgentSpeakParser(token_stream)
        return parser
