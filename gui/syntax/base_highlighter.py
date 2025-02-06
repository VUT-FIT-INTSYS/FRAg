import re
from abc import abstractmethod

from PyQt6.QtGui import QSyntaxHighlighter, QTextCharFormat, QColor, QFont
from antlr4.InputStream import InputStream
from antlr4.error.ErrorListener import ErrorListener

def create_format(color, bold=False, italic=False, underline=False, underline_style=QTextCharFormat.UnderlineStyle.NoUnderline):
    text_format = QTextCharFormat()
    text_format.setForeground(QColor(color))
    if bold:
        text_format.setFontWeight(QFont.Weight.Bold)
    if italic:
        text_format.setFontItalic(italic)
    if underline:
        text_format.setUnderlineStyle(underline_style)
    return text_format


class CustomSyntaxError(Exception):
    def __init__(self, msg, line, column):
        super().__init__(msg)
        self.line = line
        self.column = column

class SyntaxErrorListener(ErrorListener):
    """Custom error listener to catch syntax errors during parsing."""
    def __init__(self, text_edit):
        self.text_edit = text_edit

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        error_msg = f"line {line}, column {column}: {msg}"
        raise CustomSyntaxError(error_msg, line, column)


class BaseSyntaxHighlighter(QSyntaxHighlighter):
    def __init__(self, document, error_callback=None):
        super().__init__(document)
        self.updating = False
        self.highlighting_rules = []
        self.error_callback = error_callback
        self.errors = []

        self.document().contentsChanged.connect(self.on_document_changed)

        # Formats for highlighting
        self.error_format = create_format("red", underline=True)

        # Base rules that should be applied first
        self.add_rule(r'"[^"]*"', create_format("magenta"))  # Strings
        self.add_rule(r'\b-?\d+(\.\d+)?\b', create_format("cyan"))  # Numbers
        self.add_rule(r"//.*", create_format("gray"))  # Single-line comments
        self.add_rule(r"/\*[\s\S]*?\*/", create_format("gray"))  # Multi-line comments

        self.add_language_specific_rules()

    def add_rule(self, pattern, format, group=0):
        """Add a new rule to the beginning of the rules list."""
        self.highlighting_rules.insert(0, (pattern, format, group))

    def add_language_specific_rules(self):
        """Hook method for derived classes to add their specific rules"""
        pass

    def highlightBlock(self, text):
        # Highlighting according to the rules
        for pattern, format, group in self.highlighting_rules:
            matches = re.finditer(pattern, text)
            for match in matches:
                start, end = match.span(group)
                self.setFormat(start, end - start, format)

        self.highlight_errors(text)

    def highlight_errors(self, text):
        if not self.errors:
            return

        block_start = self.currentBlock().position()
        block_end = block_start + len(text)

        for error in self.errors:
            error_line_start = self.document().findBlockByNumber(error["line"] - 1).position()
            error_pos = error_line_start + error["column"]

            if block_start <= error_pos < block_end:
                start = error_pos - block_start
                self.setFormat(start, 1, self.error_format)

    def validate_syntax(self):
        self.errors.clear()
        text = self.document().toPlainText()

        if not text:
            return
        try:
            input_stream = InputStream(text)
            parser = self.get_parser(input_stream)

            error_listener = SyntaxErrorListener(text)
            parser.removeErrorListeners()
            parser.addErrorListener(error_listener)

            parser.agent()
        except CustomSyntaxError as e:
            # self.errors.append(str(e))
            line, column = e.line, e.column
            self.errors.append({"line": line, "column": column, "message": str(e)})
            self.setFormat(0, len(text), self.error_format)
        except Exception as e:
            pass

    def on_document_changed(self):
        if self.updating:
            return

        self.updating = True
        # self.validate_syntax() TODO: fix later
        self.rehighlight()
        self.updating = False

    @abstractmethod
    def get_parser(self, input_stream):
        ...
