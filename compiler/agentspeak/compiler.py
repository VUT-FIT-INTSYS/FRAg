import os
import sys
from pathlib import Path

from antlr4 import CommonTokenStream, FileStream, ParseTreeWalker
from antlr4.error.ErrorListener import ErrorListener

from .asl.AgentSpeakLexer import AgentSpeakLexer
from .asl.AgentSpeakParser import AgentSpeakParser
from .mas2j.MAS2JavaLexer import MAS2JavaLexer
from .mas2j.MAS2JavaParser import MAS2JavaParser
from .frag_generator import FragGenerator
from .mas2fp_generator import Mas2fpGenerator, Agent

class StrictErrorListener(ErrorListener):
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        raise SyntaxError(f"Parse error at line {line}:{column} - {msg}")

# TODO: more agents
def _compile_mas_file(path: Path, output_dir: Path, strict_parsing : bool) -> tuple[str, str | None, list[Agent]]:
    input_stream = FileStream(path.as_posix())
    lexer = MAS2JavaLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = MAS2JavaParser(stream)

    if strict_parsing:
        parser.removeErrorListeners()
        error_listener = StrictErrorListener()
        parser.addErrorListener(error_listener)

    tree = parser.mas()

    mas2f_generator = Mas2fpGenerator(output_dir.as_posix())
    walker = ParseTreeWalker()
    walker.walk(mas2f_generator, tree)

    return mas2f_generator.output, mas2f_generator.env_name, mas2f_generator.agents


def _compile_asl_file(path: Path, env_name: str | None, strict_parsing: bool) -> str:
    input_stream = FileStream(path.as_posix())
    lexer = AgentSpeakLexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = AgentSpeakParser(stream)

    if strict_parsing:
        parser.removeErrorListeners()
        error_listener = StrictErrorListener()
        parser.addErrorListener(error_listener)

    tree = parser.agent()

    frag_generator = FragGenerator(env_name)
    walker = ParseTreeWalker()
    walker.walk(frag_generator, tree)

    return frag_generator.output


def compile_mas(mas_file: str, output_dir: str, strict_parsing: bool = True) -> tuple[Path, list[Path]]:
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    mas_path = Path(mas_file)
    output_dir_path = Path(output_dir)

    source_dir = mas_path.parent.resolve()

    mas_compiled, mas_env_name, agents_info = _compile_mas_file(mas_path, output_dir_path, strict_parsing)

    mas_file_name = mas_path.name.replace("mas2j", "mas2fp")
    mas2fp_file = output_dir_path / mas_file_name
    with mas2fp_file.open("w") as f:
        f.write(mas_compiled)

    program_name = mas2fp_file.name.replace(".mas2fp", "")
    output_files = []

    for agent_info in agents_info:
        agent_compiled = _compile_asl_file((source_dir / agent_info.filename.replace("fap", "asl")), mas_env_name, strict_parsing)
        agent_file_name = Path(agent_info.filename).name
        with (output_dir_path / agent_file_name).open("w") as f:
            f.write(agent_compiled)

        if agent_info.count == 1:
            output_files.append(output_dir_path / f"{program_name}_{agent_info.name}.out")
        else:
            for i in range(1, agent_info.count + 1):
                output_files.append(output_dir_path / f"{program_name}_{agent_info.name}{i}.out")

    return mas2fp_file, output_files


if __name__ == "__main__":
    compile_mas(sys.argv[1], sys.argv[2])
