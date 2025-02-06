import sys
from pathlib import Path

from .agentspeak.compiler import compile_mas


def main(mas_file, target_dir):
    target_dir_path = Path(target_dir)
    target_dir_path.mkdir(parents=True, exist_ok=True)
    compile_mas(mas_file, target_dir)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
