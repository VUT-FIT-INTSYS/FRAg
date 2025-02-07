import sys
from pathlib import Path

from .agentspeak.compiler import compile_mas


def main(mas_file, target_dir):
    # Convert both paths to absolute
    mas_file_path = Path(mas_file).resolve()
    target_dir_path = Path(target_dir).resolve()

    # Create target directory
    target_dir_path.mkdir(parents=True, exist_ok=True)

    # Convert to string paths
    compile_mas(mas_file_path.as_posix(), target_dir_path.as_posix())


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
