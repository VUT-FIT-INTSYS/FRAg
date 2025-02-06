import json
import pathlib
import subprocess
from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QLineEdit, QPushButton, QFileDialog, QMessageBox

CONFIG_FILE = "config.json"

def load_config():
    config_path = pathlib.Path(CONFIG_FILE)
    if config_path.exists():
        return json.loads(config_path.read_text())
    return {}

def save_config(config):
    with open(CONFIG_FILE, "w") as file:
        file.write(json.dumps(config, indent=4))


def get_swipl_path():
    """Check and retrieve SWI-Prolog path."""
    config = load_config()

    if "swipl_path" in config:
        swipl_path = pathlib.Path(config["swipl_path"])
        if swipl_path.exists():
            return swipl_path.as_posix()

    # If the path is not in config.json or is invalid, ask the user
    swipl_path = ask_for_swipl_path()
    config["swipl_path"] = swipl_path
    save_config(config)
    return swipl_path


def ask_for_swipl_path():
    """Prompt user for the path to SWI-Prolog with validation after confirmation."""

    # Create a dialog window
    dialog = QDialog()
    dialog.setWindowTitle("Configure SWI-Prolog Path")
    layout = QVBoxLayout(dialog)

    label = QLabel("Enter the path to SWI-Prolog executable:")
    layout.addWidget(label)

    path_input = QLineEdit()
    layout.addWidget(path_input)

    select_button = QPushButton("Browse")
    layout.addWidget(select_button)

    confirm_button = QPushButton("OK")
    layout.addWidget(confirm_button)

    def browse_path():
        file_path, _ = QFileDialog.getOpenFileName(
            dialog,
            "Select SWI-Prolog Executable",
            "",
            "SWI-Prolog (swipl)"
        )
        if file_path:
            path_input.setText(file_path)

    def validate_and_accept():
        path = path_input.text().strip()
        if not path:
            QMessageBox.critical(dialog, "Error", "Path to SWI-Prolog cannot be empty!")
            return

        try:
            # Execute `swipl --version` to verify the validity of the path
            result = subprocess.run(
                [path, "--version"],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            if result.returncode == 0:
                dialog.accept()
            else:
                QMessageBox.critical(dialog, "Error", "Invalid SWI-Prolog executable!")
        except Exception:
            QMessageBox.critical(dialog, "Error", "Failed to validate SWI-Prolog path!")

    # Connect signals
    select_button.clicked.connect(browse_path)  # Open file dialog on button click
    confirm_button.clicked.connect(validate_and_accept)  # Validate path on "OK" click

    # Show the dialog and wait for user interaction
    if dialog.exec() == QDialog.DialogCode.Accepted:
        return path_input.text().strip()
    else:
        QMessageBox.critical(None, "Error", "Path to SWI-Prolog is required!")
        raise RuntimeError("Path to SWI-Prolog not provided.")
