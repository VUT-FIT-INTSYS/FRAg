import subprocess
import os
import time
import pathlib
import tempfile
from PyQt6.QtCore import QThread, pyqtSignal
from PyQt6.QtWidgets import QMainWindow, QTabWidget, QTextEdit, QProgressBar, QWidget, QHBoxLayout, QLabel

from compiler.agentspeak.compiler import compile_mas

class FRAgError(Exception):
    ...


class ProcessThread(QThread):
    """Thread to handle the execution of the Prolog process."""
    process_finished = pyqtSignal(int, str, str)  # Emit return code, stdout, and stderr
    error_occurred = pyqtSignal(str)
    execution_completed = pyqtSignal(bool)

    def __init__(self, command, working_dir):
        super().__init__()
        self.command = command
        self.working_dir = working_dir

    def run(self):
        try:
            result = subprocess.run(
                self.command,
                cwd=self.working_dir,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            self.process_finished.emit(result.returncode, result.stdout, result.stderr)
            self.execution_completed.emit(result.returncode == 0)
        except Exception as e:
            self.process_finished.emit(-1, "", str(e))
            self.execution_completed.emit(False)


class FileWatcher(QThread):
    """Thread to monitor a file for changes."""
    file_updated = pyqtSignal(str, str)  # Emit file path and new content

    def __init__(self, file_path):
        super().__init__()
        self.file_path = file_path
        self._running = True

    def run(self):
        last_size = 0
        while self._running:
            if os.path.exists(self.file_path):
                current_size = os.path.getsize(self.file_path)
                if current_size != last_size:
                    last_size = current_size
                    with open(self.file_path, "r", encoding="utf-8") as file:
                        content = file.read()
                    self.file_updated.emit(self.file_path.as_posix(), content)
            time.sleep(1)  # Polling interval

    def stop(self):
        self._running = False


class OutputWindow(QMainWindow):
    """Window to display output files in tabs."""

    window_closed = pyqtSignal()

    def __init__(self, output_files, temp_dir, process_thread: ProcessThread):
        super().__init__()
        self.setWindowTitle("Output Viewer")
        self.resize(800, 600)  # Initial size
        self.temp_dir = temp_dir

        self.process_thread = process_thread
        self.process_thread.execution_completed.connect(self.on_execution_completed)
        self.is_completed = False

        self.tab_widget = QTabWidget(self)
        self.setCentralWidget(self.tab_widget)

        self.watchers = []
        for file_path in output_files:
            tab = QTextEdit(self)
            tab.setReadOnly(True)
            self.tab_widget.addTab(tab, pathlib.Path(file_path).name)

            # Create and start a file watcher
            watcher = FileWatcher(file_path)
            watcher.file_updated.connect(self.update_tab_content)
            watcher.start()
            self.watchers.append(watcher)

        # Status bar setup
        self.status_bar = self.statusBar()

        # Create container widget
        status_widget = QWidget()
        status_layout = QHBoxLayout(status_widget)
        status_layout.setSpacing(10)  # Gap between elements
        status_layout.setContentsMargins(5, 0, 5, 0)  # Small margins

        # Status label
        self.status_label = QLabel("Running...")
        status_layout.addWidget(self.status_label)

        # Progress bar
        self.progress_bar = QProgressBar()
        self.progress_bar.setRange(0, 0)  # Endless animation
        self.progress_bar.setFixedWidth(100)
        self.progress_bar.setMinimumHeight(14) # Ensures minimum height
        status_layout.addWidget(self.progress_bar)

        status_layout.addStretch()

        self.status_bar.addWidget(status_widget)

    def on_execution_completed(self, success: bool):
        self.is_completed = True
        self.progress_bar.setRange(0, 1) # Stops animation
        self.progress_bar.setValue(1) # Sets full progress
        self.status_bar.showMessage("Completed" if success else "Failed")

    def closeEvent(self, event):
        """Clean up the temporary directory when the window is closed."""
        print(f"Cleaning up temporary directory: {self.temp_dir.name}")
        self.temp_dir.cleanup()  # Remove the temporary directory
        for watcher in self.watchers:
            watcher.stop()
        self.window_closed.emit()
        super().closeEvent(event)

    def update_tab_content(self, file_path, content):
        """Update the content of the corresponding tab."""
        for i in range(self.tab_widget.count()):
            if pathlib.Path(file_path).name == self.tab_widget.tabText(i):
                text_edit = self.tab_widget.widget(i)
                # Remember the scrollbar position
                scrollbar = text_edit.verticalScrollBar()
                current_position = scrollbar.value()

                text_edit.setPlainText(content)

                # Restore scrollbar position
                scrollbar.setValue(current_position)

class FragExecutor:
    def __init__(self, frag_path: pathlib.Path, swipl_path: str) -> None:
        self.frag_path = frag_path
        self.swipl_path = swipl_path

        # Initialize variables
        self.output_window = None
        self.process_thread = None

    def execute(self, active_config_path: str) -> OutputWindow:

        temp_dir = tempfile.TemporaryDirectory()
        dir_name = temp_dir.name

        def clear_temp_dir():
            print(f"Cleaning up temporary directory: {dir_name}")
            temp_dir.cleanup()

        def on_process_finished(returncode, stdout, stderr):
            if returncode == 0:
                print("FRAg process finished successfully.")
            else:
                print(f"FRAg process failed: {stderr}")
                process_thread.error_occurred.emit(f"FRAg process failed:\n{stderr}")

        try:
            try:
                mas2j_path = pathlib.Path(active_config_path)
                mas2fp_path, output_files = compile_mas(mas2j_path.as_posix(), dir_name)
            except SyntaxError:
                raise # Handled in the main window
            except Exception as e:
                raise FRAgError(f"Failed to compile MAS:\n{e}")

            mas2fp_path_without_extension = mas2fp_path.with_suffix('')
            command = [self.swipl_path, "-l", "FragPL.pl", "-g", f"frag('{mas2fp_path_without_extension.as_posix()}')",
                       "-g", "halt"]

            process_thread = ProcessThread(command, self.frag_path.as_posix())
            process_thread.process_finished.connect(on_process_finished)
            process_thread.start()

            # Open output window
            self.output_window = OutputWindow(output_files, temp_dir, process_thread)

            self.process_thread = process_thread

            return self.output_window
        except Exception as e:
            clear_temp_dir()
            raise e
