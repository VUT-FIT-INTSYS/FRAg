import os
import glob
import pathlib
import platform

from PyQt6.QtGui import QFileSystemModel, QKeySequence, QShortcut
from PyQt6.QtWidgets import QMainWindow, QTextEdit, QMessageBox
from PyQt6.QtCore import QDir, Qt

from .error_dialog import ErrorDialog
from .design import Ui_MainWindow  # Import the generated UI
from .syntax.asl.highlighter import ASLSyntaxHighlighter
from .syntax.mas2j.highlighter import MAS2JSyntaxHighlighter
from .logic.swipl_config import get_swipl_path
from .logic.frag_executor import FragExecutor, FRAgError

FRAG_PATH = pathlib.Path(__file__).parent.parent / "core"


def is_valid_configuration(folder_path):
    mas2j_files = glob.glob(os.path.join(folder_path, "*.mas2j"))
    return mas2j_files[0] if mas2j_files else None

class MainWindow(QMainWindow, Ui_MainWindow):
    def __init__(self):
        super().__init__()
        self.setupUi(self)

        # Dictionary to track open files
        self.open_files = {}

        # File model to display the file system
        self.file_model = None

        # Syntax highlighter
        self.highlighter = None

        # Path to a valid ASL configuration
        self.active_config_path = None

        # Initialize UI components
        self.initialize_tree_view()
        self.initialize_buttons()
        self.initialize_tabs()

        # Set initial status message
        self.set_invalid_config_status()

        # Get the path to SWI-Prolog
        self.swipl_path = get_swipl_path()

        # Initialize the FragExecutor
        self.frag_executor = FragExecutor(FRAG_PATH, self.swipl_path)

        # Output viewer
        self.output_viewer = None

    # Init methods
    def initialize_tree_view(self):
        # Create an instance of QFileSystemModel
        self.file_model = QFileSystemModel()
        self.treeView.setModel(self.file_model)

        # Retrieve the current working directory
        current_dir = os.getcwd()

        # Determine the root path based on the operating system
        if platform.system() == "Windows":
            # On Windows, split the drive letter from the current directory
            drive, _ = os.path.splitdrive(current_dir)
            # Construct the root path (e.g., "D:\")
            root_drive = drive + os.path.sep
        else:
            # On Unix-like systems (e.g., macOS), use the system root "/"
            root_drive = QDir.rootPath()  # Typically "/"

        # Set the root path for the file system model to the determined drive or root directory
        self.file_model.setRootPath(root_drive)

        # Set the tree view's root index to the determined root path
        self.treeView.setRootIndex(self.file_model.index(root_drive))

        # Set the current index to highlight the current working directory within the tree
        self.treeView.setCurrentIndex(self.file_model.index(current_dir))

        # Set filters
        self.file_model.setNameFilters(["*.asl", "*.mas2j"])
        self.file_model.setNameFilterDisables(False)

        # Horizontal slider setting
        self.treeView.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.treeView.header().setMinimumSectionSize(self.treeView.geometry().width())
        self.treeView.header().setDefaultSectionSize(self.treeView.geometry().width() * 4)
        self.treeView.header().setStretchLastSection(False)

        # Hide unnecessary columns
        self.treeView.setColumnHidden(1, True)  # Hide the "Size" column
        self.treeView.setColumnHidden(2, True)  # Hide the "Type" column
        self.treeView.setColumnHidden(3, True)  # Hide the "Date Modified" column

    def initialize_buttons(self):
        # Connect signals
        self.runButton.clicked.connect(self.on_run)
        self.saveButton.clicked.connect(self.save_current_tab)
        self.treeView.doubleClicked.connect(self.on_file_selected)

        # Add keyboard shortcut for save
        save_shortcut = QShortcut(QKeySequence.StandardKey.Save, self)
        save_shortcut.activated.connect(self.save_current_tab)

        # Disable buttons
        self.runButton.setEnabled(False)
        self.saveButton.setEnabled(False)

    def initialize_tabs(self):
        self.filesTab.clear()
        self.filesTab.setTabsClosable(True)
        self.filesTab.tabCloseRequested.connect(self.close_tab)
        self.filesTab.currentChanged.connect(self.update_run_button)

    # Signal handlers
    def on_run(self):
        self.runButton.setEnabled(False)
        try:
            self.output_viewer = self.frag_executor.execute(self.active_config_path)
            self.output_viewer.show()
            self.output_viewer.window_closed.connect(self.on_output_viewer_closed)
            self.frag_executor.process_thread.error_occurred.connect(self.handle_frag_error)
        except FRAgError as e:
            QMessageBox.critical(self, "Error", str(e))
            self.runButton.setEnabled(True)
        except SyntaxError as e:
            QMessageBox.critical(self, "Syntax Error", f"Syntax Error\n\n{str(e)}")
            self.runButton.setEnabled(True)

    def handle_frag_error(self, error_message):
        if self.output_viewer is not None:
            self.output_viewer.close()

        dialog = ErrorDialog("Error", error_message, self)
        dialog.exec()
        self.runButton.setEnabled(True)

    def on_output_viewer_closed(self):
        self.runButton.setEnabled(True)
        self.output_viewer = None

    def on_file_selected(self, index):
        if self.file_model.isDir(index):
            return

        file_path = self.file_model.filePath(index)
        file_name = os.path.basename(file_path)

        index = self.filesTab.count()
        text_edit = QTextEdit()

        file_extension = pathlib.Path(file_path).suffix
        # Select appropriate highlighter and validator
        if file_extension == ".asl":
            self.highlighter = ASLSyntaxHighlighter(text_edit.document())
        elif file_extension == ".mas2j":
            self.highlighter = MAS2JSyntaxHighlighter(text_edit.document())
        else:
            return # Unsupported file type

        # Handle syntax validation errors
        # if not valid:
        #    QMessageBox.warning(self, "Syntax Error", f"Error in {file_type} file:\n{error}")

        if file_path in self.open_files:
            self.filesTab.setCurrentIndex(self.open_files[file_path])  # Switch to the open tab
            return

        # Open the file in a new tab
        try:
            text_edit.setText(pathlib.Path(file_path).read_text(encoding="utf-8"))
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to open file:\n{file_path}\n\nError: {e}")
            return

        self.filesTab.addTab(text_edit, file_name)
        self.filesTab.setCurrentIndex(self.filesTab.count() - 1) # Switch to the new tab

        self.open_files[file_path] = self.filesTab.count() - 1

        # Connect to QTextEdit signal to track changes
        # text_edit.textChanged.connect(lambda: self.handle_text_change(self.filesTab.count() -1))
        text_edit.textChanged.connect(lambda idx=index: self.handle_text_change(idx))

        # Update the run button
        self.update_run_button()

    def close_tab(self, index):
        tab_text = self.filesTab.tabText(index)
        if tab_text.endswith("*"):
            # Ask user if they want to save changes
            reply = QMessageBox.question(
                self,
                "Save Changes",
                f"Do you want to save changes to '{tab_text.rstrip(' *')}'?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No | QMessageBox.StandardButton.Cancel,
            )
            if reply == QMessageBox.StandardButton.Yes:
                self.save_current_tab()
            elif reply == QMessageBox.StandardButton.Cancel:
                return  # Do not close the tab

        # Remove the file path from open_files
        for file_path, tab_index in list(self.open_files.items()):
            if tab_index == index:
                del self.open_files[file_path]
                break

        # Adjust indices in open_files
        for file_path, tab_index in self.open_files.items():
            if tab_index > index:
                self.open_files[file_path] -= 1

        widget = self.filesTab.widget(index)
        if widget: # Disconnect the signal
            widget.textChanged.disconnect()

        self.filesTab.removeTab(index)

        self.update_run_button()


    def save_current_tab(self):
        current_index = self.filesTab.currentIndex()
        if current_index == -1:
            return

        # Find the associated file path
        current_tab = self.filesTab.widget(current_index)
        for file_path, tab_index in self.open_files.items():
            if tab_index == current_index:
                break
        else:
            return  # No file path associated with the tab

        # Save content to file
        try:
            content = current_tab.toPlainText()
            pathlib.Path(file_path).write_text(content, encoding="utf-8")

            # Update the tab name
            tab_text = self.filesTab.tabText(current_index).rstrip(" *")
            self.filesTab.setTabText(current_index, tab_text)
            self.saveButton.setEnabled(False)
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to save file:\n{file_path}\n\nError: {e}")
            return

    def handle_text_change(self, index):
        self.mark_tab_as_dirty(index)

    # Helper methods
    def mark_tab_as_dirty(self, index):
        """Marks a tab as dirty (modified)."""
        tab_text = self.filesTab.tabText(index)
        if not tab_text.endswith("*"):
            self.filesTab.setTabText(index, tab_text + " *")
            self.saveButton.setEnabled(True)

    def update_run_button(self):
        current_index = self.filesTab.currentIndex()
        if current_index == -1:
            # No open tabs
            self.active_config_path = None
            self.runButton.setEnabled(False)
            self.set_invalid_config_status()
            return

        # Get the associated file path for the current tab
        for file_path, tab_index in self.open_files.items():
            if tab_index == current_index:
                current_file = file_path
                break
        else:
            self.active_config_path = None
            self.runButton.setEnabled(False)
            self.set_invalid_config_status()
            return

        # Determine the folder and check for .mas2j
        folder_path = os.path.dirname(current_file)
        if current_file.endswith(".mas2j"):
            self.active_config_path = current_file
        elif current_file.endswith(".asl"):
            self.active_config_path = is_valid_configuration(folder_path)
        else:
            self.active_config_path = None

        # Update the run button and label
        self.runButton.setEnabled(self.active_config_path is not None)
        if self.active_config_path:
            self.update_dynamic_config_label(self.active_config_path, "blue")
        else:
            self.set_invalid_config_status()

    def update_dynamic_config_label(self, text, color):
        self.configStatusLabel.setText(text)
        self.configStatusLabel.setStyleSheet(f"color: {color};")

    def set_invalid_config_status(self):
        self.update_dynamic_config_label("No valid configuration selected.", "orange")
