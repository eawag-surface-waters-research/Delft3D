from abc import ABC, abstractmethod

from src.config.credentials import Credentials


class IHandler(ABC):
    @abstractmethod
    def prepare_upload(
        self, from_path: str, to_path: str, credentials: Credentials
    ) -> None:
        """Prepares upload

        Args:
            from_path (str): original path
            to_path (str): destination path
            credentials (Credentials): credentials needed for connection

        Raises:
            NotImplementedError: this method could be invalid for some handlers
        """

    @abstractmethod
    def upload(self, from_path: str, to_path: str, credentials: Credentials) -> None:
        """Upload file using the specified from and to path

        Args:
            from_path (str): original path
            to_path (str): destination path
            credentials (Credentials): credentials needed for connection

        Raises:
            NotImplementedError: this method could be invalid for some handlers
        """

    @abstractmethod
    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: str
    ):
        """Download a file from the specified location

        Args:
            from_path (str): original path
            to_path (str): destination path
            credentials (Credentials): credentials needed for connection
            version (str): version to use

        Raises:
            NotImplementedError: this method could be invalid for some handlers
        """
