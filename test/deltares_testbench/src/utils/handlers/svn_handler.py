"""
Description: Executes SVN commands
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
"""

import logging

from src.config.credentials import Credentials
from src.config.program_config import ProgramConfig
from src.suite.program import Programs
from src.utils.handlers.i_handler import IHandler


# SVN wrapper, has handler interface
class SvnHandler(IHandler):
    def __init__(self, autocommit=False):
        self.autocommit = autocommit

    def prepare_upload(
        self, from_path: str, to_path: str, credentials: Credentials
    ) -> None:
        logging.debug("Preparing svn upload from %s to %s", from_path, to_path)

        # Prepare svn and its configuration.
        pcnf = ProgramConfig()
        pcnf.working_directory = from_path
        prg = Programs().get("svn")

        # Use the mkdir command to make sure that the reference directory exists.
        arguments = self.__buildInitialArguments__(["mkdir", "--parents"], credentials)
        arguments.extend([to_path])
        arguments.extend(['-m"Automated_creation_of_reference_directory_by_TestBench"'])
        pcnf.arguments = arguments
        pcnf.ignore_return_value = True  # In case the folder already exists in SVN, a reference run has already been executed. Suppress warnings and error return value.
        pcnf.ignore_standard_error = True
        prg.overwriteConfiguration(pcnf)
        prg.run()

        self.download(to_path, from_path, credentials, "HEAD")

    # upload to svn
    # input: local path, svn path, credentials
    def upload(self, from_path: str, to_path: str, credentials: Credentials) -> None:
        logging.debug("Uploading from  %s to %s", from_path, to_path)

        # Prepare svn and its configuration.
        pcnf = ProgramConfig()
        pcnf.working_directory = from_path
        prg = Programs().get("svn")

        # Add the new files
        logging.debug("Adding files...")
        arguments = self.__buildInitialArguments__(["add", "--force"], credentials)
        arguments.extend(["."])
        pcnf.arguments = arguments
        prg.overwriteConfiguration(pcnf)
        prg.run()

        if self.autocommit:
            logging.debug("Auto-committing changes...")
            arguments = self.__buildInitialArguments__(["commit"], credentials)
            arguments.extend(["."])
            arguments.extend(['-m"Automated_commit_from_TestBench"'])
            pcnf.arguments = arguments
            prg.overwriteConfiguration(pcnf)
            prg.run()
        else:
            logging.info("Autocommit not selected; user needs to commit manually.")

    # download from svn
    # input: svn path, local path, credentials, version
    def download(
        self, from_path: str, to_path: str, credentials: Credentials, version: str
    ):
        logging.debug("downloading from svn: %s", from_path)
        logging.debug("-                 to: %s", to_path)
        arguments = None
        revision = "-r HEAD"
        if version and version != "":
            revision = "-r " + version
            logging.debug(f"-                 revison: {version}")

        svn_io = "export"
        if self.autocommit:
            svn_io = "checkout"

        # Even the "checkout" has the option "--force", to be sure that the files on teamcity are those from SVN
        arguments = self.__buildInitialArguments__(
            [svn_io, "--force", revision], credentials
        )
        arguments.extend([from_path, "."])
        prg = Programs().get("svn")
        pcnf = ProgramConfig()
        pcnf.working_directory = to_path
        pcnf.arguments = arguments
        prg.overwriteConfiguration(pcnf)
        prg.run()
        if prg.getError():
            raise RuntimeError("Errors during svn download: " + str(prg.getError()))

    # default svn arguments
    # input: initial command, credentials
    # output: argument string
    def __buildInitialArguments__(self, initial, credentials: Credentials):
        arguments = initial
        arguments.extend(
            ["--no-auth-cache", "--non-interactive", "--trust-server-cert"]
        )
        if credentials:
            arguments.extend(
                ["--username", credentials.username, "--password", credentials.password]
            )
        return arguments
