'''
Description: Executes SVN commands
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import logging
from src.Config.ProgramConfig import ProgramConfig
from src.Suite.Program import Programs


# SVN wrapper, has handler interface
class SvnHandler(object):
    def __init__(self, autocommit=False):
        self.autocommit = autocommit

    def prepare_upload(self, frompath, topath, credentials):
        logging.debug("Preparing svn upload from %s to %s", frompath, topath)

        # Prepare svn and its configuration.
        pcnf = ProgramConfig()
        pcnf.setWorkingDirectory(frompath)
        prg = Programs().get("svn")

        # Use the mkdir command to make sure that the reference directory exists.
        arguments = self.__buildInitialArguments__(["mkdir", "--parents"], credentials)
        arguments.extend([topath])
        arguments.extend(['-m"Automated_creation_of_reference_directory_by_TestBench"'])
        pcnf.setArguments(arguments)
        pcnf.setIgnoreReturnValue(
            True)  # In case the folder already exists in SVN, a reference run has already been executed. Suppress warnings and error return value.
        pcnf.setIgnoreStandardError(True)
        prg.overwriteConfiguration(pcnf)
        prg.run()

        self.download(topath, frompath, credentials, "HEAD")

    # upload to svn
    # input: local path, svn path, credentials
    def upload(self, frompath, topath, credentials):
        logging.debug("Uploading from  %s to %s", frompath, topath)

        # Prepare svn and its configuration.
        pcnf = ProgramConfig()
        pcnf.setWorkingDirectory(frompath)
        prg = Programs().get("svn")

        # Add the new files
        logging.debug("Adding files...")
        arguments = self.__buildInitialArguments__(["add", "--force"], credentials)
        arguments.extend(["."])
        pcnf.setArguments(arguments)
        prg.overwriteConfiguration(pcnf)
        prg.run()

        if self.autocommit:
            logging.debug("Auto-committing changes...")
            arguments = self.__buildInitialArguments__(["commit"], credentials)
            arguments.extend(["."])
            arguments.extend(["-m\"Automated_commit_from_TestBench\""])
            pcnf.setArguments(arguments)
            prg.overwriteConfiguration(pcnf)
            prg.run()
        else:
            logging.info("Autocommit not selected; user needs to commit manually.")

    # download from svn
    # input: svn path, local path, credentials, version
    def download(self, frompath, topath, credentials, version):
        logging.debug("downloading from svn: %s", frompath)
        logging.debug("-                 to: %s", topath)
        arguments = None
        revision = "-r HEAD"
        if version and version != "":
            revision = "-r " + version
        svn_io = "export"
        if self.autocommit:
            svn_io = "checkout"

        # Even the "checkout" has the option "--force", to be sure that the files on teamcity are those from SVN
        arguments = self.__buildInitialArguments__([svn_io, "--force", revision], credentials)
        arguments.extend([frompath, "."])
        prg = Programs().get("svn")
        pcnf = ProgramConfig()
        pcnf.setWorkingDirectory(topath)
        pcnf.setArguments(arguments)
        prg.overwriteConfiguration(pcnf)
        prg.run()
        if prg.getError():
            raise RuntimeError("Errors during svn download: " + str(prg.getError()))

    # default svn arguments
    # input: initial command, credentials
    # output: argument string
    def __buildInitialArguments__(self, initial, credentials):
        arguments = initial
        arguments.extend(["--no-auth-cache", "--non-interactive", "--trust-server-cert"])
        if credentials:
            arguments.extend(["--username", credentials.getUsername(), "--password", credentials.getPassword()])
        return arguments
