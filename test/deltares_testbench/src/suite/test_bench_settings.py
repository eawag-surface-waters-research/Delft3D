"""
Description: TestBenchSettings data class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import sys
from typing import List, Optional

from src.config.local_paths import LocalPaths
from src.config.program_config import ProgramConfig
from src.config.test_case_config import TestCaseConfig
from src.config.types.mode_type import ModeType
from src.utils.common import log_separator, log_sub_header
from src.utils.logging.log_level import LogLevel
from src.utils.logging.logger import Logger


class TestBenchSettings:
    """Settings for a test bench run"""

    log_level: LogLevel = LogLevel.INFO
    local_paths: Optional[LocalPaths] = None
    programs: List[ProgramConfig] = []
    configs: List[TestCaseConfig] = []
    run_mode: ModeType = ModeType.COMPARE
    config_file: str
    user_name: str
    filter: str
    autocommit: bool = False
    only_post: bool = False
    teamcity: bool = False
    test_bench_root: Optional[str] = None
    test_bench_script_name: Optional[str] = None
    test_bench_startup_dir: Optional[str] = None

    def log_overview(self, logger: Logger):
        """Logs overview of the parameters

        Args:
            logger (Logger): logger to log to
        """
        log_sub_header("Parsed arguments", logger)

        logger.info(f"Version  : {sys.version}")
        logger.info(f"Mode     : {self.run_mode}")
        logger.info(f"Config   : {self.config_file}")
        logger.info(f"Filter   : {self.filter}")
        logger.info(f"LogLevel : {str(self.log_level)}")
        logger.info(f"Username : {self.user_name}")

        log_separator(logger, char="-", with_new_line=True)
