"""
Description: TestRunSettings data class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

import logging
from typing import List, Optional

from src.config.local_paths import LocalPaths
from src.config.program_config import ProgramConfig
from src.config.test_case_config import TestCaseConfig
from src.config.types.mode_type import ModeType


class TestRunSettings:
    """Settings for a test bench run"""

    log_level: int = logging.INFO
    local_paths: Optional[LocalPaths] = None
    programs: List[ProgramConfig] = []
    configs: List[TestCaseConfig] = []
    run_mode: ModeType = ModeType.COMPARE
    autocommit: bool = False
    only_post: bool = False
    teamcity: bool = False
