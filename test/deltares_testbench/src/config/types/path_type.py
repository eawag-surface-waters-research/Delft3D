"""
Description: Enum for network path element type
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""

from enum import Enum


class PathType(Enum):
    """Type of NetworkPath"""

    NONE = 0
    CHECK = 1
    REFERENCE = 2
    INPUT = 3
