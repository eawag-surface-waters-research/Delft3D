"""
Description: Parameter Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2023
"""


class Parameter:
    """Parameter storage object"""

    def __init__(self):
        self.__name = ""
        self.__location = None
        self.__tolerance_absolute = None
        self.__tolerance_relative = None
        self.__ignore = False

    @property
    def name(self) -> str:
        """Parameter name"""
        return self.__name

    @name.setter
    def name(self, value: str):
        self.__name = value

    @property
    def location(self):
        """Location of the parameter"""
        return self.__location

    @location.setter
    def location(self, value):
        self.__location = value

    @property
    def tolerance_absolute(self):
        return self.__tolerance_absolute

    @tolerance_absolute.setter
    def tolerance_absolute(self, value):
        self.__tolerance_absolute = value

    @property
    def tolerance_relative(self):
        return self.__tolerance_relative

    @tolerance_relative.setter
    def tolerance_relative(self, value):
        self.__tolerance_relative = value

    @property
    def ignore(self) -> bool:
        return self.__ignore

    @ignore.setter
    def ignore(self, value: bool):
        self.__ignore = value

    def set_tolerance(self, value: float):
        """Sets both absolute and relative tolerance to value"""
        self.__tolerance_absolute = value
        self.__tolerance_relative = value
