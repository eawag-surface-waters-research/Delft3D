from typing import Dict, List


class DictTable:
    def __init__(self, values: Dict[str, List[str]]) -> None:
        self.__values_dict = values
        self.__column_values = [r for r in self.__values_dict.values()]

    @property
    def headers(self) -> List[str]:
        return list(self.__values_dict.keys())

    def max_column_width(self, key: str) -> int:
        default_width = len(key)
        max_value_width = max([len(str(v)) for v in self.__values_dict[key]])

        return max(default_width, max_value_width)

    def row_values(self, row_index: int) -> List:
        return [cv[row_index] for cv in self.__column_values]

    def number_of_rows(self) -> int:
        first_key = next(enumerate(self.__values_dict.keys()))[1]
        return len(self.__values_dict[first_key])
