'''
Description: Enum for network path element type
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

# Enum for type of NetworkPath
class PathType(object):
   NONE = 0
   CHECK = 1
   REFERENCE = 2
   INPUT = 3

# Enum for type of comparer
class FileType(object):
   NONE = 0
   ASCII = 1
   NEFIS = 2
   NETCDF = 3
   CONTENT = 4
   HIS = 5
   NUMBERTEXT = 6   
   DSERIESREGRESSION = 7
   DSERIESVERIFICATION = 8
   PITIMESERIES = 9
   CSVTIMESERIES = 10

# Enum for type of file presence
class PresenceType(object):
   NONE = 0
   PRESENT = 1
   ABSENT = 2


