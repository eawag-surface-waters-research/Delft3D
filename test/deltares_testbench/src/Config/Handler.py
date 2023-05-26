'''
Description: Handler type
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

# Enum for Handler used in HandlerFactory
class Handler(object):
   NONE = 0
   WEB = 1
   SVN = 2
   FTP = 3
   NET = 4
   PATH = 5