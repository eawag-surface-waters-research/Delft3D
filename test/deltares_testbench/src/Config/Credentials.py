'''
Description: Credentials Data Class
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

# (network) credentials
class Credentials(object):

   def __init__(self):
      self.__name = ""
      self.__username = ""
      self.__password = ""
   
   def getName(self):
      return self.__name
   def setName(self, value):
      self.__name = value
   
   def getUsername(self):
      return self.__username
   def setUsername(self, value):
      self.__username = value

   def getPassword(self):
      return self.__password
   def setPassword(self, value):
      self.__password = value
