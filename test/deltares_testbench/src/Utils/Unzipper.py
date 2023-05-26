'''
Description: file unzipper
-----------------------------------------------------
Copyright (C)  Stichting Deltares, 2013
'''

import os
import zipfile
import fnmatch
import logging

# compare files on ASCII content equality
class Unzipper(object):
   
   # unzip file to path
   # input: full path to zip file name
   def __unzip__(self, zfp):
      fh = open(zfp, "rb")
      z = zipfile.ZipFile(fh)
      z.extractall(os.path.dirname(zfp))
      fh.close()
   
   # unzip all zip files in directory (recursive) 
   # input: path
   def recursive(self, path):
      matches = []
      for dirpath,_,filenames in os.walk(path):
         for f in fnmatch.filter(filenames, "*.zip"):
            matches.append(os.path.abspath(os.path.join(dirpath, f)))
      for m in matches:
         logging.debug("unzipping %s", m)
         self.__unzip__(m)
