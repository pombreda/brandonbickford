#!env python 

from ctypes import *
import sys

taglib_dll = CDLL('libtag_c.' + ('dylib' if sys.platform == 'darwin' else 'so'))


file_new = taglib_dll.taglib_file_new
file_free = taglib_dll.taglib_file_free
file_tag = taglib_dll.taglib_file_tag

tag_title = taglib_dll.taglib_tag_title
tag_artist = taglib_dll.taglib_tag_artist
tag_album = taglib_dll.taglib_tag_album
tag_year = taglib_dll.taglib_tag_year
tag_trackno = taglib_dll.taglib_tag_track

tag_free_strings = taglib_dll.taglib_tag_free_strings

# oo iface
class FileRef(object): 
   def __init__(self, filename): 
      self.fileref = file_new(filename) 
     
   def __del__(self): 
      file_free(self.fileref)
      self.fileref = None

   def tag(self): 
      return Tag(self)
   
class Tag(object): 
   def __init__(self, fileref): 
      self.tagref = file_tag(fileref.fileref) 
      # hold onto a reference
      self.fileref = fileref

   def __title(self): 
      s = unicode(string_at(tag_title(self.tagref)))
      tag_free_strings() 
      return s

 
   def __artist(self): 
      s = unicode(string_at(tag_artist(self.tagref)))
      tag_free_strings() 
      return s

   def __year(self): 
      return int(tag_year(self.tagref))
     
   def __track(self): 
      return int(tag_trackno(self.tagref))

   def __album(self): 
      s = unicode(string_at(tag_album(self.tagref)))
      tag_free_strings() 
      return s


   track = property(__track)
   year = property(__year) 
   title = property(__title)
   artist = property(__artist) 
   album = property(__album) 

      
def file_to_tag(f): 
   return FileRef(f).tag()   

if __name__ == "__main__":
   import unittest

   class TestTagLibC(unittest.TestCase): 
      def test_tag(self): 
         t = file_to_tag('01.mp3') 
         self.assertEquals(t.artist, u'Magnolia Electric Company')
         self.assertEquals(t.album, u'Trials And Errors')
         self.assertEquals(t.track, 1)
         self.assertEquals(t.year, 2004)
         self.assertEquals(t.title, u'The Dark Don\'t Hide It')
   unittest.main() 


