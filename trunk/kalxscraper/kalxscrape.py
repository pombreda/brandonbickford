"""This is a scraper for the kalx playlist
current playlist url: http://kalx.berkeley.edu/last24hours.php

"""
from BeautifulSoup import BeautifulStoneSoup
import sys
import datetime
import re
import unittest
import time

def text(node): 
   t = ''
   for child in node.recursiveChildGenerator(): 
      if isinstance(child, unicode): 
         t += child
   return t

def match_date(row): 
   if row.strong != None:
      s = unicode(row.strong.string) 
      if re.match(ur'^(\d+/\d+/\d+)$', s):
         return datetime.datetime.strptime(s, '%m/%d/%Y').date()
   return None

def match_mike_break(node): 
   if node(text=re.compile('mic break', re.I)):
      return True
   return False

mike_break_ex = """><tr class="row_type_a"><td class="tall_row"
width="65"> 10:54 pm</td><td
class="tall_row">&gt;&gt;&gt;&gt;&gt;&gt;&gt; Mic Break
&lt;&lt;&lt;&lt;&lt;&lt;&lt;</td></tr>""" 

track_ex = """<tr class="row_type_b"><td class="tall_row"
width="65"> 10:50 pm</td><td class="tall_row"><strong>Corb
Lund</strong> - "Hurtin Albertan" - Hair In My Eyes Like A Highland
Steer (Stony Plain)</td></tr>"""

date_ex = """<tr><td width="65" colspan="2"><strong>08/28/2007</strong></td></tr>"""

def match_track(node): 
   time_node, track_node = node('td')
   the_time = get_time(time_node)
   try:
      _, artist, title_album = list(
         track_node.recursiveChildGenerator())    
   except ValueError, e:
      artist = ''
      try:
         _, title_album = list(
            track_node.recursiveChildGenerator())
      except ValueError, e:   
         print node
         return 

   remove_ws = lambda tok: re.sub('\s+', ' ', tok)
   artist = remove_ws(artist) 
   title_album = remove_ws(title_album) 
   m = re.match(r".*?\"(?P<title>.*?)\"(?: - (?P<album>.*?))?(?:\s*\((?P<label>.*?)\))?", title_album)
   if not m:
      return
   d = m.groupdict().copy()
   d['time'] = the_time
   d['artist'] = artist or u''
   d['title'] = d['title'] or u''
   d['label'] = d['label'] or u''
   return d

def get_time(node): 
   match_text = node(
      text = re.compile('.*?(\d{1,2}:\d{2} (?:am|pm)).*?'))
   if not match_text:
      return
   a_time_tuple = time.strptime(match_text[0].strip(), '%H:%M %p')
   t = datetime.time(*a_time_tuple[3:5])
   return t
       
class TestScrape(unittest.TestCase): 
   def test_date_match(self): 
      soup = BeautifulStoneSoup(date_ex)
      d = match_date(soup)
      assert d
      assert d.year == 2007
      assert d.month == 8
      assert d.day == 28

   def test_track_match(sefl): 
      soup = BeautifulStoneSoup(track_ex)
      assert match_track(soup)

   def test_mike_break_match(self): 
      soup = BeautifulStoneSoup(mike_break_ex) 
      assert match_mike_break(soup)


def parse(input):
   result = []
   soup = BeautifulStoneSoup(input)
   current_date = None
   for row in soup('tr'): 
      a_date = match_date(row) 
      if a_date:
         current_date = a_date
         print current_date
         continue
      
      if match_mike_break(row):
         #print "mike break", row
         continue

      m = match_track(row)
      if m:
         result.append(m) 
         m['date'] = datetime.datetime.combine(current_date,
            m['time'])
         del m['time']
         continue
   return result

if __name__ == "__main__": 
   import sys
   if len(sys.argv) == 2 and sys.argv[1] == 'runtests':
      unittest.main(argv=sys.argv[:1] + sys.argv[2:])
   else:
      result = parse(open('last.html'))
      print result


