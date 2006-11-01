
class Match(object): 
	def __init__(self): 
		self.__attributes = {} 

	def attributes(self): 
		return self.__attributes

class rule(object): 
	def match(self, X): 
		"returns a Match() or None"
		abstract

class ruledispatch(object): 
	def __init__(self): 
		self.__items = []

	def add(self, rule, predicate): 
		self.__items.append((rule, predicate))

	def match(self, subject): 
		for (rule, predicate) in self.__items: 
			match = rule.match(subject)
			if match: return (match, predicate)

if __name__ == "__main__":
	import unittest
	import re

	class RuleDispatchTestCase(unittest.TestCase): 
		def test(self): 
			rd = ruledispatch() 
			class regexrule(rule):
				def __init__(self, pattern): 
					self._pattern = pattern 

				def match(self, x): 
					m = re.match(self._pattern, x)
					if m:
						match = Match() 
						match.attributes().update(
							{
							'groups': m.groups(), 
							'groupdict': m.groupdict()
							})
						return match

			rd.add(regexrule('foo'), 1)
			rd.add(regexrule('blah'), 2)
			rd.add(regexrule('eek'), 3)

			self.assertEquals(rd.match('foo')[1], 1)

	unittest.main()


						

					 
