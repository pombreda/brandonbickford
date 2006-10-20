#!env python
"""Manager to let a receiver listen to messages about an interface by a sender.

A little simpler than zope.interface.adapter.AdapterRegistry
"""
import itertools
from zope.interface import Interface, implements
import types

class InterfaceError(ValueError): 
	pass

# marker type
Any = types.ClassType('Any', (object, ), {})() 

class SubscriptionManager(object):
	"I manage interface->sender->listeners"

	def __init__(self): 
		self.__subscriptions = {}
 
	def subscribe(self, interface, receiver, sender = Any):
		"subscribe receiver with interface and optional sender"
		if not interface.providedBy(receiver):
			raise InterfaceError(receiver)

		subscribers = self.__subscriptions.setdefault((interface, sender), [])
		if receiver not in subscribers: 
			subscribers.append(receiver) 

	def subscribers(self, interface, sender = Any):
		"iterate over the subscribers for an interface and optional sender"
		def _subscribers():
			iter1 = iter(self.__subscriptions.get((interface, sender), ()))
			if sender != Any:
				return itertools.chain(iter1, self.__subscriptions.get((interface, Any), ())) 
			else: 
				return iter1
			
		_seen = set() 
		def seen(x): 
			if x not in _seen: 
				_seen.add(x)
				return True
			return False
	
		return itertools.ifilter(seen, _subscribers())

__all__ = ['SubscriptionManager', 'Any']

if __name__ == "__main__":
	import unittest
	class TestSubscription(unittest.TestCase):
		def test_subscription(self):
			"only implementing interfaces are allowed to be receivers"
			x = SubscriptionManager()
			class A(Interface): pass

			class B(object):
				implements(A)

			class C(object):
				pass

			x.subscribe(A, B())
			
			self.assertRaises(InterfaceError, x.subscribe, A, C())

			"test membership"	
			x = SubscriptionManager()
			z = B() 
			x.subscribe(A, z)
			self.assert_(z in x.subscribers(A))
			y = object()
			x.subscribe(A, z, y)
			self.assertEquals(list(x.subscribers(A, y)), [z])
 	
	unittest.main() 	
