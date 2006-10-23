#!env python
## requires zope.interface (it's packaged separately from z0pe)

from ConfigParser import ConfigParser

from zope.interface import Interface, implements
from zope.interface.adapter import AdapterRegistry
import imp
import re

class AlreadyRegistered(Exception): 
	pass

class AnyNotifier(object): pass
AnyNotifier = AnyNotifier() 

class ComponentMap(dict): 
	def __init__(self, *args): 
		self.registry = AdapterRegistry()
		self.all_components = set() 
		dict.__init__(self, *args)

	def __setitem__(self, k, v):
		raise "Unsupported"
	
	def register(self, cls, *args, **kwargs):
		if cls in self:
			raise AlreadyRegistered, cls
	
		instance = cls(*args, **kwargs)	
		instance.set_enabled(True)
		instance.set_component_map(self)
		dict.__setitem__(self, cls, instance)

	def __delitem__(self, k): 
		v = self[k]
		v.set_component_map(None)
		v.set_enabled(False) 
		super(ComponentMap, self).__delitem__(cls)

	def subscribe(self, listener, listenerinterface, notifier):
		self.registry.subscribe([listenerinterface], notifier, self[listener])
	
	def subscriptions(self, notifier, listenerinterface):		
		return self.registry.subscriptions([listenerinterface], notifier)
	 

def loadComponentMap(afile): 
	def _import(pathname, modulename, classname):
		def __import(name):
			mod = __import__(name)
			components = name.split('.')
			for comp in components[1:]:
				mod = getattr(mod, comp)
			return mod

		if pathname:
			module = __import(pathname+'.'+modulename)
		else:
			module = __import(modulename)
		return getattr(module, classname)

	def findclass(classpath):
		isfqclass(classpath) 
		ipos = classpath.rindex('.')
		modulepath = classpath[:ipos]
		classname = classpath[ipos+1:]
		try: 
			ipos = modulepath.rindex('.')
			path = modulepath[:ipos]
			module = modulepath[ipos + 1:]
		except ValueError:
			path = None
			module = modulepath
		cls = _import(path, module, classname)
		return cls

	def isfqclass(moduleclass): 
		if not re.match(r'^(\w+\.)+\w+$', moduleclass): 
			raise ValueError(moduleclass)

	def parse_subscriptions(atom): 
		for notifier_interfaces_atom in atom.split():
			notifier, interfaces_atom = notifier_interfaces_atom.split(':')
			if notifier == 'any':
				notifiercls = AnyNotifier
			else:
				notifiercls = findclass(notifier)
			
			interfacescls = map(findclass, interfaces_atom.split(','))
			x = notifiercls, interfacescls		
			yield x

	componentmap = ComponentMap()
	parser = ConfigParser() 
	parser.readfp(afile)

	"Register Components"
	for section in parser.sections():
		cls = findclass(section)
		items = parser.items(section)
		componentmap.register(cls, **dict((k,v) for k,v in items if k != 'subscriptions'))

	"Load interfaces"
	for section in parser.sections(): 
		if not parser.has_option(section, 'subscriptions'):
			continue
	
		listener = findclass(section)
		for notifier, interfaces in parse_subscriptions(parser.get(section, 'subscriptions')):
			for listenerinterface in interfaces:
				componentmap.subscribe(listener, listenerinterface, notifier)

	return componentmap


class Component(object):
	def __init__(self): 
		self.__componentmap = None
		self.__enabled = False

	def set_component_map(self, component_map):
		"Set the ComponentMap" 
		self.__componentmap = component_map
	
	def component_map(self):
		"Return the ComponentMap" 
		return self.__componentmap

	def set_enabled(self, enabled):
		"Enable or disable this component." 
		self.__enabled = bool(enabled)
	
	def enabled(self): 
		"Is this component enabled?"
		return self.__enabled

	def subscribers(self, interface): 
		return self.component_map().subscriptions(self.__class__, interface)

if __name__ == "__main__":
	import unittest
	from StringIO import StringIO
	class AComponent(Component): pass
	class BComponent(Component): pass

	class TestComponentMap(unittest.TestCase): 
		def test_load_components(self):
			x = ComponentMap()

			class C(Component): pass

			x.register(C)
			x[C]

			class NamedComponent(Component): 
				def __init__(self, name): 
					self.__name = name
					super(NamedComponent, self).__init__()	

				def name(self): return self.__name
 
			x.register(NamedComponent, name="hello")

			self.assertEquals(x[NamedComponent].name(), "hello") 
 
		def test_already_added(self): 
			x = ComponentMap() 

			class A(Component): pass
			x.register(A)
			x[A]
			self.assertRaises(AlreadyRegistered, x.register, A)

		def test_load(self):
			config = "[__main__.Component]\n[__main__.AComponent]"
			cmap = loadComponentMap(StringIO(config))
 			assert isinstance(cmap, ComponentMap)
			self.assertEquals(set(cmap.keys()), set((Component, AComponent)))

	unittest.main()  


