from component import Component, loadComponentMap
from zope.interface import Interface, implements

class IServlet(Interface): 
	def execute(self): pass

class IResponder(Interface): 
	pass

class Homepage(Component): 
	implements(IServlet)
		
	def execute(self): 
		print "hi!"

class MainResponder(Component): 
	"the main app responder"
	implements(IResponder)
	def run(self): 
		for subscriber in self.subscribers(IServlet): 
			subscriber.execute()


mapper = loadComponentMap(file('app.config', 'r'))
mapper[MainResponder].run()

