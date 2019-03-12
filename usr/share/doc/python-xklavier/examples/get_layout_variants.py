#!/usr/bin/env python
#
# I, Sayamindu Dasgupta <sayamindu@laptop.org, am the original author of 
# this work.  I hereby donate it into the public domain, and relinquish any 
# rights I may have in it.

import xklavier
import gtk

def print_variant(c_reg, item):
    print ('\t %s (%s)' % (item.get_description(), item.get_name()))

def print_layout(c_reg, item):
    print ('\n%s (%s)' % (item.get_description(), item.get_name()))
    c_reg.foreach_layout_variant(item.get_name(), print_variant)
    
display = gtk.gdk.display_get_default()
engine = xklavier.Engine(display)
configreg = xklavier.ConfigRegistry(engine)

configreg.load(False)
configreg.foreach_layout(print_layout)
