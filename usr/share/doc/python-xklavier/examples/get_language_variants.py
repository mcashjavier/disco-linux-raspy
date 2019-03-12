#!/usr/bin/env python
#
# I, Sayamindu Dasgupta <sayamindu@laptop.org, am the original author of 
# this work.  I hereby donate it into the public domain, and relinquish any 
# rights I may have in it.

import xklavier
import gtk

def print_variant(c_reg, item, subitem):
    print ('\t %s (%s)' % (item.get_description(), item.get_name()))
    if subitem:
        print ('\t\t %s (%s)' % (subitem.get_description(), subitem.get_name()))

def print_language(c_reg, item):
    print ('\n%s (%s)' % (item.get_description(), item.get_name()))
    c_reg.foreach_language_variant(item.get_name(), print_variant)
    
display = gtk.gdk.display_get_default()
engine = xklavier.Engine(display)
configreg = xklavier.ConfigRegistry(engine)

configreg.load(False)
configreg.foreach_language(print_language)
