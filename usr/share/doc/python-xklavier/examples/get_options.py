#!/usr/bin/env python
#
# I, Sayamindu Dasgupta <sayamindu@laptop.org, am the original author of 
# this work.  I hereby donate it into the public domain, and relinquish any 
# rights I may have in it.

import xklavier
import gtk

def print_option(c_reg, item):
    print ('\t %s (%s)' % (item.get_description(), item.get_name()))

def print_option_group(c_reg, item):
    print ('\n%s (%s)' % (item.get_description(), item.get_name()))
    c_reg.foreach_option(item.get_name(), print_option)
    
display = gtk.gdk.display_get_default()
engine = xklavier.Engine(display)
configreg = xklavier.ConfigRegistry(engine)

configreg.load(False)
configreg.foreach_option_group(print_option_group)
