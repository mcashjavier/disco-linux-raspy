#!/usr/bin/env python
#
# I, Sayamindu Dasgupta <sayamindu@laptop.org, am the original author of 
# this work.  I hereby donate it into the public domain, and relinquish any 
# rights I may have in it.

import xklavier
import gtk

display = gtk.gdk.display_get_default()
engine = xklavier.Engine(display)

crec = xklavier.ConfigRec()
crec.get_from_server(engine)

print "Layouts:"
for layout in crec.get_layouts():
    print "\t%s" % layout

print "Variants:"
for variant in crec.get_variants():
    print "\t%s" % variant

print "Options:"
for option in crec.get_options():
    print "\t%s" % option

print "Model:"
print "\t%s" % crec.get_model()

engine.start_listen(xklavier.XKLL_TRACK_KEYBOARD_STATE)
state = engine.get_current_state()
engine.stop_listen()

active_group = state['group']
print "Active group:"
print "\t%s(%s)" % (crec.get_layouts()[active_group], \
    crec.get_variants()[active_group])
