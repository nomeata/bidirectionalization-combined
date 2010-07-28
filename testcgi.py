#!/usr/bin/python

from BaseHTTPServer import HTTPServer
from CGIHTTPServer import CGIHTTPRequestHandler
import sys

class MyRequestHandler(CGIHTTPRequestHandler):
	def is_cgi(self):
		self.cgi_info = ("","")
		return True

	def translate_path(self, path):
		return sys.argv[1]


if len(sys.argv) == 1:
    print "Usage: %s cgi-script" % sys.argv[0]
else:    
    server_address = ('', 8000)
    http  = HTTPServer(server_address, MyRequestHandler)
    print "Please connect to http://localhost:%d/" % server_address[1]
    http.serve_forever()
