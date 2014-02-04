#!/bin/zsh

python -c "exec('try:import SimpleHTTPServer as m\nexcept:import http.server as m');m.test(HandlerClass=m.SimpleHTTPRequestHandler)"

