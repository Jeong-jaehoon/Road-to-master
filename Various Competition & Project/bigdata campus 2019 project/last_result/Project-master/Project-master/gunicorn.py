# -*- coding: utf-8 -*-

import os
import multiprocessing

bind = "127.0.0.1:5000"
# workers = multiprocessing.cpu_count() * 2 + 1   # workers=1
workers = 1
threads = 1
# worker_class = "eventlet"   #pip install gunicorn[eventlet] 설치
reload = True
loglevel = "error"
preload_app = True
# worker_class = "sync"
# worker_connections = 1000
# timeout = 60*60*2
errorlog = os.path.join('/root/logs/', 'server.log')