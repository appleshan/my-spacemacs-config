import os

path = './'

for prefix, dirs, files in os.walk(path):
    for name in files:
        if name.endswith('.elc'):
            filename = os.path.join(prefix, name)
            os.remove(filename)
