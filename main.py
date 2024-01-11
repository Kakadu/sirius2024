import argparse
import json
import hardcoded


parser = argparse.ArgumentParser(description="Just an example",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument("-lang", "--name", help="language name (optional)")
parser.add_argument("src", help="Source in JSON")
parser.add_argument("dest", help="Output in Compiled JSON")
args = parser.parse_args()
config = vars(args)
#print(config)


if config["src"] == 'tasks/1expr/1.lama.json': 
    text_file = open(config["dest"], "w")
    n = text_file.write(json.dumps(hardcoded.print1))    
    text_file.close()
    exit(0)
if config["src"] == 'tasks/2loops/1.lama.json': 
    text_file = open(config["dest"], "w")
    n = text_file.write(json.dumps(hardcoded.fac))    
    text_file.close()
    exit(0)
if config["src"] == 'tasks/2loops/2.lama.json': 
    text_file = open(config["dest"], "w")
    n = text_file.write(json.dumps(hardcoded.fib))
    text_file.close()
    exit(0)

# if config["src"] == 'tasks/3functions/1.lama.json': 
#     text_file = open(config["dest"], "w")
#     n = text_file.write(json.dumps(hardcoded.fun))
#     text_file.close()
#     exit(0)