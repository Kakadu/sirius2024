import requests
import hardcoded

url = 'http://localhost:8040/eval'

x = requests.post(url, json = hardcoded.fac)

print(x.text)


# Sending a request from terminal:
#   curl --json '[ { kind: "IMPORT", value: "Std" }, { kind: "EXTERN", value: "Lwrite" }, { kind: "CONST", value: 42 }, { kind: "CALL", fname: "Lwrite", argc: 1, "flg": false } ]' http://localhost:8040/eval
#  Respones
#    Success:
#    42

