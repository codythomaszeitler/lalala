import sys
import json


def gen_format_string(type):
    return "(" + type['name'] + gen_format_body(type['formats']) + ")"


def gen_format_body(formats):
    def gen_key_value(format):
        return format['keyName'] + "=" + format['format']
    body = "@;<1 2>{@["
    for i in range(len(formats)):
        param = formats[i]
        if i == (len(formats) - 1):
            body += gen_key_value(param)
        else:
            body += gen_key_value(param) + ",@;<1 2>"
    body += "@]}"
    return body


def main():
    with open(sys.argv[1]) as f:
        j = json.load(f)

    for type in j['types']:
        print ('------------------')
        print (type['name'])
        print(gen_format_string(type))
        print ('------------------')


main()
