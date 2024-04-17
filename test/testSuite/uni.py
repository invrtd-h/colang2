import os

template = '''let test = "{}"'''

def transform(s: str) -> str:
    data = ["{" + hex(ord(c)).replace('0x', '') + "}" for c in s]
    data = [f"\\u{c}" for c in data]
    code = "".join(data)
    return template.format(code)

def main():
    files = os.listdir('./test/testSuite')
    for file in files:
        file = './test/testSuite/' + file
        if not file.endswith('.mte'):
            continue
        with open(file, 'r') as f:
            s = f.read()
            code = transform(s)
        with open(file.replace(".mte", ".ml"), 'w') as f:
            f.write(code)


if __name__ == '__main__':
    main()