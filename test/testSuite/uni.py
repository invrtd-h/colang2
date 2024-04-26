import os

template = '''let test = "{}"'''
template_numbered = '''let test{} = "{}"'''

def transform(s: str) -> str:
    data = ["{" + hex(ord(c)).replace('0x', '') + "}" for c in s]
    data = [f"\\u{c}" for c in data]
    code = "".join(data)
    return template.format(code)

def transform_idx(i: int, s: str) -> str:
    data = ["{" + hex(ord(c)).replace('0x', '') + "}" for c in s]
    data = [f"\\u{c}" for c in data]
    code = "".join(data)
    return template_numbered.format(i, code)

def main():
    files = os.listdir('./test/testSuite')
    for file in files:
        file = './test/testSuite/' + file
        if not file.endswith('.mte'):
            continue
        with open(file, 'r') as f:
            s = f.read()
            ls = s.split('#####')
            n = len(ls)
            codes = [transform_idx(i, ls[i]) for i in range(n)]
        with open(file.replace(".mte", ".ml"), 'w') as f:
            for code in codes:
                f.write(code)
                f.write('\n')
            test = "let test = [|" + ";".join(["test{}".format(i) for i in range(n)]) + "|]"
            f.write(test)
            f.write('\n')


if __name__ == '__main__':
    main()