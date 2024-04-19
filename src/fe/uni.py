def transform(s: str) -> str:
    data = ["{" + hex(ord(c)).replace('0x', '') + "}" for c in s]
    data = [f"\\u{c}" for c in data]
    code = "".join(data)
    return code

kwds = [
    "유링게슝한",
    "안유링게슝",
    "춘잣",
    
    "아니세상에",
    "자기가",
    "라는",
    "이라는",
    "사람인데",
    "을",
    "를",
    "했대",
    
    "아",
    "야",
    "먹어라",
    
    "스킵이야",
    "스키비야",
    "유리계수",
    "조이고",
    "뭉탱이",
    
    "인",
    "중에는",
    "아무리",
    "라도",
    "이라도",
    "할",
    "수가",
    "없단다",
]

if __name__ == '__main__':
    ret = {kwd: transform(kwd) for kwd in kwds}
    for kwd in ret:
        print(kwd, ret[kwd])
