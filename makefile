update: src/fe/parser.messages
	menhir --update-errors src/fe/parser.messages src/fe/parser.mly > src/fe/parser.messages.new && cat src/fe/parser.messages.new > src/fe/parser.messages && rm src/fe/parser.messages.new

build_test: 
	python3 ./test/testSuite/uni.py

new_msg: src/fe/parser.mly
	menhir --list-errors src/fe/parser.mly > src/fe/parser.messages