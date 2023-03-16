BIN_NAME=flp22-fun

TEST_DIR=./test/
TEST_SCRIPT=test.sh
TEST_SCRIPT_ARGS=-i -b -o

SRC_FILES=./src/*.hs
OBJECT_FILES=./src/*.o
INTERFACE_FILES=./src/*.hi

${BIN_NAME}: ${SRC_FILES}
	ghc -Wall ${SRC_FILES} -o ${BIN_NAME}

tests: ${BIN_NAME}
	chmod +x ${TEST_DIR}${TEST_SCRIPT}
	cd ${TEST_DIR} && ./${TEST_SCRIPT} ${TEST_SCRIPT_ARGS}

clean:
	rm -f ${OBJECT_FILES}
	rm -f ${INTERFACE_FILES}
	rm -f flp22-fun
