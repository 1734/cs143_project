#!/usr/bin/bash
diff <(./lexer $1 | ./parser | ../../bin/semant 2>&1) <(./mysemant $1 2>&1)
