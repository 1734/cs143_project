#!/usr/bin/bash
diff <(../../bin/coolc $1 2>&1) <(./mysemant $1 2>&1)
