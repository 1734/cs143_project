Class Nochild {};

Class Helper {
    check_type(x : Nochild) : Int {
        1
    };
    check_argument_number(x1 : Int, x2 : Int, x3 : Int) : Int {
        1
    };
};

Class HelperChild inherits Helper {};

Class A {
    helper : Helper;
    x : Undefined_class;
    int1 : Int;
    fun1() : Nochild {
        {
            undefined_id1 <- 1;

            undefined_id2 <- new Object;

            x <- 1;

            undefined_id3 <- undefined_id4;

            int1 <- new Nochild;

            undefined_id4 <- new Nochild;

            self <- new SELF_TYPE;

            self <- new Nochild;

            self <- undefined_id5;
        }
    };
};
