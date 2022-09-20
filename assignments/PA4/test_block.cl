Class Nochild {};

Class Helper {
    check_type(x : Nochild) : Int {
        1
    };
    check_argument_number(x1 : Int, x2 : Int, x3 : Int) : Int {
        1
    };
};

Class A {
    helper : Helper;
    fun1() : Int1 {
        helper.check_type(
            {
                1;
            }
        )
    };
};
