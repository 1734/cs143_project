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
    fun1() : Int {
        {
            helper.check_type(
                isvoid
                x
            );
            helper.check_type(
                isvoid
                undefined_id1
            );
        }
    };
};
