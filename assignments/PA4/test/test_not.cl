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
            not
            x;

            helper.check_type(
                not
                x
            );

            not
            undefined_id1;

            helper.check_type(
                not
                undefined_id2
            );

            not
            1;

            not
            true;

            helper.check_type(
                not
                1
            );

            helper.check_type(
                not
                true
            );
        }
    };
};
