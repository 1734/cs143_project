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
            ~
            x;

            helper.check_type(
                ~
                x
            );

            ~
            undefined_id1;

            helper.check_type(
                ~
                undefined_id2
            );

            ~
            1;

            ~
            true;

            helper.check_type(
                ~
                1
            );
            }
    };
};
