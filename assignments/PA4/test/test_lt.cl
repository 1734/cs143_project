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
            x
            <
            x;

            helper.check_type(
                x
                <
                x
            );

            undefined_id1
            <
            undefined_id2;

            helper.check_type(
                undefined_id3
                <
                undefined_id4
            );

            1
            <
            x;

            x
            <
            2;

            }
    };
};
