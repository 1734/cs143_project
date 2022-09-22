Class Nochild {};

Class Good1 {};

Class Good2 {};

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
    good_id1_1 : Good1;
    good_id1_2 : Good1;
    good_id1_3 : Good1;
    good_id1_4 : Good1;
    good_id2_1 : Good2;
    good_id2_2 : Good2;
    good_id2_3 : Good2;
    good_id2_4 : Good2;
    fun1() : Int {
        {
            x
            =
            x;

            helper.check_type(
                x
                =
                x
            );

            undefined_id1
            =
            undefined_id2;

            helper.check_type(
                undefined_id3
                =
                undefined_id4
            );

            good_id1_1
            =
            good_id1_2;

            good_id1_3
            =
            good_id2_1;

            1
            =
            x;

            x
            =
            2;

            1
            =
            new String;

            true
            =
            "abc";

            }
    };
};
