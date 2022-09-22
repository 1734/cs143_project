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
            while undefined_id1
            loop undefined_id2
            pool;

            helper.check_type(
                while undefined_id3
                loop undefined_id4
                pool
            );

            while x
            loop undefined_id5
            pool;

            helper.check_type(
                while x
                loop undefined_id6
                pool
            );

            while 1
            loop undefined_id7
            pool;

            helper.check_type(
                while 1
                loop undefined_id8
                pool
            );
        }
    };
};
