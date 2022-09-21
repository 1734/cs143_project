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
        {
            fun1();
            self.fun1();

            undefined_id.copy();
            undefined_id.undefined_fun();
            (new Object).undefined_fun();
            (new A).undefined_fun();

            helper.check_argument_number();
            helper.check_argument_number(1);
            helper.check_argument_number(1,2,3,4);

            helper.check_type(
                undefined_id.undefined_fun());
            helper.check_type(
                (new Object).undefined_fun());
            helper.check_type(
                (new A).undefined_fun());
            helper.check_type(
                helper.check_argument_number(1,2,3,4));
        }
    };
};
