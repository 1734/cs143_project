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
            fun1();
            helper.check_type(x);
            (new Nochild)@SELF_TYPE.undefined_fun(1,1);
            x@Undefined_class.undefined_fun(1,1,1,1,1);
            x@Nochild.undefined_fun(1,1,1,1,1);
            x@Object.undefined_fun(1,1,1,1,1);
            x@Object.copy(1,1,1,1,1);
            (new Object)@Nochild.undefined_fun(1,1,1,1,1);
            (new Object)@Object.undefined_fun(1,1,1,1,1);
            (new HelperChild)@Helper.check_argument_number(1,1,1,1,1);
            (new HelperChild)@Helper.check_type(1);

        }
    };
};

Class AA inherits A {
    fun2() : Int {
        {
            self@A.fun1();
            self@AA.fun1();
            self@A.fun2();
            self@AA.fun2();
        }
    };
};
