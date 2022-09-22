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
            if x
            then x
            else x
            fi;

            helper.check_type(
                if x
                then x
                else x
                fi
            );

            if undefined_id1
            then 1
            else true
            fi;

            helper.check_type(
                if undefined_id2
                then 1
                else true
                fi
            );

            helper.check_type(
                if undefined_id3
                then 1
                else x
                fi
            );

            helper.check_type(
                if undefined_id4
                then x
                else true
                fi
            );
        }
    };
};
