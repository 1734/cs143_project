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
            let x0 : 
                Undefined_class0
                    <- undefined_id in
                        undefined_id0;

            let x1 : 
                Undefined_class1
                    <- x in
                        undefined_id1;

            let x2 : 
                Nochild
                    <- (new Object) in
                        undefined_id2;

            let x3 : 
                Nochild
                    <- (new SELF_TYPE) in
                        undefined_id3;

            let
                self : 
                Undefined_class0
                    <- undefined_id5 in
                        undefined_id6;

            let self : 
                Nochild
                    <- (new Object) in
                        undefined_id7;

            let self : 
                Nochild
                    <- (new SELF_TYPE) in
                        undefined_id4;

            helper.check_type(
                let self : 
                    Nochild
                        <- (new Object) in
                            undefined_id8
            );

            helper.check_type(
                let self : 
                    Nochild
                        <- (new Object) in
                            true
            );

            let x9 : 
                Undefined_class9
                    in undefined_id9;

            let x10 : 
                Undefined_class10
                    in true;
        }
    };
};
