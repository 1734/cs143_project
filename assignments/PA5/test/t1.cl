Class A {
    i1 : Int <- i1+1;
    i2 : Int;
    i3 : Bool;
    i4_1 : String <- "bbbbb";
    i4 : String <- "aaaaa";
    i5 : B;
    i6 : Bool <- i3;
    fun_1(i : Int, j : Int, k : Int, l : Int, m : Int) : Int {
        {
            (*
            i + j + k + l + m;
            1111;
            2222;
            3333;
            4444;
            5555;
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((new SELF_TYPE).fun_2(11111, 22222) - m))))))))))));
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + (l - m))))))))))));
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((l<-m) - m))))))))))));
            (l<-m) - 99999;
            let tmp1:Int in (((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((tmp1<-m) - m)))))))))))));
            6666;
            7777;
            fun_2(1234567+0, 7654321+0);
            (new B)@B.fun_b(1234567+0, 7654321+0);
            (new B).fun_b(1234567+0, 7654321+0);
            8888;
            9999;
            *)
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((let tmp1: Int in 111111) - m))))))))))));
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((let tmp1: Bool in 222222) - m))))))))))));
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((let tmp1: String in 333333) - m))))))))))));
            ((999-0)-123) + (888 + (777 + (666 + (555 + (444 + (333 + (222 + (111 + (i + (j + (k + ((let tmp1: B in 444444) - m))))))))))));
        }
    };
    fun_2(x1 : Int, x2 : Int) : Int {
        {
            --case 1 of id1 : String => 123; id2 : Int => 234; id3 : A => 345; esac;
            --(12345+54321) - (123456+654321);
            --(x1 <- 1234567) + 7654321;
            let tmp1 : Int in ((tmp1 <- 1234567) + 7654321);
            if (new B).fun_b_1() then 111111 else 222222 fi;
            let tmp1 : Int in 11111;
            let tmp1 : Bool in 22222;
            let tmp1 : String in 33333;
            let tmp1 : B in 44444;
        }
    };
};

Class C {
    i4 : String <- "aaaaa111";
    i4_1 : String <- "eeeee";
};

Class Main inherits IO {
    main() : Int {
        1
    };
};

Class B inherits A {
    i_b : Int <- i1+2;
    i_1 : String <- "ccccc";
    fun_b(x1 : Int, x2 : Int) : Int {
        {
            let tmp1 : Int <- (1234+0) + 4321, tmp2 : Int <- 12345 + 54321 in tmp1 + tmp2 - i_b;
        }
    };
    fun_b_1() : Bool {
        {
            true;
        }
    };
    fun_b_test_new() : Int {
        {
            new SELF_TYPE;
            new B;
            1;
        }
    };
    fun_b_test_loop() : Int {
        {
            while fun_b_1() loop 111 pool;
            1;
        }
    };
    fun_b_test_case(): Int {
        {
            case 
            1 
            of 
            id5 : Object => id5;
            id4 : IO => id4;
            id1 : String => id1; 
            id2 : Int => id2;
            id3 : A => id3;
            esac;
            1;
        }
    };
    fun_b_test_string(): Int {
        {
            "aaa";
            "bbb";
            1;
        }
    };
    fun_b_test_mul(): Int {
        {
            i1 * i_b;
            1;
        }
    };
    fun_b_test_div(): Int {
        {
            i1 / i_b;
            1;
        }
    };
    fun_b_test_neg(): Int {
        {
            ~i1 + 1;
            1;
        }
    };
    fun_b_use_bool(b : Bool) : Int { 1 };
    fun_b_test_not_and_lt(): Int {
        {
            not (i1 < i_b) ;
            --not (not (i1 < i_b)) ;
            --fun_b_use_bool(i1 < i_b);
            1;
        }
    };
    fun_b_test_eq(): Int {
        {
            i1 = i_b;
            true = false;
            "aaa1" = "aaa2";
            new B = new B;
            (new B).fun_b_1() = (new B).fun_b_1();
            let a1:A, a2:A in a1 = a2;
            1;
        }
    };
    fun_b_test_leq(): Int {
        {
            (i1 <= i_b) ;
            1;
        }
    };
    fun_b_test_isvoid(): Int {
        {
            isvoid(i1) ;
            1;
        }
    };
};
