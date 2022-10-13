Class A1 inherits A3 {
    fun1() : String { "a1_1" };
    attr1 : String <- "a1_2";
};

Class A2 inherits A3 {
    fun2() : String { "a2_1" };
    attr2 : String <- "a2_2";
};

Class A3 {
    fun3() : String { "a3_1" };
    attr3 : String <- "a3_2";
};

Class Main inherits IO {
    main() : Int {
        {
            1;
        }
    };
};
