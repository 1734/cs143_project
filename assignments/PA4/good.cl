class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
		new SELF_TYPE;
           }
	};
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};
