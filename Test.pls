PLATYPUS{
 a=-000;
 b=+0.;
 c$ = "prog" # "ram" # " is for C";
 READ(c);
 READ(d,e,f);
 c=((d+e)/a)*f-(((d-e)*a)/f);
 WHILE TRUE(a<>b .OR. c==d .AND. e<f .OR. a>0)REPEAT{
   IF TRUE(a==1 .AND. b==0.0)THEN{
    c=-(5.9);
   }ELSE {c=-c;};
 };
 WRITE(c$);
 WRITE("Results: ");
 WRITE(d,e,f,a);
}