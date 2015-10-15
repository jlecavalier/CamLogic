A, A=>B, B=>C, C=>E, E=>F, F=>G, G=>D |= D;
A, (A&B)|(A&C) |= C|B;
A|B, A=>C, B=>C |= C;
A=>(B&~B) |= ~A;