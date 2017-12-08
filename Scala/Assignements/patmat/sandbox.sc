



def if_test(num :Int) : Int = {
  if (num < 0) num * 2
  else {println("else clause"); num}
  //else num
  //num
}

if_test(4)
if_test(-2)

