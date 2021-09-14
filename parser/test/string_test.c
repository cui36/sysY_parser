string s1 = "This is a game about guessing a number between 0 to 100.\n";
string s2 = "Please input a number: ";
string s3 = "The number you guess is bigger.\n";
string s4 = "The number you guess is smaller.\n";
string s5 = "You win ! ! !\n";
string s6 = "Please input an integer between 0 to 100.\n";

int main()
{
  int ans = 62;
  int a = 0;

  printstr(&s1);
  while( a != ans ){
    printstr(&s2);
    readint(&a);

    if( a < 0  ){
      printstr(&s6);
      continue;
    }
    if( a > 100  ){
      printstr(&s6);
      continue;
    }

    if( a == ans ){
      printstr(&s5);
      break;
    }
    
    if( a > ans ){
      printstr(&s3);
      continue;
    }

    if( a < ans ){
      printstr(&s4);
      continue;
    }

  }
}
