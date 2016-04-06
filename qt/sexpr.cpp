#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <iostream>
#include "sexpr.h"

using namespace std;

// trim from start
inline string &sexpr::ltrim(string &s) {
  s.erase(s.begin(), find_if(s.begin(), s.end(), not1(ptr_fun<int, int>(isspace))));
  return s;
}

// trim from end
inline string &sexpr::rtrim(string &s) {
  s.erase(find_if(s.rbegin(), s.rend(), not1(ptr_fun<int, int>(isspace))).base(), s.end());
  return s;
}

// trim from both ends
inline string &sexpr::trim(string &s) {
  return ltrim(rtrim(s));
}

vector<string> &sexpr::split(const string &s, char delim, vector<string> &elems) {
    stringstream ss(s);
    string item;
    while (getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}

// counts "(test (paren) list) blah blah"
//                           ^
size_t sexpr::count_paren(const string &str) {
  return count_paren(0,str);
}

size_t sexpr::count_paren(size_t start, const string &str) {
  unsigned int depth=0;
  for (size_t i=start; i<str.length(); i++) {
    if (str[i]=='(') depth++;
    if (str[i]==')') depth--;
    if (depth==0) return i;
  }
  return str.length();
}

vector<string> sexpr::tokenise(const string &str) {
  vector<string> ret;
  string current;

  for(size_t i=0; i<str.size(); ++i) {
    if (str[i]==' ') {
      if (current!="") {
        ret.push_back(current);
        current="";
      }
    } else {
      if (str[i]=='(') {
        size_t end = count_paren(i,str);
        //cerr<<"["<<str.substr(i,end-i)<<"]"<<endl;
        ret.push_back(str.substr(i,end));
        i=end;
      } else {
        current+=str[i];
      }
    }
  }
  if (current!="") ret.push_back(current);
  return ret;
}
