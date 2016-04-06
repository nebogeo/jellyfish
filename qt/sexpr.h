#ifndef SEXPR
#define SEXPR

#include <string>
#include <vector>

class sexpr {
 public:
  static inline std::string &ltrim(std::string &s);
  static inline std::string &rtrim(std::string &s);
  static inline std::string &trim(std::string &s);

  static std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems);

  // counts "(test (paren) list) blah blah"
  //                           ^
  static size_t count_paren(const std::string &str);
  static size_t count_paren(size_t start, const std::string &str);
  static std::vector<std::string> tokenise(const std::string &str);

};

#endif
