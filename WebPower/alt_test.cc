#include <cstring>
#include "oui.h"
#include "nl.h"
#include "dasio/interface.h"
#include "dasio/appid.h"

using namespace DAS_IO;

class alt_test : public Interface {
  public:
    alt_test();
    bool test_not_alt(const char *kw1, const char *kw2, const char *context);
    bool test(const char *context, const char *input);
    void tests();
};

alt_test::alt_test()
    : Interface("alt_test", 80)
{}

bool alt_test::test_not_alt(const char *kw1, const char *kw2, const char *context) {
  int matched;
  if (not_alt(kw1, kw2, matched, context)) {
    if (matched == 0)
      msg(0, "%s: %s: non-match at end of buffer", iname, context);
    else
      msg(2, "%s: %s: non-match should have already been reported", iname, context);
    return true;
  } else {
    msg(0, "%s: %s: matched %d, cp:nc=%d:%d", iname, context, matched, cp, nc);
    if (matched == 0) return true;
  }
  return false;
}

bool alt_test::test(const char *context, const char *input) {
  msg(0, "%s: %s: Test string is '%s'", iname, context, input);
  strcpy((char*)buf, input);
  nc = strlen(input);
  cp = 0;
  return (
    test_not_alt("OK", "NOK", context) ||
    not_spaces() ||
    test_not_alt("true", "false", context) ||
    not_spaces() ||
    test_not_alt("true", "false", context) ||
    not_spaces() ||
    test_not_alt("true", "false", context) ||
    not_spaces() ||
    test_not_alt("true", "false", context)
  );
}

void alt_test::tests() {
  msg(0, "%s: Running tests", iname);
  test("test1", "OK false");
  test("test2", "OK fal");
  test("test3", "OK false true");
}

int main(int argc, char **argv) {
  oui_init_options(argc, argv);
  AppID.report_startup();
  alt_test *AT = new alt_test();
  AT->tests();
  AppID.report_shutdown();
}
