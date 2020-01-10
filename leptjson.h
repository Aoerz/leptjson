#ifndef LEPTJSON_H__
#define LEPTJSON_H__

typedef enum {
  LEPT_NULL,
  LEPT_FALSE,
  LEPT_TRUE,
  LEPT_NUMBER,
  LEPT_STRING,
  LEPT_ARRAY,
  LEPT_OBJECT
} lept_type;

enum {
  LEPT_PARSE_OK = 0,
  LEPT_PARSE_EXPECT_VALUE,
  LEPT_PARSE_INVALID_VALUE,
  LEPT_PARSE_ROOT_NOT_SINGULAR
};

typedef struct {
  lept_type type;
  double n;
} lept_value;

int lept_parse(lept_value *v, const char *json);

lept_type lept_get_type(const lept_value *v);

double lept_get_number(const lept_value *v);
#endif