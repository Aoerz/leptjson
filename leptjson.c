#include "leptjson.h"
#include <assert.h>
#include <math.h>
#include <stdint.h> //int64_t
#include <stdlib.h>
#include <string.h>

#define EXPECT(c, ch)                                                          \
  do {                                                                         \
    assert(*c->json == (ch));                                                  \
    c->json++;                                                                 \
  } while (0)

#define ISDIGIT(ch) ((ch) >= '0' && (ch) <= '9')

#define ISDIGIT1TO9(ch) ((ch) >= '1' && (ch) <= '9')

#define STRING_ERROR(ret)                                                      \
  do {                                                                         \
    c->top = head;                                                             \
    return ret;                                                                \
  } while (0)

#ifndef LEPT_PARSE_STACK_INIT_SIZE
#define LEPT_PARSE_STACK_INIT_SIZE 256

#endif

typedef struct {
  const char *json;
  char *stack;
  size_t size, top;
} lept_context;

static int lept_parse_array(lept_context *c, lept_value *v);

#define PUTC(c, ch)                                                            \
  do {                                                                         \
    *(char *)lept_context_push(c, sizeof(char)) = (ch);                        \
  } while (0)

static void *lept_context_push(lept_context *c, size_t size) {
  void *ret;
  assert(size > 0);
  if (c->top + size >= c->size) {
    if (c->size == 0)
      c->size = LEPT_PARSE_STACK_INIT_SIZE;
    while (c->top + size >= c->size)
      c->size += c->size >> 1; // 1.5
    c->stack = (char *)realloc(c->stack, c->size);
  }
  ret = c->stack + c->top;
  c->top += size;
  return ret;
}

static void *lept_context_pop(lept_context *c, size_t size) {
  assert(c->top >= size);
  return c->stack + (c->top -= size);
}

// whitespace: ' ', '\n', '\t'
static void lept_parse_whitespace(lept_context *c) {
  const char *p = c->json;
  while (*p == ' ' || *p == '\n' || *p == '\r') {
    p++;
  }
  c->json = p;
}

// parse true/false/null
static int lept_parse_literal(lept_context *c, lept_value *v,
                              const char *literal, lept_type type) {
  size_t i;
  EXPECT(c, literal[0]);
  for (i = 0; literal[i + 1]; i++) {
    if (c->json[i] != literal[i + 1])
      return LEPT_PARSE_INVALID_VALUE;
  }
  c->json += i;
  v->type = type;
  return LEPT_PARSE_OK;
}

static int lept_parse_number(lept_context *c, lept_value *v) {
  // char *end;
  // TODO:validate number
  // int negative = 0;
  // int64_t mantissa = 0;
  // int exp = 0;

  const char *p = c->json;
  if (*p == '-')
    p++;
  // 0
  if (*p == '0')
    p++;
  else {
    if (!ISDIGIT1TO9(*p))
      return LEPT_PARSE_INVALID_VALUE;
    for (p++; ISDIGIT(*p); p++)
      ;
  }
  if (*p == '.') {
    p++;
    if (!ISDIGIT(*p))
      return LEPT_PARSE_INVALID_VALUE;
    for (p++; ISDIGIT(*p); p++)
      ;
  }
  if (*p == 'e' || *p == 'E') {
    p++;
    if (*p == '+' || *p == '-')
      p++;
    if (!ISDIGIT(*p))
      return LEPT_PARSE_INVALID_VALUE;
    for (p++; ISDIGIT(*p); p++)
      ;
  }
  v->u.n = strtod(c->json, NULL);
  if (v->u.n == HUGE_VAL || v->u.n == -HUGE_VAL)
    return LEPT_PARSE_NUMBER_TOO_BIG;
  v->type = LEPT_NUMBER;
  c->json = p;
  return LEPT_PARSE_OK;
}

static const char *lept_parse_hex4(const char *p, unsigned *u) {
  *u = 0;
  for (size_t i = 0; i < 4; i++) {
    char ch = *p++;
    *u <<= 4;
    if ('0' <= ch && ch <= '9')
      *u |= ch - '0';
    else if ('A' <= ch && ch <= 'F')
      *u |= ch - ('A' - 10);
    else if ('a' <= ch && ch <= 'f')
      *u |= ch - ('a' - 10);
    else
      return NULL;
  }
  return p;
}
static void lept_encode_utf8(lept_context *c, unsigned u) {
  assert(0 <= u && u < 0x10ffff);
  if (u <= 0x7f) {
    PUTC(c, u & 0xff);
  } else if (0x80 <= u && u <= 0x7ff) {
    PUTC(c, 0xc0 | ((u >> 6) & 0xff));
    PUTC(c, 0x80 | (u & 0x3f));
  } else if (0x800 <= u && u <= 0xffff) {
    PUTC(c, 0xe0 | ((u >> 12) & 0xf));
    PUTC(c, 0x80 | ((u >> 6) & 0x3f));
    PUTC(c, 0x80 | (u & 0x3f));
  } else if (0x10000 <= u && u <= 0x10ffff) {
    PUTC(c, 0xf0 | ((u >> 18) & 0x7));
    PUTC(c, 0x80 | ((u >> 12) & 0x3f));
    PUTC(c, 0x80 | ((u >> 6) & 0x3f));
    PUTC(c, 0x80 | (u & 0x3f));
  }
}

static int lept_parse_string(lept_context *c, lept_value *v) {
  size_t head = c->top, len;
  unsigned u;
  const char *p;
  EXPECT(c, '\"');
  p = c->json;
  while (1) {
    char ch = *p++;
    switch (ch) {
    case '\"':
      len = c->top - head;
      lept_set_string(v, (const char *)lept_context_pop(c, len), len);
      c->json = p;
      return LEPT_PARSE_OK;
    case '\0':
      // c->top = head;
      // return LEPT_PARSE_MISS_QUOTATION_MARK;
      STRING_ERROR(LEPT_PARSE_MISS_QUOTATION_MARK);
    case '\\':
      switch (*p++) {
      case '\"':
        PUTC(c, '\"');
        break;
      case '\\':
        PUTC(c, '\\');
        break;
      case '/':
        PUTC(c, '/');
        break;
      case 'n':
        PUTC(c, '\n');
        break;
      case 'r':
        PUTC(c, '\r');
        break;
      case 't':
        PUTC(c, '\t');
        break;
      case 'b':
        PUTC(c, '\b');
        break;
      case 'f':
        PUTC(c, '\f');
        break;
      case 'u':
        if (!(p = lept_parse_hex4(p, &u)))
          STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_HEX);
        // surrogate pair handling
        unsigned h, l;
        if (0xd800 <= u && u <= 0xdbff) {
          h = u;
          if (*p++ != '\\')
            STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_SURROGATE);
          if (*p++ != 'u')
            STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_SURROGATE);
          if (!(p = lept_parse_hex4(p, &u)))
            STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_HEX);
          l = u;

          if (0xdc00 <= l && l <= 0xdfff) {
            u = 0x10000 + (h - 0xd800) + (l - 0xdc00);
          } else
            STRING_ERROR(LEPT_PARSE_INVALID_UNICODE_SURROGATE);
        }
        lept_encode_utf8(c, u);
        break;
      default:
        // c->top = head;
        // return LEPT_PARSE_INVALID_STRING_ESCAPE;
        STRING_ERROR(LEPT_PARSE_INVALID_STRING_ESCAPE);
      }
      break;
    default:
      if ((unsigned char)ch < 0x20) {
        // c->top = head;
        // return LEPT_PARSE_INVALID_STRING_CHAR;
        STRING_ERROR(LEPT_PARSE_INVALID_STRING_CHAR);
      }
      PUTC(c, ch);
    }
  }
}

static int lept_parse_value(lept_context *c, lept_value *v) {
  switch (*c->json) {
  case 't':
    return lept_parse_literal(c, v, "true", LEPT_TRUE);
  case 'f':
    return lept_parse_literal(c, v, "false", LEPT_FALSE);
  case 'n':
    return lept_parse_literal(c, v, "null", LEPT_NULL);
  case '\0':
    return LEPT_PARSE_EXPECT_VALUE;
  case '\"':
    return lept_parse_string(c, v);
  case '[':
    return lept_parse_array(c, v);
  default:
    return lept_parse_number(c, v);
  }
}

static int lept_parse_array(lept_context *c, lept_value *v) {
  size_t size = 0;
  int ret;
  EXPECT(c, '[');
  lept_parse_whitespace(c);
  if (*c->json == ']') {
    c->json++;
    v->type = LEPT_ARRAY;
    v->u.a.size = 0;
    v->u.a.e = NULL;
    return LEPT_PARSE_OK;
  }
  for (;;) {
    lept_value e;
    lept_init(&e);
    if ((ret = lept_parse_value(c, &e)) != LEPT_PARSE_OK)
      break;
    memcpy(lept_context_push(c, sizeof(lept_value)), &e, sizeof(lept_value));
    size++;
    lept_parse_whitespace(c);
    if (*c->json == ',') {
      c->json++;
      lept_parse_whitespace(c);
    } else if (*c->json == ']') {
      c->json++;
      v->type = LEPT_ARRAY;
      v->u.a.size = size;
      size *= sizeof(lept_value);
      memcpy(v->u.a.e = (lept_value *)malloc(size), lept_context_pop(c, size),
             size);
      return LEPT_PARSE_OK;
    } else {
      ret = LEPT_PARSE_MISS_COMMA_OR_SQUARE_BRACKET;
      break;
    }
  }
  for (size_t i = 0; i < size; i++) {
    lept_free((lept_value *)lept_context_pop(c, sizeof(lept_value)));
  }
  return ret;
}

int lept_parse(lept_value *v, const char *json) {
  int ret;
  lept_context c;
  assert(v != NULL);
  c.json = json;
  c.stack = NULL;
  c.size = c.top = 0;
  // v->type = LEPT_NULL;
  lept_init(v);
  lept_parse_whitespace(&c);
  if ((ret = lept_parse_value(&c, v)) == LEPT_PARSE_OK) {
    lept_parse_whitespace(&c);
    if (*c.json != '\0') {
      v->type = LEPT_NULL;
      ret = LEPT_PARSE_ROOT_NOT_SINGULAR;
    }
  }
  assert(c.top == 0);
  free(c.stack);
  // free(v->u.a.e);
  return ret;
}

lept_type lept_get_type(const lept_value *v) {
  assert(v != NULL);
  return v->type;
}

double lept_get_number(const lept_value *v) {
  assert(v != NULL && v->type == LEPT_NUMBER);
  return v->u.n;
}

void lept_set_number(lept_value *v, double n) {
  lept_free(v);
  v->u.n = n;
  v->type = LEPT_NUMBER;
}

void lept_set_string(lept_value *v, const char *s, size_t len) {
  assert(v != NULL && (s != NULL || len == 0));
  lept_free(v);
  v->u.s.s = (char *)malloc(len + 1);
  memcpy(v->u.s.s, s, len);
  v->u.s.s[len] = '\0';
  v->u.s.len = len;
  v->type = LEPT_STRING;
}

void lept_free(lept_value *v) {
  assert(v != NULL);
  switch (v->type) {
  case LEPT_STRING:
    free(v->u.s.s);
    break;
  case LEPT_ARRAY:
    for (size_t i = 0; i < v->u.a.size; i++) {
      lept_free(&v->u.a.e[i]);
    }
    free(v->u.a.e);
    break;
  default:
    break;
  }
  v->type = LEPT_NULL;
}

size_t lept_get_string_length(const lept_value *v) {
  assert(v != NULL && v->type == LEPT_STRING);
  return v->u.s.len;
}

const char *lept_get_string(const lept_value *v) {
  assert(v != NULL && v->type == LEPT_STRING);
  return v->u.s.s;
}

int lept_get_boolean(const lept_value *v) {
  assert(v != NULL && (v->type == LEPT_TRUE || v->type == LEPT_FALSE));
  return v->type == LEPT_TRUE;
}

void lept_set_boolean(lept_value *v, int b) {
  lept_free(v);
  v->type = b ? LEPT_TRUE : LEPT_FALSE;
}

size_t lept_get_array_size(const lept_value *v) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  return v->u.a.size;
}

lept_value *lept_get_array_element(const lept_value *v, size_t index) {
  assert(v != NULL && v->type == LEPT_ARRAY);
  assert(index < v->u.a.size);
  return &v->u.a.e[index];
}
