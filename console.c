#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "raylib.h"
#include "raymath.h"

static bool is_visible = false;
static bool override_visible = false;

static Rectangle location;
typedef struct CChar {
  char c;
  char flags;
} CChar;

#define CONSOLE_WIDTH 120
#define CONSOLE_HEIGHT 35

static CChar* characters;
static CChar* last_insertion_location;

static int scroll_offset;

static Font debug_font;

#define CONSOLE_FONT_SIZE 30
#define BORDER 10.f
#define FIRST_GLYPH ' '
#define LAST_GLYPH '~'

void console_init(void) {
  debug_font = LoadFontEx("hack-font\\Hack-Regular.ttf", CONSOLE_FONT_SIZE, NULL, 0);
  location.x = location.y = BORDER;
  location.width = (float)GetScreenWidth() - BORDER * 2;
  location.height = (float)GetScreenHeight() - BORDER * 2;
  characters = calloc(CONSOLE_WIDTH * CONSOLE_HEIGHT, sizeof(CChar));
}

void console_clear(void) {
  // TODO probably need a separate stream for compile errors vs. regular logs
  memset(characters, 0, CONSOLE_HEIGHT * CONSOLE_WIDTH * sizeof(CChar));
  last_insertion_location = NULL;
}

void console_update(void) {
  if (IsKeyPressed(KEY_GRAVE))
    is_visible = !is_visible;

  if (is_visible || override_visible) {
    float target_height = BORDER;
    location.y = Clamp(Lerp(location.y, target_height, 0.5f), (float)-GetScreenHeight(), target_height);
  } else {
    location.y = Lerp(location.y, (float)-GetScreenHeight(), 0.5f);
  }

  DrawRectangleRec(location, Fade(BLACK, 0.85f));

  float scale_factor = (float)CONSOLE_FONT_SIZE / debug_font.baseSize;

  Vector2 pos = {BORDER, BORDER + location.y};
  for (int y = 0; y < CONSOLE_HEIGHT; ++y) {
    pos.x = BORDER;
    for (int x = 0; x < CONSOLE_WIDTH; ++x) {
      CChar* ch = &characters[y * CONSOLE_WIDTH + x];
      if (ch->c < FIRST_GLYPH || ch->c > LAST_GLYPH) {
        continue;
      }
      // XXX color
      DrawTextCodepoint(debug_font, ch->c, pos, CONSOLE_FONT_SIZE, WHITE);
      int index = GetGlyphIndex(debug_font, ch->c);
      if (debug_font.glyphs[index].advanceX == 0) {
        pos.x += ((float)debug_font.recs[index].width * scale_factor);
      } else {
        pos.x += ((float)debug_font.glyphs[index].advanceX * scale_factor);
      }
    }
    pos.y += debug_font.baseSize * scale_factor;
  }
}

int console_vprintf(int level, const char* fmt, va_list ap) {
  char flags = 0;
  if (level >= 2) {
    flags = 1;
  }

  char stack_buf[256];
  int num_chars = vsnprintf(stack_buf, sizeof(stack_buf), fmt, ap);
  if (num_chars >= sizeof(stack_buf)) {
    return 0;
  }

  char* p = stack_buf;
  CChar* start_of_last_line = &characters[CONSOLE_WIDTH * (CONSOLE_HEIGHT - 1)];
  CChar* q = last_insertion_location ? last_insertion_location : start_of_last_line;
  while (*p) {
    if (*p == '\n') {
      memmove(&characters[0], &characters[CONSOLE_WIDTH],
              sizeof(CChar) * CONSOLE_WIDTH * (CONSOLE_HEIGHT - 1));
      q = start_of_last_line;
      memset(q, 0, sizeof(CChar) * CONSOLE_WIDTH);
      q[0].c = '>';
      ++p;
      continue;
    } else if (!isprint(*p)) {
      ++p;
      continue;
    }
    q->c = *p;
    q->flags = flags;
    ++p;
    ++q;
  }

  last_insertion_location = q;

  return num_chars;
}

int console_errf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ret = console_vprintf(2, fmt, ap);
  va_end(ap);
  return ret;
}

int console_logf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ret = console_vprintf(1, fmt, ap);
  va_end(ap);
  return ret;
}

void console_set_override_visible(bool visible) {
  override_visible = visible;
}

void console_shutdown(void) {
  // XXX
}
