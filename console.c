#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "raylib.h"
#include "raymath.h"

static bool is_visible = false;
static bool override_visible = false;

static Rectangle location;

typedef struct CChar {
  char c;
  char colour;
} CChar;

static Color colours_by_index[] = {
    {12, 12, 12, 255},     // Black
    {197, 15, 31, 255},    // Red
    {19, 161, 14, 255},    // Green
    {193, 156, 0, 255},    // Yellow
    {0, 55, 218, 255},     // Blue
    {136, 23, 152, 255},   // Magenta
    {58, 150, 221, 255},   // Cyan
    {204, 204, 204, 255},  // White

    {118, 118, 118, 255},  // Bright Gray
    {231, 72, 86, 255},    // Bright Red
    {22, 198, 12, 255},    // Bright Green
    {249, 241, 165, 255},  // Bright Yellow
    {59, 120, 255, 255},   // Bright Blue
    {180, 0, 158, 255},    // Bright Magenta
    {97, 214, 214, 255},   // Bright Cyan
    {242, 242, 242, 255},  // Bright White
};

#define CONSOLE_WIDTH 120
#define CONSOLE_HEIGHT 35

static CChar* characters;
static CChar* last_insertion_location;

static int scroll_offset;
static char current_colour;

static Font debug_font;

#define CONSOLE_FONT_SIZE 30
#define BORDER 10.f
#define FIRST_GLYPH ' '
#define LAST_GLYPH '~'

void console_preinit(void) {
  characters = calloc(CONSOLE_WIDTH * CONSOLE_HEIGHT, sizeof(CChar));
}

void console_init(void) {
  debug_font = LoadFontEx("hack-font\\Hack-Regular.ttf", CONSOLE_FONT_SIZE, NULL, 0);
  location.x = location.y = BORDER;
  location.width = (float)GetScreenWidth() - BORDER * 2;
  location.height = (float)GetScreenHeight() - BORDER * 2;
  current_colour = 7;
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
    location.y =
        Clamp(Lerp(location.y, target_height, 0.5f), (float)-GetScreenHeight(), target_height);
  } else {
    location.y = Lerp(location.y, (float)-GetScreenHeight(), 0.5f);
  }

  DrawRectangleRounded(location, .02f, 6, Fade(BLACK, 0.9f));

  float scale_factor = (float)CONSOLE_FONT_SIZE / debug_font.baseSize;

  Vector2 pos = {BORDER, BORDER + location.y};
  for (int y = 0; y < CONSOLE_HEIGHT; ++y) {
    pos.x = BORDER;
    for (int x = 0; x < CONSOLE_WIDTH; ++x) {
      CChar* ch = &characters[y * CONSOLE_WIDTH + x];
      if (ch->c < FIRST_GLYPH || ch->c > LAST_GLYPH) {
        continue;
      }
      DrawTextCodepoint(debug_font, ch->c, pos, CONSOLE_FONT_SIZE, colours_by_index[ch->colour]);
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

void console_out(const char* text) {
  const char* p = text;
  CChar* start_of_last_line = &characters[CONSOLE_WIDTH * (CONSOLE_HEIGHT - 1)];
  CChar* past_end_of_last_line = &characters[CONSOLE_WIDTH * CONSOLE_HEIGHT];
  CChar* q = last_insertion_location ? last_insertion_location : start_of_last_line;
  while (*p) {
    if (*p == '\033') {
      ++p;
      if (p[0] == '[' && p[1] == '0' && p[2] == 'm') {
        p += 3;
        current_colour = 7;
      } else if (p[0] == '[' && p[1] == '1' && p[2] == ';' && p[3] == '3' && p[4] != 0 &&
                 p[5] == 'm') {
        current_colour = 8 + (p[4] - '0');
        p += 6;
      }
      continue;
    }
    if (q >= past_end_of_last_line || *p == '\n') {
      memmove(&characters[0], &characters[CONSOLE_WIDTH],
              sizeof(CChar) * CONSOLE_WIDTH * (CONSOLE_HEIGHT - 1));
      q = start_of_last_line;
      memset(q, 0, sizeof(CChar) * CONSOLE_WIDTH);
      if (*p == '\n')
        ++p;
      continue;
    } else if (!isprint(*p)) {
      ++p;
      continue;
    }
    q->c = *p;
    q->colour = current_colour;
    ++p;
    ++q;
  }

  last_insertion_location = q;
}

int console_vprintf(const char* fmt, va_list ap) {
  char stack_buf[256];
  int num_chars = vsnprintf(stack_buf, sizeof(stack_buf), fmt, ap);
  if (num_chars >= sizeof(stack_buf)) {
    return 0;
  }

  console_out(stack_buf);

  return num_chars;
}

void console_set_override_visible(bool visible) {
  override_visible = visible;
}

void console_shutdown(void) {
  free(characters);
  UnloadFont(debug_font);
}
