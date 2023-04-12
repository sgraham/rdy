#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "raylib.h"
#include "raymath.h"

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

typedef struct CChar {
  char c;
  char colour;
} CChar;

typedef struct Console {
  Rectangle location;
  CChar* characters;
  int buffer_width;
  int buffer_height;
  CChar* last_insertion_location;
  char current_colour;
  bool is_visible;
} Console;

static Console main;
static Console error;

static Font debug_font;
static float font_scale_factor;

#define CONSOLE_FONT_SIZE 30
#define BORDER 10.f
#define FIRST_GLYPH ' '
#define LAST_GLYPH '~'

void console_preinit(void) {
  main.buffer_width = 120;
  main.buffer_height = 35;
  main.characters = calloc(main.buffer_width * main.buffer_height, sizeof(CChar));

  error.buffer_width = 100;
  error.buffer_height = 8;
  error.characters = calloc(error.buffer_width * error.buffer_height, sizeof(CChar));
}

void console_init(void) {
  debug_font = LoadFontEx("hack-font\\Hack-Regular.ttf", CONSOLE_FONT_SIZE, NULL, 0);
  font_scale_factor = (float)CONSOLE_FONT_SIZE / debug_font.baseSize;

  main.location.x = main.location.y = BORDER;
  main.location.width = (float)GetScreenWidth() - BORDER * 2;
  main.location.height = (float)GetScreenHeight() - BORDER * 2;
  main.current_colour = 7;

  error.location.x = BORDER * 2;
  error.location.y = (float)GetScreenHeight();
  error.location.width = GetScreenWidth() - BORDER * 4;
  error.location.height = CONSOLE_FONT_SIZE * font_scale_factor * error.buffer_height + BORDER * 2;
}

void console_main_clear(void) {
  memset(main.characters, 0, main.buffer_width * main.buffer_height * sizeof(CChar));
  main.last_insertion_location = NULL;
}

void console_main_set_visible(bool visible) {
  main.is_visible = visible;
}

void console_error_clear(void) {
  memset(error.characters, 0, error.buffer_width * error.buffer_height * sizeof(CChar));
  error.last_insertion_location = NULL;
}

void console_error_set_visible(bool visible) {
  error.is_visible = visible;
}

static void render_console_text(Console* con, float startx, float starty) {
  Vector2 pos = {startx, starty};
  for (int y = 0; y < con->buffer_height; ++y) {
    pos.x = startx;
    for (int x = 0; x < con->buffer_width; ++x) {
      CChar* ch = &con->characters[y * con->buffer_width + x];
      if (ch->c < FIRST_GLYPH || ch->c > LAST_GLYPH) {
        continue;
      }
      DrawTextCodepoint(debug_font, ch->c, pos, CONSOLE_FONT_SIZE, colours_by_index[ch->colour]);
      int index = GetGlyphIndex(debug_font, ch->c);
      if (debug_font.glyphs[index].advanceX == 0) {
        pos.x += ((float)debug_font.recs[index].width * font_scale_factor);
      } else {
        pos.x += ((float)debug_font.glyphs[index].advanceX * font_scale_factor);
      }
    }
    pos.y += debug_font.baseSize * font_scale_factor;
  }
}

void console_update(void) {
  if (IsKeyPressed(KEY_GRAVE))
    main.is_visible = !main.is_visible;

#define EASE_SPEED 0.3f
  float error_hidden_y_location = (float)GetScreenHeight() + BORDER;
  if (error.is_visible) {
    float target_y = (float)GetScreenHeight() - error.location.height - BORDER * 2;
    error.location.y = Lerp(error.location.y, target_y, EASE_SPEED);
  } else {
    error.location.y = Lerp(error.location.y, error_hidden_y_location, EASE_SPEED);
  }

  float main_hidden_y_location = (float)-GetScreenHeight();
  if (main.is_visible) {
    float target_y = BORDER;
    if (error.is_visible) {
      target_y -= error.location.height + BORDER * 3;
    }
    main.location.y = Lerp(main.location.y, target_y, EASE_SPEED);
  } else {
    main.location.y = Lerp(main.location.y, main_hidden_y_location, EASE_SPEED);
  }


  if (!FloatEquals(main.location.y, main_hidden_y_location)) {
    DrawRectangleRounded(main.location, .02f, 6, Fade(BLACK, 0.9f));

    render_console_text(&main, main.location.x + BORDER, main.location.y + BORDER);
  }

  if (!FloatEquals(error.location.y, error_hidden_y_location)) {
    DrawRectangleRounded(error.location, .1f, 6, Fade(BLACK, 0.9f));
    DrawRectangleRoundedLines(error.location, .1f, 6, 5, RED);
    render_console_text(&error, error.location.x + BORDER, error.location.y + BORDER);
  }
}

static void console_out(Console* con, const char* text) {
  const char* p = text;
  CChar* start_of_last_line = &con->characters[con->buffer_width * (con->buffer_height - 1)];
  CChar* past_end_of_last_line = &con->characters[con->buffer_width * con->buffer_height];
  CChar* q = con->last_insertion_location ? con->last_insertion_location : start_of_last_line;
  while (*p) {
    if (*p == '\033') {
      ++p;
      if (p[0] == '[' && p[1] == '0' && p[2] == 'm') {
        p += 3;
        con->current_colour = 7;
      } else if (p[0] == '[' && p[1] == '1' && p[2] == ';' && p[3] == '3' && p[4] != 0 &&
                 p[5] == 'm') {
        con->current_colour = 8 + (p[4] - '0');
        p += 6;
      }
      continue;
    }
    if (q >= past_end_of_last_line || *p == '\n') {
      memmove(&con->characters[0], &con->characters[con->buffer_width],
              sizeof(CChar) * con->buffer_width * (con->buffer_height - 1));
      q = start_of_last_line;
      memset(q, 0, sizeof(CChar) * con->buffer_width);
      if (*p == '\n')
        ++p;
      continue;
    } else if (!isprint(*p)) {
      ++p;
      continue;
    }
    q->c = *p;
    q->colour = con->current_colour;
    ++p;
    ++q;
  }

  con->last_insertion_location = q;
}

void console_main_out(const char* text) {
  console_out(&main, text);
}

void console_error_out(const char* text) {
  console_out(&error, text);
}

int console_main_vprintf(const char* fmt, va_list ap) {
  char stack_buf[256];
  int num_chars = vsnprintf(stack_buf, sizeof(stack_buf), fmt, ap);
  if (num_chars >= sizeof(stack_buf)) {
    return 0;
  }

  console_main_out(stack_buf);

  return num_chars;
}

void console_main_printf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  console_main_vprintf(fmt, ap);
  va_end(ap);
}

int console_error_vprintf(const char* fmt, va_list ap) {
  char stack_buf[256];
  int num_chars = vsnprintf(stack_buf, sizeof(stack_buf), fmt, ap);
  if (num_chars >= sizeof(stack_buf)) {
    return 0;
  }

  console_error_out(stack_buf);

  return num_chars;
}

void console_error_printf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  console_error_vprintf(fmt, ap);
  va_end(ap);
}

void console_shutdown(void) {
  free(main.characters);
  UnloadFont(debug_font);
}
