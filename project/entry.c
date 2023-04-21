#define QQ(expr) qq_eval(__FILE__, __LINE__, _ReflectTypeOf(expr), expr)
void qq_eval(const char* file, int line, int num_args, ...);

#include <math.h>
#include <stdio.h>
#include "raylib.h"

#define GRID 16

static void init(void) {
  SetWindowPosition(10, 50);
}

void Log(const char* fmt, ...);

static void update(void) {
  ClearBackground((Color){0x64, 0x95, 0xed, 0xff});

  Camera2D cam = { (Vector2){0,0}, (Vector2){0,0}, 0.f, 1.f };
  //BeginMode2D(cam);

#if 0
  for (int x = 0; x < GetRenderWidth(); x += GRID) {
    DrawLine(x, 0, x, GetRenderHeight(), GRAY);
  }
  for (int y = 0; y < GetRenderHeight(); y += GRID) {
    DrawLine(0, y, GetRenderWidth(), y, GRAY);
  }
#endif

  EndMode2D();
}

// 0 = init
// 1 = update
void RdyEntryPoint(int state) {
  if (state == 0) {
    init();
  }
  update();
}
