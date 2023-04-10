#include "raylib.h"
#include <stdio.h>

void Log(const char* fmt, ...);

static void init(void) {
  SetWindowPosition(10, 50);
}

static void update(void) {
  ClearBackground((Color){0x64, 0x95, 0xed, 0xff});

  DrawText("Welcome to Rdy!", 290, 200, 40, WHITE);
  Vector2 scale = GetWindowScaleDPI();
  char buf[256];
  sprintf(buf, "DPI Scale: %f %f", scale.x, scale.y);
  DrawText(buf, 190, 290, 40, WHITE);
}

// 0 = init
// 1 = update
void RdyEntryPoint(int state) {
  if (state == 0) {
    init();
  }
  update();
}
