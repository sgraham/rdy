#include <stdio.h>
#include "raylib.h"

void Log(const char* fmt, ...);

static void init(void) {
  SetWindowPosition(10, 50);
}

static void update(void) {
  ClearBackground((Color){0x64, 0x95, 0xed, 0xff});

  DrawText("Welcome to Rdy!", 100, 100, 40, WHITE);
}

// 0 = init
// 1 = update
void RdyEntryPoint(int state) {
  if (state == 0) {
    init();
  }
  update();
}
