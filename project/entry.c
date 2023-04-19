#ifndef __dyibicc__
#define 🔎(...)
#endif
#include <math.h>
#include <stdio.h>
#include "raylib.h"

void Log(const char* fmt, ...);

static void init(void) {
  SetWindowPosition(10, 50);
}

#define NUM_BALLS 1000

typedef struct SnowBall {
  double x;
  double y;
  double yspeed;
} SnowBall;

SnowBall snowballs[NUM_BALLS];

double counter;

static void bounce_a_thing(void) {
  int x = 400 * cos(counter);
  int y = 400 * sin(counter * 2);
  counter += 0.05;
  DrawCircle(x + 1000, y + 500, 50.f, RED);
  // DrawText(TextFormat("%f", counter), 100, 200, 40, WHITE);
}

static void init_snowball(SnowBall* b) {
  b->x = GetRandomValue(0, GetRenderWidth() - 1);
  b->y = 0;
  b->yspeed = GetRandomValue(1, 100) / 10.0;
}

static void init_snowballs(void) {
  for (int i = 0; i < NUM_BALLS; ++i) {
    init_snowball(&snowballs[i]);
  }
}

static void draw_snowballs(void) {
  for (int i = 0; i < NUM_BALLS; ++i) {
    SnowBall* b = &snowballs[i];
    b->y += b->yspeed;
    DrawCircle(b->x, b->y, 5.f, WHITE);
    if (b->y > GetScreenWidth()) {
      init_snowball(b);
    }
  }
  DrawText(TextFormat("%d %d", GetRenderWidth(), GetRenderHeight()), 100, 300,
           40, WHITE);
}

static void update(void) {
  ClearBackground((Color){0x64, 0x95, 0xed, 0xff});

  //init_snowballs();
  draw_snowballs();

  bounce_a_thing();

  DrawText("Welcome to Rdy!", 100, 100, 40, ORANGE);
}

// 0 = init
// 1 = update
void RdyEntryPoint(int state) {
  if (state == 0) {
    init();
  }
  update();
}
