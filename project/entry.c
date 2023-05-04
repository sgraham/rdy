#ifdef __dyibicc__
#include <reflect.h>

#define QQ(expr) qq_eval(__FILE__, __LINE__, _ReflectTypeOf(expr), expr)

void qq_eval(const char* file, int line, int num_args, ...);
#else
#define QQ(expr) ((void)expr)
#endif

#include <math.h>
#include <stdio.h>
#include "raylib.h"
#include "raymath.h"

#define SCREEN_WIDTH 1920
#define SCREEN_HEIGHT 1080
#define GRID 18

void Log(const char* fmt, ...);

void do_player_movement(Camera2D* cam);

static bool is_fullscreen;

Camera2D cam = {0};  //(Vector2){0,0}, (Vector2){0,0}, 0.f, 1.f };
Texture2D texall;
bool edit_mode;

#define MAX_LEVEL_SIZE 4096
unsigned char raw_level_data[MAX_LEVEL_SIZE * MAX_LEVEL_SIZE];

static unsigned char get_level_at(int x, int y) {
  return raw_level_data[y * MAX_LEVEL_SIZE + x];
}

bool level_is_set(int x, int y, int layer) {
  if (y < 0 || y >= MAX_LEVEL_SIZE)
    return false;
  if (x < 0 || x >= MAX_LEVEL_SIZE)
    return false;
  return raw_level_data[y * MAX_LEVEL_SIZE + x] & (1 << layer);
}

static void set_level_at(int x, int y, int layer) {
  raw_level_data[y * MAX_LEVEL_SIZE + x] |= (unsigned char)(1 << layer);
}

static void clear_level_at(int x, int y, int layer) {
  raw_level_data[y * MAX_LEVEL_SIZE + x] &= (unsigned char)(~(1 << layer));
}

static void init(void) {
  SetWindowPosition(10, 50);
  cam.zoom = 4.f;
  texall = LoadTexture("project/assets/all.dds");

  FILE* f = fopen("level0.dat", "rb");
  if (f) {
    fread(raw_level_data, sizeof(raw_level_data), 1, f);
    fclose(f);
  }
  for (int i = 0; i < MAX_LEVEL_SIZE; ++i) {
    set_level_at(i, 0, 0);
    set_level_at(i, MAX_LEVEL_SIZE - 1, 0);
    set_level_at(0, i, 0);
    set_level_at(MAX_LEVEL_SIZE - 1, i, 0);
  }
}

static void toggle_fullscreen(void) {
  if (!is_fullscreen) {
    SetWindowState(FLAG_WINDOW_RESIZABLE | FLAG_WINDOW_UNDECORATED |
                   FLAG_WINDOW_TOPMOST);
    SetWindowPosition(0, 0);
    SetWindowSize(GetMonitorWidth(0), GetMonitorHeight(0));
  } else {
    ClearWindowState(FLAG_WINDOW_RESIZABLE | FLAG_WINDOW_UNDECORATED |
                     FLAG_WINDOW_TOPMOST);
    SetWindowSize(1920, 1080);
    SetWindowPosition(10, 50);
  }
  is_fullscreen = !is_fullscreen;
}

typedef struct Rule {
  char pattern[10];
  int tx;
  int ty;
  bool fixed;
} Rule;

static const Rule rules[] = {
    {
        // island
        .pattern = ".x."
                   "x0x"
                   ".x.",
        .tx = 0,
        .ty = 0,
    },
    {
        // bridge right
        .pattern = ".x."
                   ".0x"
                   ".x.",
        .tx = 3,
        .ty = 0,
    },
    {
        // bridge left
        .pattern = ".x."
                   "x0."
                   ".x.",
        .tx = 1,
        .ty = 0,
    },
#if 0 // too many extra tiles needed for these for thin parts
    {
      // inside corner bottom left
      .pattern = "00."
                 "00."
                 "x0.",
      .tx = 5,
      .ty = 0,
      .fixed = true,
    },
    {
      // inside corner bottom right
      .pattern = ".00"
                 ".00"
                 ".0x",
      .tx = 4,
      .ty = 0,
      .fixed = true,
    },
#endif
    {
        // bridge centre
        .pattern = ".x."
                   ".0."
                   ".x.",
        .tx = 2,
        .ty = 0,
    },
    {
        // pillar top
        .pattern = ".x."
                   "x0x"
                   "...",
        .tx = 0,
        .ty = 1,
    },
    {
        // pillar bottom
        .pattern = "..."
                   "x0x"
                   ".x.",
        .tx = 0,
        .ty = 3,
    },
    {
        // pillar middle
        .pattern = "..."
                   "x0x"
                   "...",
        .tx = 0,
        .ty = 2,
    },
    {
        // top left
        .pattern = ".x."
                   "x0."
                   "...",
        .tx = 1,
        .ty = 1,
    },
    {
        // top right
        .pattern = ".x."
                   ".0x"
                   "...",
        .tx = 3,
        .ty = 1,
    },
    {
        // bottom left
        .pattern = "..."
                   "x0."
                   ".x.",
        .tx = 1,
        .ty = 3,
    },
    {
        // bottom right
        .pattern = "..."
                   ".0x"
                   ".x.",
        .tx = 3,
        .ty = 3,
    },
    {
        // top
        .pattern = ".x."
                   ".0."
                   "...",
        .tx = 2,
        .ty = 1,
    },
    {
        // bottom
        .pattern = "..."
                   ".0."
                   ".x.",
        .tx = 2,
        .ty = 3,
    },
    {
        // wall left
        .pattern = "..."
                   "x0."
                   "...",
        .tx = 1,
        .ty = 2,
    },
    {
        // wall right
        .pattern = "..."
                   ".0x"
                   "...",
        .tx = 3,
        .ty = 2,
    },
    {
        // fill
        .pattern = "..."
                   ".0."
                   "...",
        .tx = 2,
        .ty = 2,
    },
};

int rule_tile_x = 20;
int rule_tile_y = 0;

static bool find_tile_by_rule(int x, int y, Rectangle* rect) {
  if (!level_is_set(x, y, 0))
    return false;  // Not strictly correct, but true for
                   // all our rules right now.

  for (size_t i = 0; i < sizeof(rules) / sizeof(rules[0]); ++i) {
    Rule* rule = &rules[i];
    // todo; multiple layers (and early out above)
    if (rule->pattern[4] != '0')
      Log("%s", "EXPECTING MIDDLE 0");

    // skip if looking at left column while at left edge of map
    if ((rule->pattern[0] != '.' || rule->pattern[3] != '.' ||
         rule->pattern[6] != '.') &&
        x == 0) {
      continue;
    }

    // skip if looking at right column while at right edge of map
    if ((rule->pattern[2] != '.' || rule->pattern[5] != '.' ||
         rule->pattern[8] != '.') &&
        x == MAX_LEVEL_SIZE - 1) {
      continue;
    }

    // skip if looking at top column while at top edge of map
    if ((rule->pattern[0] != '.' || rule->pattern[1] != '.' ||
         rule->pattern[2] != '.') &&
        y == 0) {
      continue;
    }

    // skip if looking at bottom column while at bottom edge of map
    if ((rule->pattern[6] != '.' || rule->pattern[7] != '.' ||
         rule->pattern[8] != '.') &&
        y == MAX_LEVEL_SIZE - 1) {
      continue;
    }

    if (rule->pattern[0] == 'x' && level_is_set(x - 1, y - 1, 0)) continue;
    if (rule->pattern[0] == '0' && !level_is_set(x - 1, y - 1, 0)) continue;
    if (rule->pattern[1] == 'x' && level_is_set(x, y - 1, 0)) continue;
    if (rule->pattern[1] == '0' && !level_is_set(x, y - 1, 0)) continue;
    if (rule->pattern[2] == 'x' && level_is_set(x + 1, y - 1, 0)) continue;
    if (rule->pattern[2] == '0' && !level_is_set(x + 1, y - 1, 0)) continue;
    if (rule->pattern[3] == 'x' && level_is_set(x - 1, y, 0)) continue;
    if (rule->pattern[3] == '0' && !level_is_set(x - 1, y, 0)) continue;
    if (rule->pattern[5] == 'x' && level_is_set(x + 1, y, 0)) continue;
    if (rule->pattern[5] == '0' && !level_is_set(x + 1, y, 0)) continue;
    if (rule->pattern[6] == 'x' && level_is_set(x - 1, y + 1, 0)) continue;
    if (rule->pattern[6] == '0' && !level_is_set(x - 1, y + 1, 0)) continue;
    if (rule->pattern[7] == 'x' && level_is_set(x, y + 1, 0)) continue;
    if (rule->pattern[7] == '0' && !level_is_set(x, y + 1, 0)) continue;
    if (rule->pattern[8] == 'x' && level_is_set(x + 1, y + 1, 0)) continue;
    if (rule->pattern[8] == '0' && !level_is_set(x + 1, y + 1, 0)) continue;

    if (rule->fixed) {
      rect->x = rule->tx * GRID;
      rect->y = rule->ty * GRID;
    } else {
      rect->x = (rule->tx + rule_tile_x) * GRID;
      rect->y = (rule->ty + rule_tile_y) * GRID;
    }
    return true;
  }
  return false;
}

static void draw_world(void) {
  Rectangle texcoords = {0, 0, GRID, GRID};
  for (int y = 0; y < SCREEN_HEIGHT; y += GRID) {
    for (int x = 0; x < SCREEN_WIDTH; x += GRID) {
      if (find_tile_by_rule(x / GRID, y / GRID, &texcoords)) {
        DrawTextureRec(texall, texcoords, (Vector2){x, y}, WHITE);
      }
    }
  }
}

static void draw_world_raw(void) {
  for (int y = 0; y < SCREEN_HEIGHT; y += GRID) {
    for (int x = 0; x < SCREEN_WIDTH; x += GRID) {
      if (get_level_at(x / GRID, y / GRID) & 1) {
        DrawRectangle(x, y, GRID, GRID, WHITE);
      }
    }
  }
}

static void save_level(char* fn) {
  FILE* f = fopen("level0.dat", "wb");
  fwrite(raw_level_data, sizeof(raw_level_data), 1, f);
  fclose(f);
  DrawText("SAVED!", 10, 10, 40, WHITE);
}

static void update(void) {
  if (IsKeyPressed(KEY_F11)) {
    toggle_fullscreen();
  }

  ClearBackground((Color){0x64, 0x95, 0xed, 0xff});

  BeginMode2D(cam);

  // QQ(cam.zoom);

  double mx = (double)GetMouseX() * SCREEN_WIDTH / GetRenderWidth();
  double my = (double)GetMouseY() * SCREEN_HEIGHT / GetRenderHeight();
  Vector2 world = GetScreenToWorld2D((Vector2){mx, my}, cam);

  int x_tile = (int)(world.x / GRID);
  int y_tile = (int)(world.y / GRID);
  // QQ(x_tile);
  // QQ(y_tile);

  if (IsKeyPressed(KEY_SPACE)) {
    edit_mode = !edit_mode;
  }

  if (IsKeyPressed('D')) {
    rule_tile_x = 20;
    rule_tile_y = 0;
  } else if (IsKeyPressed('S')) {
    rule_tile_x = 0;
    rule_tile_y = 4;
  } else if (IsKeyPressed('M')) {
    rule_tile_x = 20;
    rule_tile_y = 7;
  } else if (IsKeyPressed('G')) {
    rule_tile_x = 0;
    rule_tile_y = 0;
  }

  if (IsKeyDown(KEY_LEFT_SHIFT)) {
    draw_world_raw();
  } else {
    draw_world();
  }

  if (IsKeyDown(KEY_LEFT)) {
    cam.offset.x += GRID;
  }
  if (IsKeyDown(KEY_RIGHT)) {
    cam.offset.x -= GRID;
  }
  if (IsKeyDown(KEY_UP)) {
    cam.offset.y += GRID;
  }
  if (IsKeyDown(KEY_DOWN)) {
    cam.offset.y -= GRID;
  }
  if (cam.offset.x > 0)
    cam.offset.x = 0;
  if (cam.offset.y > 0)
    cam.offset.y = 0;

  if (IsKeyDown(KEY_F1)) {
    DrawTexture(texall, 0, 0, WHITE);
  }

  if (edit_mode) {
    cam.zoom += GetMouseWheelMove();
    cam.zoom = Clamp(cam.zoom, 1, 10);

    static int current_layer = 0;

    if (IsMouseButtonDown(MOUSE_BUTTON_LEFT)) {
      set_level_at(x_tile, y_tile, current_layer);
    } else if (IsMouseButtonDown(MOUSE_BUTTON_RIGHT)) {
      clear_level_at(x_tile, y_tile, current_layer);
    }

    for (int x = 0; x < SCREEN_WIDTH; x += GRID) {
      DrawLine(x, 0, x, SCREEN_HEIGHT, GREEN);
    }
    for (int y = 0; y < SCREEN_HEIGHT; y += GRID) {
      DrawLine(0, y, SCREEN_WIDTH, y, GREEN);
    }

    DrawRectangle(x_tile * GRID, y_tile * GRID, GRID, GRID, Fade(GREEN, .25f));
    DrawText(TextFormat("%d,%d", x_tile, y_tile), x_tile * GRID + GRID,
             y_tile * GRID + GRID, 12, GREEN);
  } else {
    cam.zoom = 4.f;
  }

  if (IsKeyPressed(KEY_ENTER)) {
    save_level("level0.dat");
  }

  do_player_movement(&cam);

  EndMode2D();

  DrawFPS(4, 1060);
}

// 0 = init
// 1 = update
void RdyEntryPoint(int state) {
  if (state == 0) {
    init();
  }
  update();
}
